module DoubleBack exposing (..)

import Browser
import Dict
import Html as H exposing (Html, div, ol, li, button, text)
import Html.Attributes as HA
import Html.Events as HE
import History exposing 
    ( History
    , undo
    , redo
    , add_step
    , can_undo
    , can_redo
    , seek
    )
import Json.Decode as D
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Svg.Keyed as SK
import TouchEvents exposing 
    ( TouchEvent(..)
    , TouchInfo(..)
    , PressAction(..)
    , PieceMethods
    , touch_events
    , update
    , press_fold
    , is_dragging
    )
import Tuple exposing (first, second, pair)
import Util exposing (fi, ff, tf, strf)
import Vector as V exposing (Vector)

type alias Press = TouchEvents.Press Piece

type alias Piece =
    { value : Int
    , position : Vector
    , coords : (Int,Int)
    , key : Int
    }

type alias Row = List Piece

type alias Board = List Row

type alias Model =
    { num_pieces : Int
    , board : Board
    , history : History Board
    , cursor_pos : (Int,Int)
    , mouse : Vector
    , presses : List Press
    , last_key_move : Maybe ((Int,Int), (Int,Int))
    , keys_active : Bool
    , screen : Screen
    }

type Screen
    = Intro
    | Game

type Msg
    = Undo
    | Redo
    | SetScreen Screen
    | Fewer
    | More
    | SvgMsg TouchEvent
    | SeekHistory String
    | SeekToStart
    | SeekToEnd
    | KeyPress Msg
    | SwapLeft
    | SwapRight
    | SwapVertical
    | CursorLeft
    | CursorRight
    | CursorUp
    | CursorDown
    | FocusBoard

scale : Float
scale = 20

r : Float
r = scale*0.3

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

distance_from_piece : Vector -> Piece -> Float
distance_from_piece pos piece = 
    let
        d : Float
        d = V.len (V.sub pos piece.position)
    in
        max 0 (d - r)

cell_position : Int -> Int -> (Float, Float)
cell_position x y = (scale*(tf x), scale*(tf y))

line_up_pieces : Board -> Board
line_up_pieces = List.indexedMap (\iy -> List.indexedMap (\ix -> \p -> { p | position = cell_position ix iy, coords = (ix,iy) }))

raway : Float -> Int
raway x = if x<0 then -(ceiling (-x)) else ceiling x

piece_moving_to : Press -> Maybe (Int,Int)
piece_moving_to press = 
    case press.action of
        DragPiece (i,dp) ->
            let
                ((dx, dy) as diff) = V.sub press.position press.start_position
                an = atan2 dy dx
                direction = (modBy 8 (round (8*an/(2*pi))))
                nan = (tf direction) * 2 * pi / 8
                ((nx,ny) as normal) = (cos nan, sin nan)
                d = V.dot diff normal
                (fx, fy) = press.position
                (x,y) = dp.coords
                (tx,ty) = [(1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1), (0,-1), (1,-1)] |> list_get direction |> Maybe.withDefault (0,0)
                x2 = x + tx
                y2 = y + ty
            in 
                if d >= scale /4 then
                    Just (x2,y2)
                else
                    Nothing
                 
        _ -> Nothing

end_move press model =
    case press.action of
        DragPiece (i,dp) ->
            let
                x1 = modBy model.num_pieces i
                y1 = (i-x1)//model.num_pieces

                smodel = case piece_moving_to press of
                    Just (x2,y2) -> swap_pieces (x1,y1) (x2,y2) { model | board = line_up_pieces model.board }
                    Nothing -> model
            in 
                { smodel | board = line_up_pieces smodel.board }
                 

        _ -> model

start_press press model = { model | last_key_move = Nothing, keys_active = False }

piece_methods : PieceMethods Model Piece
piece_methods =
    let
        get_pieces = (.board) >> List.concat

        pick_piece buffer pos = (List.indexedMap (\i -> \p -> ((i,p), distance_from_piece pos p))) >> List.reverse >> List.filter (\(_,d) -> d <= buffer) >> List.sortBy second >> List.map first >> List.head

    in
        { get_pieces = get_pieces
        , pick_piece = pick_piece
        , after_move = move_pieces
        , end_move = end_move
        , start_press = start_press
        }

update_piece : Int -> Int -> ((Int,Int) -> Piece -> Piece) -> Board -> Board
update_piece num_pieces i fn = List.indexedMap (\y -> List.indexedMap (\x -> \p -> if y*num_pieces + x == i then fn (x,y) p else p))

move_pieces = press_fold (\press -> \model ->
    let
        board = model.board
        nboard = 
            case press.action of
                DragPiece (i,op) -> update_piece model.num_pieces i (\_ -> \p -> { p | position = V.add (V.sub press.position press.start_position) op.position }) board
                _ -> board
    in
        { model | board = nboard }
    )



init_piece n y x = 
    { value = x
    , position = ((tf x)*scale, (tf y)*scale)
    , coords = (x,y)
    , key = y*n + x
    }

init_board n = List.range 0 1 |> List.map (\y -> List.range 0 (n-1) |> List.map (init_piece n y))

solved_board n = List.map List.reverse (init_board n)

abstract_board = List.map (List.map (.value))

init_num_pieces = 5

init_model num_pieces =
    { num_pieces = num_pieces
    , board = init_board num_pieces
    , cursor_pos = (0,0)
    , mouse = (0,0)
    , presses = []
    , history = ([], [])
    , last_key_move = Nothing
    , keys_active = False
    , screen = Intro
    }

init : () -> (Model, Cmd Msg)
init _ = init_model init_num_pieces |> nocmd

nocmd model = (model, Cmd.none)

history_move : (Board -> History Board -> (Board, History Board)) -> Model -> Model
history_move fn model = case fn model.board model.history of
    (board, history) -> { model | board = board, history = history }

set_screen screen model = { model | screen = screen }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Undo -> history_move undo model |> nocmd

    Redo -> history_move redo model |> nocmd

    SeekHistory v -> case (String.toInt v) of
        Just i -> history_move (seek i) model |> nocmd
        Nothing -> nocmd model

    SeekToStart -> history_move (seek 0) model |> nocmd
    SeekToEnd -> history_move (seek (History.length model.history)) model |> nocmd

    SetScreen screen -> set_screen screen model |> nocmd

    Fewer -> 
        if model.num_pieces > 1 then
            init_model (model.num_pieces - 1) |> set_screen Game |> nocmd
        else
            model |> nocmd

    More -> init_model (model.num_pieces + 1) |> set_screen Game |> nocmd

    SvgMsg tmsg -> TouchEvents.update piece_methods tmsg model |> nocmd

    KeyPress kmsg -> 
           { model | keys_active = True } 
        |> \m -> case kmsg of
            KeyPress _ -> m |> nocmd
            _ -> update kmsg m
           

    SwapLeft -> horizontal_key_move -1 model |> nocmd
    SwapRight -> horizontal_key_move 1 model |> nocmd
    SwapVertical -> vertical_key_move model |> nocmd
    CursorLeft -> move_cursor model (-1,0) |> nocmd
    CursorRight -> move_cursor model (1,0) |> nocmd
    CursorUp -> move_cursor model (0,-1) |> nocmd
    CursorDown -> move_cursor model (0,1) |> nocmd

    FocusBoard -> { model | keys_active = True } |> nocmd

move_cursor model (dx,dy) = 
    let
        (x,y) = model.cursor_pos
    in
        { model | cursor_pos = (clamp 0 (model.num_pieces-1) (x+dx), clamp 0 1 (modBy 2 (y+dy))), last_key_move = Nothing }

list_get i = List.drop i >> List.head

get_piece : Board -> (Int,Int) -> Maybe Piece
get_piece board (x,y) = board |> list_get y |> Maybe.andThen (list_get x)

valid_coords n (x,y) = x>= 0 && x< n && y>=0 && y<2

pieces_can_swap : Model -> (Int,Int) -> (Int,Int) -> Bool
pieces_can_swap model ((x1,y1) as c1) ((x2,y2) as c2) =
    valid_coords model.num_pieces c1
    &&
    valid_coords model.num_pieces c2
    &&
    abs (x1-x2) <= 1
    &&
    (case Maybe.map2 pair (get_piece model.board (x1,y1)) (get_piece model.board (x2,y2)) of
        Just (p1,p2) -> abs (p1.value - p2.value) == 1
        Nothing -> False
    )

swap_pieces : (Int,Int) -> (Int,Int) -> Model -> Model
swap_pieces ((x1,y1) as c1) ((x2,y2) as c2) model =
    if pieces_can_swap model c1 c2 then
        let
            mp1 = get_piece model.board (x1,y1)
            mp2 = get_piece model.board (x2,y2)
            nboard = case (mp1, mp2) of
                (Just p1, Just p2) -> Just <| List.indexedMap (\y -> \row -> List.indexedMap (\x -> \p -> if (x,y) == c1 then p2 else if (x,y) == c2 then p1 else p) row) model.board

                _ -> Nothing

        in
            case nboard of
                Just b2 -> { model | board = b2, history = add_step model.board model.history }

                Nothing -> model
    else
        model

subscriptions : Model -> Sub msg
subscriptions model = Sub.none

pluralise : String -> String -> Int -> String
pluralise single plural n = if n == 1 then single else plural

svg_classList = List.filter second >> List.map first >> String.join " " >> SA.class

i_to_xy : Int -> Int -> (Int,Int)
i_to_xy num_pieces i =
    let
        x = modBy num_pieces i
        y = (i-x) // num_pieces
    in
        (x,y)

is_swap_press : Model -> (Int,Int) -> Press -> Bool
is_swap_press model (x,y) press = case press.action of
    DragPiece (i2,p2) ->
        let
            (x2,y2) = i_to_xy model.num_pieces i2
        in
            (x,y) /= (x2,y2) && pieces_can_swap model (x,y) (x2,y2)

    _ -> False

horizontal_key_move dx model =
    let
        (x,y) = model.cursor_pos
        do_move (x2,y2) =
            swap_pieces (x,y) (x2,y2) { model | cursor_pos = (x2,y2), last_key_move = Just (model.cursor_pos, (x2,y2)) }
            |> \m -> { m | board = line_up_pieces m.board }
    in
        case (pieces_can_swap model (x,y) (x + dx, y), pieces_can_swap model (x,y) (x + dx, 1-y)) of
            (True, _) -> do_move (x + dx, y)

            (_, True) -> do_move (x + dx, 1-y)

            _ -> model

vertical_key_move model = case model.last_key_move of
    Just ((x1,y1), (x2,y2)) ->
        if pieces_can_swap model (x2,y2) (x2, 1-y2) then
            history_move undo model |> swap_pieces (x1,y1) (x2, 1-y2) |> \m -> { m | board = line_up_pieces m.board, cursor_pos = (x2, 1-y2), last_key_move = Just ((x1,y1), (x2,1-y2)) }
        else
            model
    
    Nothing -> model


keymap : Bool -> String -> Maybe Msg
keymap shifted key = case key of
    "ArrowLeft" -> if shifted then Just SwapLeft else Just CursorLeft
    "ArrowRight" -> if shifted then Just SwapRight else Just CursorRight
    "ArrowUp" -> if shifted then Just SwapVertical else Just CursorUp
    "ArrowDown" -> if shifted then Just SwapVertical else Just CursorDown
    "u" -> Just Undo
    "U" -> Just SeekToStart
    "r" -> Just Redo
    "R" -> Just SeekToEnd 
    _ -> Nothing

decode_keypress = 
    D.map2 pair
        (D.field "key" D.string)
        (D.field "shiftKey" D.bool)
    |> D.andThen (\(key,shifted) ->
        case keymap shifted key of
            Just msg -> D.succeed (KeyPress msg)
            Nothing -> D.fail ("unrecognised key "++key)
       )

view : Model -> Html Msg
view model = case model.screen of
    Intro -> view_intro model
    Game -> view_game model

control_button msg label enabled =
    button
        [ HE.onClick msg
        , HA.disabled (not enabled)
        , HA.id label
        ]
        [ text label ]

view_intro model =
    div
        [ HA.id "intro" ]
        [ H.header
            []
            [ H.h1 [] [text "Double Back"]
            ]
        
        , H.node "main"
            []
            [ H.p [] [ text "Put the baubles in reverse order!" ]
            , H.p [] [ text "You can swap two adjacent baubles if they have adjacent numbers on them." ]

            , control_button (SetScreen Game) "Begin" True
            ]
        ]

view_game : Model -> Html Msg
view_game model = 
    let
        svg_row y row =
            (List.indexedMap (svg_piece y) row)

        svg_piece y x p =
            let
                (cx,cy) = p.position
                i = y * model.num_pieces + x

                select_press = List.filter (is_dragging i) model.presses |> List.head

                selected = select_press /= Nothing || (model.keys_active && model.cursor_pos == (x,y))

                swapping_with = List.filter (\press -> piece_moving_to press == Just (x,y) && is_swap_press model (x,y) press) model.presses 

                pressed_pieces : List (Int,Int)
                pressed_pieces = 
                    List.filterMap 
                        (\press -> case press.action of
                            DragPiece (i2,p2) -> Just (i_to_xy model.num_pieces i2)
                            _ -> Nothing
                        )
                        model.presses

                can_be_swapped = List.filter (pieces_can_swap model (x,y)) pressed_pieces |> (/=) []

                (sx,sy) = case List.head swapping_with |> Maybe.map (\press -> (press, press.action)) of
                    Just (press, DragPiece (i2,p2)) ->
                        let
                            (x2,y2) = i_to_xy model.num_pieces i2
                            (tx,ty) = cell_position x2 y2
                            (mx,my) = V.sub press.position press.start_position
                            (dx,dy) = V.sub p.position p2.position
                            d = (V.dot (V.normalise (dx,dy)) (mx,my)) / (V.len (dx,dy)) - (1/4) |> (*) (4/3) |> clamp 0 1
                            ed = 1 - ((1-d) ^ 2)
                        in
                           V.add (cx,cy) (V.smul ed (V.sub (tx, ty) (cx,cy)))

                    _ -> case select_press |> Maybe.map (\press -> (press, piece_moving_to press)) of
                        Just (press, Just (x2,y2)) ->
                            let
                                (tx,ty) = cell_position x2 y2
                                (mx,my) = V.sub press.position press.start_position
                                (dx,dy) = V.sub (tx,ty) (cell_position x y)
                                d = (V.dot (V.normalise (dx,dy)) (mx,my)) / (V.len (dx,dy)) |> clamp 0 1
                            in
                                if pieces_can_swap model (x,y) (x2,y2) then
                                    V.add (cx,cy) (V.smul d (V.sub (tx, ty) (cx,cy)))
                                else
                                    (cx,cy)


                        _ -> (cx, cy)

            in
                (fi p.key
                , draw_bauble 
                    p.value
                    (sx, sy)
                    [ svg_classList
                        [ ("piece", True)
                        , ("selected", selected)
                        , ("swapping", swapping_with /= [])
                        , ("swappable", can_be_swapped)
                        ]
                    ]
                    ""
                )

        draw_bauble value (x, y) attrs transform =
            Svg.g
                ( attrs ++ 
                  [
                    HA.style "transform" <| (strf "translate(%px, %px)" [ff x, ff y])++" "++transform
                  ]
                )

                [ Svg.circle
                    [ SA.r <| ff r
                    ]
                    [ ]

                , Svg.use
                    [ HA.attribute "href" "bauble.svg#layer1"
                    , SA.x "-6"
                    , SA.y "-7.181"
                    ]
                    []

                , Svg.text_
                    [ SA.dominantBaseline "middle"
                    , SA.textAnchor "middle"
                    , SA.fontSize <| ff <| r
                    ]
                    [ Svg.text <| fi (value+1) ]
                ]


        num_moves = List.length <| first <| model.history

        is_solved = (abstract_board model.board) == (abstract_board <| solved_board model.num_pieces)

    in
        div
            [ HA.id "game"
            , HA.classList 
                [ ("solved", is_solved)
                ]
            ]
            (
                [ div
                    [ HA.id "controls" ]

                    [ control_button Undo "Undo" (can_undo model.history)

                    , H.input
                        [ HA.type_ "range"
                        , HA.min "0"
                        , HA.max <| fi (History.length model.history)
                        , HA.value <| fi <| List.length <| first model.history
                        , HE.onInput SeekHistory
                        , HA.id "seek-history"
                        ]
                        []

                    , control_button Redo "Redo" (can_redo model.history)

                    , control_button Fewer "Fewer" (model.num_pieces > 2)

                    , control_button More "More" True
                    ]

                , svg
                    (  (touch_events SvgMsg)
                    ++ [ HA.attribute "viewBox" <| strf "% % % %" (List.map ff [-scale, -scale*1.5, (tf (model.num_pieces+1))*scale, 3.5*scale])
                       , HA.id "board"
                       , HA.attribute "tabindex" "0"
                       , HE.on "keydown" decode_keypress
                       , HE.onFocus FocusBoard
                       ]
                    )
                    [ Svg.rect
                        [ SA.x <| ff (-scale/2)
                        , SA.y <| ff (-scale/2)
                        , SA.rx <| ff (scale/2)
                        , SA.ry <| ff (scale/2)
                        , SA.width <| ff <| (tf (model.num_pieces))*scale
                        , SA.height <| ff (2*scale)
                        , SA.id "background"
                        , SA.fill "black"
                        , SA.fillOpacity "0.5"
                        ]
                        []

                    , Svg.g
                        [ SA.id "ghosts"
                        ]
                        (List.map (\i -> draw_bauble i ((tf (model.num_pieces - i - 1))*scale, -scale*3/4) [ SA.class "piece" ] "scale(0.5)") (List.range 0 (model.num_pieces-1)))

                    , SK.node "g"
                        []
                        (List.concat <| List.indexedMap svg_row model.board)

                    , Svg.g
                        [ SA.id "finished"
                        , SA.transform <| strf "translate(%, %)" [ff <| (tf (model.num_pieces-1))*scale/2, ff <| -scale/2]
                        ]

                        [ Svg.text_
                            [ SA.dominantBaseline "middle"
                            , SA.textAnchor "middle"
                            , SA.fontSize <| ff <| scale*0.6
                            , SA.class "wow"
                            ]
                            [ Svg.text "Finished!!!" ]

                        , Svg.text_
                            [ SA.dominantBaseline "middle"
                            , SA.textAnchor "middle"
                            , SA.fontSize <| ff <| scale/2
                            , SA.y <| ff scale
                            ]
                            [ Svg.tspan [] [Svg.text "in " ]
                            , Svg.tspan [ SA.id "num-moves", SA.class "wow" ] [ Svg.text <| fi <| num_moves ]
                            , Svg.tspan [] [Svg.text <| if num_moves == 1 then " move" else " moves" ]
                            ]
                        ]
                    ]

                ]
            )
