module TouchEvents exposing 
    ( TouchEvent(..)
    , TouchInfo(..)
    , PressAction(..)
    , PieceMethods
    , Press
    , touch_events
    , update
    , press_fold
    , is_dragging
    )

import Json.Decode as D
import Svg
import Svg.Events as SE
import Vector as V exposing (Vector)
import Util exposing (update_where, listGet)

type alias PieceMethods a piece =
    { get_pieces : Model a piece -> List piece
    , pick_piece : Float -> Vector -> List piece -> Maybe (Int,piece)
    , after_move : Model a piece -> Model a piece
    , end_move : Press piece -> Model a piece -> Model a piece
    , start_press : Press piece -> Model a piece -> Model a piece
    }

type alias Model a piece = 
    { a 
    | mouse : Vector
    , presses : List (Press piece)
    }

type PressSource
    = Mouse Int
    | Touch Int

type PressKind
    = StaticPress
    | MovingPress

type PressAction piece
    = Background
    | DragPiece (Int, piece)

type alias Press piece =
    { start_position: Vector
    , position : Vector
    , source : PressSource
    , action : PressAction piece
    , kind : PressKind
    }

type TouchInfo = TouchInfo Int Float Float

type TouchEvent 
    = MouseMove Float Float
    | MouseUp Int
    | MouseDown Int
    | TouchStart (List TouchInfo)
    | TouchMove (List TouchInfo)
    | TouchEnd (List TouchInfo)

decode_touch : (List TouchInfo -> TouchEvent) -> (TouchEvent -> msg) -> D.Decoder msg
decode_touch tmsg msg =
    D.field "detail" 
    <| D.map (tmsg >> msg)
    <| D.list 
        (D.map3 TouchInfo
            (D.field "identifier" D.int)
            (D.at ["position","x"] D.float)
            (D.at ["position","y"] D.float)
        )

decode_mouse : (Int -> TouchEvent) -> (TouchEvent -> msg) -> D.Decoder msg
decode_mouse tmsg msg = D.map (tmsg >> msg) (D.field "button" D.int)

decode_event_vector : (Float -> Float -> TouchEvent) -> (TouchEvent -> msg) -> D.Decoder msg
decode_event_vector tmsg msg = 
    D.map msg 
    <| D.map2 tmsg
        (D.at ["detail", "x"] D.float)
        (D.at ["detail", "y"] D.float)


touch_events : (TouchEvent -> msg) -> List (Svg.Attribute msg)
touch_events msg =
    [ SE.on "svgmove" <| decode_event_vector MouseMove msg
    , SE.on "mouseup" <| decode_mouse MouseUp msg
    , SE.on "mousedown" <| decode_mouse MouseDown msg
    , SE.on "svgtouchstart" <| decode_touch TouchStart msg
    , SE.on "svgtouchmove" <| decode_touch TouchMove msg
    , SE.on "svgtouchend" <| decode_touch TouchEnd msg
    ]

press_fold : (Press piece -> Model a piece -> Model a piece ) -> Model a piece -> Model a piece
press_fold fn model = List.foldl fn model model.presses

is_dragging : Int -> Press piece -> Bool
is_dragging i press = case press.action of
    DragPiece (j,_) -> i == j
    _ -> False

update : PieceMethods a piece -> TouchEvent -> Model a piece -> Model a piece
update methods msg umodel = 
    let
        start_press : Vector -> PressSource -> Model a piece -> Model a piece
        start_press position source model = 
            let
                picked_piece = methods.pick_piece (press_buffer source) position (methods.get_pieces model)
                action = case picked_piece of
                    Just (i,p) -> DragPiece (i,p)
                    Nothing -> Background
                press = 
                    { start_position = position
                    , position = position
                    , source = source
                    , action = action
                    , kind = StaticPress
                    }
            in
                if List.any (\s -> s.source == source) model.presses then
                    model
                else
                    { model | presses = press::model.presses} |> methods.start_press press

        if_active_press : PressSource -> (Model a piece -> Model a piece) -> Model a piece -> Model a piece
        if_active_press source fn model =
            if List.any (\s -> s.source == source) model.presses then
                fn model
            else
                model

        end_press : PressSource -> Model a piece -> Model a piece
        end_press source = if_active_press source (press_fold methods.end_move >> remove_press source)

        press_buffer : PressSource -> Float
        press_buffer source = case source of
            Mouse _ -> 1
            Touch _ -> 3

        remove_press : PressSource -> Model a piece -> Model a piece
        remove_press source model = { model | presses = List.filter (\s -> s.source /= source) model.presses }

        is_mouse_press : PressSource -> Bool
        is_mouse_press source = case source of
            Mouse _ -> True
            _ -> False

        update_press_sources : (PressSource -> Bool) -> (Press piece -> Press piece) -> List (Press piece) -> List (Press piece)
        update_press_sources is_source = update_where (\p -> is_source p.source)

        set_press_position : (PressSource -> Bool) -> Vector -> Model a piece -> Model a piece
        set_press_position is_source pos model = 
            let
                update_press p =
                    let
                        d = V.len (V.sub pos p.start_position)
                        nkind = case p.kind of
                            StaticPress -> if d > 0.1 then MovingPress else StaticPress
                            _ -> p.kind
                    in
                        { p | position = pos, kind = nkind }
            in
                { model | presses = update_press_sources is_source update_press model.presses }


        start_touches : List TouchInfo -> Model a piece -> Model a piece
        start_touches touches model =
            List.foldl (\touch -> \m ->
                case touch of
                    TouchInfo id x y -> start_press (x,y) (Touch id) m
            ) model touches

        set_mouse : Vector -> Model a piece -> Model a piece
        set_mouse v model = { model | mouse = v }

        move_touches : List TouchInfo -> Model a piece -> Model a piece
        move_touches touches model = List.foldl (\touch -> \m -> case touch of
            TouchInfo id x y -> set_press_position ((==) (Touch id)) (x,y) m) model touches

        end_touches : List TouchInfo -> Model a piece -> Model a piece
        end_touches touches model = List.foldl (\touch -> \m -> case touch of
            TouchInfo id x y -> end_press (Touch id) m
            ) model touches


    in
        case msg of
            MouseDown button -> start_press umodel.mouse (Mouse button) umodel
            MouseUp button -> end_press (Mouse button) umodel
            MouseMove x y -> (set_mouse (x,y) >> set_press_position is_mouse_press (x,y) >> methods.after_move) umodel
            TouchStart touches -> start_touches touches umodel
            TouchMove touches -> (move_touches touches >> methods.after_move) umodel
            TouchEnd touches -> end_touches touches umodel

