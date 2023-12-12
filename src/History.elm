module History exposing
    ( History
    , undo
    , redo
    , add_step
    , can_undo
    , can_redo
    , seek
    , length
    )

type alias History a = (List a, List a)

add_step : a -> History a -> History a
add_step step (past,future) = (step::past, [])

can_undo : History a -> Bool
can_undo (past,future) = past /= []

can_redo : History a -> Bool
can_redo (past,future) = future /= []

undo : a -> History a -> (a, History a)
undo current (past,future) = case past of
    a::rest -> (a, (rest, current::future))
    [] -> (current, (past,future))

redo : a -> History a -> (a, History a)
redo current (past,future) = case future of
    a::rest -> (a, (current::past, rest))
    [] -> (current, (past,future))


seek : Int -> a -> History a -> (a, History a)
seek i current (past, future) =
    let
        np = List.length past
    in
        if i < np then case past of
            p::rest -> seek i p (rest, current::future)
            _ -> (current , (past,future))
        else if i > np then case future of
            f::rest -> seek i f (current::past, rest)
            _ -> (current, (past,future))
        else
            (current, (past, future))

length : History a -> Int
length (past,future) = (List.length past) + (List.length future)
