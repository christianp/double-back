module Util exposing (..)

import Tuple exposing (first)

fi = String.fromInt
ff = String.fromFloat
tf = toFloat

strf : String -> List String -> String
strf template bits =
    let
        next_bit cbits = case cbits of
            a::rest -> (a,rest)
            [] -> ("",[])
    in
        first <| List.foldl (\chr -> \(out,cbits) -> 
            if chr=='%' then
                let
                    (suffix,nbits) = next_bit cbits
                in
                    (out++suffix, nbits)
            else
                (out++(String.fromChar chr), cbits)
        ) ("",bits) (String.toList template)


update_where : (a -> Bool) -> (a -> a) -> List a -> List a
update_where test fn = List.map (\x -> if test x then fn x else x)

listGet n = (List.drop n) >> List.head
