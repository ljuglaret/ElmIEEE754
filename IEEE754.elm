module IEEE755 exposing(..)
import FoldPerso exposing(..)


----- LIGNE 1 -----

partieEntiere : Float -> Int 
partieEntiere p = 
    case (String.split "." (String.fromFloat p)) of 
        x::y::s -> Maybe.withDefault 0 (String.toInt x) 
        x::s  -> Maybe.withDefault 0 (String.toInt x) 
        _ -> 0 


partieEntiereBin :  Int  -> List Int 
partieEntiereBin x = decompositionBase x  2


partieDecimale : Float -> Float  
partieDecimale p = 
    case (String.split "." (String.fromFloat p)) of 
        x::y::s -> Maybe.withDefault 0 (String.toFloat ("0."++y))
        _ -> 0   


finalApresVirgule  :  Float -> List Int 
finalApresVirgule x = 
    let
        aux : List Int -> Float -> Int -> List Int 
        aux l x0  compte = 
            let 
                a = (partieEntiere (2* x0))
            in
                if (compte /= 10)
                then aux  (a ::l) (2*x0 - (toFloat a) )     (compte + 1 )
                else List.reverse l
    in aux [] (partieDecimale x) 0

final : Float -> (List Int , List Int)
final x = (partieEntiereBin (partieEntiere x) , finalApresVirgule x )

----- LIGNE 2 -----

{-
15 < 16 = 2^4 -> 126 + 4 
-}
exposant : Float -> Int 
exposant x = 
    let 
        expoMax x0 puiss2 acc = 
            if (x0 > puiss2 )
            then expoMax x0       (2 * puiss2)  (acc + 1 ) 
            else 126 + acc
    in expoMax (partieEntiere x)  1 0

exposantEnBinaire :  Float -> List Int 
exposantEnBinaire x = decompositionBase (exposant x ) 2

----- LIGNE 3 -----

complete : List Int -> List Int 
complete str = 
    if (List.length str == 32) 
    then str 
    else complete (str++[0])


format x = 
    let 
        signe =
            if x < 0
            then [1]
            else[0]
    in
        let 
            semi = signe ++ (exposantEnBinaire x)++ (List.drop 1 ((partieEntiereBin (partieEntiere x)) ++ (finalApresVirgule x)))
        in complete semi


----- LIGNE 4 et 5 -----

parBlocs : List Int -> List (List Int )
parBlocs l =
    if List.isEmpty l
    then []
    else
        (List.take 4 l) :: (parBlocs (List.drop 4 l))


binVersHexa : List Int -> String
binVersHexa b =
    let 
        dec :  Int
        dec = foldLeft (\x y -> x + 2  * y ) 0 b
    in
        if dec < 10
        then String.fromFloat (toFloat dec)
        else 
            if dec == 10
            then "A"
            else if dec == 11
            then "B"
            else if dec == 12
            then "C"
            else if dec == 13
            then "D"
            else if dec == 14
            then "E"
            else  "F"

blocsHexa : Float ->  String
blocsHexa x = String.concat(List.map (\bloc -> binVersHexa bloc) (parBlocs(format x )))

