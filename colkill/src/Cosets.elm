module Cosets exposing (makeCoset)

type alias Vector = List Bool

makeCoset : List Vector -> Int -> List Vector
makeCoset codewords n =
    List.map (xorVectors (makeTwoHot 0 n)) codewords
        |> List.filter isWeightTwo


xorVectors : Vector -> Vector -> Vector
xorVectors a b =
    case (List.head a, List.head b) of
        (Just x, Just y) ->
            (xor x y)
            :: (xorVectors (List.drop 1 a) (List.drop 1 b))

        _ -> []

makeTwoHot : Int -> Int -> Vector
makeTwoHot a b =
    turnElemOn a (List.repeat 32 False)
        |> turnElemOn b

isWeightTwo : Vector -> Bool
isWeightTwo vec =
    List.filter identity vec
        |> List.length
        |> (==) 2

turnElemOn : Int -> Vector -> Vector
turnElemOn n vec =
    List.concat
        [ (List.take n vec)
        , [True]
        , (List.drop (n + 1) vec)
        ]

