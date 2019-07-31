import Browser
import Html exposing (Html, Attribute, div, button, text, span, hr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Set exposing (Set)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button

import Codewords
import Cosets


main =
    Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Vector = List Bool
type alias Coset = List Vector

type VecState
    = Alive
    | Hurt
    | Dead

type alias Model =
    { kills : Set Int
    , cosets : List Coset
    }

init : Model
init =
    { kills = Set.empty
    , cosets =
        List.map
            (Cosets.makeCoset Codewords.codewords32)
            (List.range 1 31)
    }


-- UPDATE

type Msg
    = Kill Int
    | Unkill Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        Kill i ->
            { model | kills = Set.insert i model.kills }
        Unkill i ->
            { model | kills = Set.remove i model.kills }


-- VIEW

view : Model -> Html Msg
view model =
        div []
            [ div [] (createButtons model.kills 32)
            , viewCosets model.kills model.cosets
            ]


viewCosets : Set Int -> List Coset -> Html Msg
viewCosets kills cosets =
    div []
        (List.indexedMap (viewCoset kills) cosets)

viewCoset : Set Int -> Int -> Coset -> Html Msg
viewCoset kills n coset =
    let
        cosetTitle = text ("Coset " ++ String.fromInt n)

        states = List.map (getVecState kills) coset
    in
        cosetTitle :: List.map viewVector (pairs coset states)
            |> div [ class "coset-container" ]

viewVector : (Vector, VecState) -> Html Msg
viewVector (vec, state) =
    case state of
        Alive ->
            div [] (List.map viewElem vec)

        Hurt ->
            div [ class "vec-hurt" ] (List.map viewElem vec)

        Dead ->
            div [ class "vec-dead" ] (List.map viewElem vec)


viewElem : Bool -> Html Msg
viewElem a =
    if a
    then
        span [] [ text "1" ]
    else
        span [] [ text "0" ]

createButtons : Set Int -> Int -> List (Html Msg)
createButtons kills n =
    List.map (createButton kills) (List.range 1 n)

createButton : Set Int -> Int -> Html Msg
createButton kills i =
    if Set.member i kills
    then
        Button.button
            [ Button.danger, Button.attrs [ onClick (Unkill i) ] ]
            [ text (String.fromInt i) ]
    else
        Button.button
            [ Button.attrs [ onClick (Kill i) ] ]
            [ text (String.fromInt i) ]

getVecState : Set Int -> Vector -> VecState
getVecState kills vec =
    let
        first = Set.member (getFirstOn vec) kills
        second = Set.member (getSecondOn vec) kills
    in
        case (first, second) of
            (True, True) -> Dead
            (False, False) -> Alive
            _ -> Hurt

pairs : List a -> List b -> List (a, b)
pairs xs ys =
    List.map2 Tuple.pair xs ys

getFirstOn : Vector -> Int
getFirstOn vec =
    List.indexedMap Tuple.pair vec
        |> List.foldl keepTrue -1

getSecondOn : Vector -> Int
getSecondOn vec =
    List.indexedMap Tuple.pair vec
        |> List.foldr keepTrue -1

keepTrue : (Int, Bool) -> Int -> Int
keepTrue (i, v) a =
    if a /= -1
    then
        a
    else if not v
    then
        -1
    else
        i + 1
