module Array2D exposing (Array2D, cartesianProduct, empty, get, indexedMap, initialize)

import Array exposing (Array, indexedMap, initialize, repeat)


type alias Array2D a =
    Array (Array a)


initialize : Int -> Int -> (Int -> Int -> a) -> Array2D a
initialize rows cols func =
    let
        initializeRow rowIndex =
            Array.initialize cols (func rowIndex)
    in
    Array.initialize rows initializeRow


empty : Array2D a
empty =
    Array.fromList [ Array.fromList [] ]


get : Int -> Int -> Array2D a -> Maybe a
get rowIndex colIndex arr =
    arr
        |> Array.get rowIndex
        |> Maybe.andThen (Array.get colIndex)


map : (a -> b) -> Array2D a -> Array2D b
map func arr =
    let
        mapRow row =
            row |> Array.map func
    in
    arr |> Array.map mapRow


indexedMap : (Int -> Int -> a -> b) -> Array2D a -> Array2D b
indexedMap func arr =
    let
        mapElement rowIndex colIndex element =
            element |> func rowIndex colIndex

        mapRow rowIndex row =
            row |> Array.indexedMap (mapElement rowIndex)
    in
    arr |> Array.indexedMap mapRow


cartesianProduct : (a -> b -> result) -> Array a -> Array b -> Array2D result
cartesianProduct func xs ys =
    Array.map (\x -> Array.map (\y -> func x y) ys) xs
