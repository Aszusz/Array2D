module Array2D exposing (Array2D, Position, Size, cartesianProduct, empty, get, indexedMap)

import Array exposing (Array, indexedMap)


type alias Array2D a =
    Array (Array a)


type alias Size =
    { cols : Int
    , rows : Int
    }


type alias Position =
    { rowIndex : Int
    , colIndex : Int
    }


empty : Array2D a
empty =
    Array.fromList [ Array.fromList [] ]


get : Position -> Array2D a -> Maybe a
get pos arr =
    arr
        |> Array.get pos.rowIndex
        |> Maybe.andThen (Array.get pos.colIndex)


map : (a -> b) -> Array2D a -> Array2D b
map func arr =
    let
        mapRow row =
            row |> Array.map func
    in
    arr |> Array.map mapRow


indexedMap : (Position -> a -> b) -> Array2D a -> Array2D b
indexedMap func arr =
    let
        mapElement rowIndex colIndex element =
            element |> func { rowIndex = rowIndex, colIndex = colIndex }

        mapRow rowIndex row =
            row |> Array.indexedMap (mapElement rowIndex)
    in
    arr |> Array.indexedMap mapRow


cartesianProduct : (a -> b -> result) -> Array a -> Array b -> Array2D result
cartesianProduct func xs ys =
    Array.map (\x -> Array.map (\y -> func x y) ys) xs
