module Example exposing (suite)

import Array
import Array2D
import Expect
import Test exposing (Test, describe, test)
import Tuple as Tuple


suite : Test
suite =
    let
        array1 =
            Array.fromList [ "A", "B", "C" ]

        array2 =
            Array.fromList [ 1, 2, 3 ]

        array3 =
            Array.fromList
                [ Array.fromList [ ( "A", 1 ), ( "A", 2 ), ( "A", 3 ) ]
                , Array.fromList [ ( "B", 1 ), ( "B", 2 ), ( "B", 3 ) ]
                , Array.fromList [ ( "C", 1 ), ( "C", 2 ), ( "C", 3 ) ]
                ]

        array4 =
            Array.fromList
                [ Array.fromList [ ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ]
                , Array.fromList [ ( 2, 1 ), ( 2, 2 ), ( 2, 3 ) ]
                , Array.fromList [ ( 3, 1 ), ( 3, 2 ), ( 3, 3 ) ]
                ]
    in
    describe "Math tests"
        [ test "List2D is a cartesian product of two lists" <|
            \_ ->
                let
                    actual =
                        Array2D.cartesianProduct Tuple.pair array1 array2
                in
                Expect.equal actual array3
        , test "get" <|
            let
                actual =
                    Array2D.get { rowIndex = 2, colIndex = 1 } array3
            in
            \_ -> Expect.equal actual <| Just ( "C", 2 )
        , test "indexedMap" <|
            let
                actual =
                    Array2D.indexedMap (\pos _ -> ( pos.rowIndex + 1, pos.colIndex + 1 )) array3
            in
            \_ -> Expect.equal actual array4
        ]
