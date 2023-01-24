module LatLngTests exposing (..)

import Expect as E
import Fuzz exposing (floatRange)
import LatLng exposing (..)
import Length exposing (kilometers)
import Places exposing (br)
import Quantity as Q
import Test exposing (..)


suite : Test
suite =
    describe "Basic conversions"
        [ describe "LatLng parsing" <|
            [ test "render origin" <| \_ -> E.equal (fromLatLonDegrees 0 0 |> toString) "0°0'00.00\"N 0°0'00.00\"E"
            , test "render br" <| \_ -> E.equal (br.center |> toString) "..."
            ]
        , describe "LatLng Transforms" <|
            [ test "move" <| \_ -> E.equal (br.capital |> move (kilometers 100) (kilometers 100) |> toString) "..."
            , test "render br" <| \_ -> E.equal (br.center |> toString) "..."
            ]
        , describe "Decode/encode roundtrip"
            [ fuzz2 (floatRange -180 180) (floatRange -90 90) "encode/decode roundtrip" <|
                \lat lng ->
                    let
                        loc =
                            fromLatLonDegrees lat lng
                    in
                    case parse (toString loc) of
                        Just loc_ ->
                            E.lessThan 0.1 <| (distance loc loc_ |> Q.abs |> Length.inMeters)

                        Nothing ->
                            E.fail "failed to parse"
            ]
        ]
