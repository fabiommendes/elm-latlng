module LatLngTests exposing (..)

import Expect as E
import Fuzz exposing (floatRange)
import Json.Decode as D
import LatLng exposing (..)
import Length exposing (Length, inKilometers, kilometers, meters)
import Places exposing (br, origin)
import Quantity as Q
import Test exposing (..)


oneDegreeMts : Length
oneDegreeMts =
    distance (fromLatLngDegrees 0 0) (fromLatLngDegrees 1 0)


suite : Test
suite =
    describe "Basic Tests"
        [ describe "Rendering" <|
            [ test "origin" <| \_ -> E.equal (fromLatLngDegrees 0 0 |> toString) "0°0'00.00\"N 0°0'00.00\"E"
            , test "br" <| \_ -> E.equal (br.center |> toString) "15°40'36.28\"S 47°40'41.98\"W"
            ]
        , describe "Conversions" <|
            [ test "toString" <|
                \_ ->
                    fromLatLngDegrees 45.125 45.125
                        |> toString
                        |> E.equal "45°7'30.00\"N 45°7'30.00\"E"
            , fuzz2 (floatRange -90 90) (floatRange -180 180) "from/to pair" <|
                \x y ->
                    fromLatLngDegrees x y
                        |> toTupleDegrees
                        |> distance2d ( x, y )
                        |> E.lessThan 1.0e-6
            ]
        ]


transforms : Test
transforms =
    describe "Transforms" <|
        [ test "move" <|
            \_ ->
                origin
                    |> move oneDegreeMts oneDegreeMts
                    |> toString
                    |> E.equal "1°0'00.00\"N 1°0'00.00\"E"
        , test "move north" <|
            \_ ->
                origin
                    |> move oneDegreeMts Q.zero
                    |> toString
                    |> E.equal "1°0'00.00\"N 0°0'00.00\"E"
        , test "move east" <|
            \_ ->
                origin
                    |> move Q.zero oneDegreeMts
                    |> toString
                    |> E.equal "0°0'00.00\"N 1°0'00.00\"E"
        , fuzz (floatRange -1000 1000) "move lat and back" <|
            \x ->
                origin
                    |> move (kilometers x) Q.zero
                    |> move (kilometers -x) Q.zero
                    |> E.equal origin
        , fuzz (floatRange -1000 1000) "move lng and back" <|
            \x ->
                origin
                    |> move Q.zero (kilometers x)
                    |> move Q.zero (kilometers -x)
                    |> E.equal origin
        ]


parsing : Test
parsing =
    describe "Parsing" <|
        [ test "parse" <|
            \_ ->
                parse "45°7'30.00\"N 45°7'30.00\"E"
                    |> E.equal (Just <| fromLatLngDegrees 45.125 45.125)
        , test "parse (fail)" <|
            \_ ->
                parse "45°7'30.00\"N 45°7'30.00\"E rubish"
                    |> E.equal Nothing
        , test "parsePair" <|
            \_ ->
                parsePair "45.125, 45.125"
                    |> E.equal (Just <| fromLatLngDegrees 45.125 45.125)
        , test "parsePair (fail)" <|
            \_ ->
                parsePair "45.125, 45.125 rubish"
                    |> E.equal Nothing
        , test "parseCoord" <|
            \_ ->
                parseCoord ( "N", "S" ) "45°7'30.00\"N"
                    |> E.equal (Just (Coord 1 45 7 30 0))
        , test "parseCoord (leading zero minutes)" <|
            \_ ->
                parseCoord ( "N", "S" ) "45°7'01.00\"N"
                    |> E.equal (Just (Coord 1 45 7 1 0))
        , test "parseCoord (origin)" <|
            \_ ->
                parseCoord ( "N", "S" ) "0°0'00.00\"N"
                    |> E.equal (Just (Coord 1 0 0 0 0))
        , test "parseCoord (fail)" <|
            \_ ->
                parseCoord ( "N", "S" ) "45°7'30.00\"N rubish"
                    |> E.equal Nothing
        ]


encoding : Test
encoding =
    describe "Decode/encode"
        [ fuzz2 (floatRange -90 90) (floatRange -180 180) "encode/decode roundtrip" <|
            \lat lng ->
                let
                    loc =
                        fromLatLngDegrees lat lng

                    locStr =
                        toString loc
                in
                case parse locStr of
                    Just loc_ ->
                        within (E.Absolute 0.01) loc loc_
                            |> E.onFail (Debug.toString loc ++ "\n" ++ Debug.toString loc_)

                    Nothing ->
                        E.fail ("failed to parse: " ++ locStr)
        , test "decodeTuple" <|
            \_ ->
                D.decodeString decodeTuple "[45.125, 45.125]"
                    |> E.equal (Ok (fromLatLngDegrees 45.125 45.125))
        , test "decodePair" <|
            \_ ->
                D.decodeString decodePair "\"45.125, -45.125\""
                    |> E.equal (Ok (fromLatLngDegrees 45.125 -45.125))
        , test "decodePretty" <|
            \_ ->
                D.decodeString decodePretty "\"45°7'30.00\\\"N 45°7'30.00\\\"E\""
                    |> E.equal (Ok (fromLatLngDegrees 45.125 45.125))
        ]


distance2d : ( Float, Float ) -> ( Float, Float ) -> Float
distance2d ( x, y ) ( a, b ) =
    sqrt ((x - a) ^ 2 + (y - b) ^ 2)


within : E.FloatingPointTolerance -> LatLng -> LatLng -> E.Expectation
within tol loc1 loc2 =
    let
        ( x1, y1 ) =
            toTupleDegrees loc1

        ( x2, y2 ) =
            toTupleDegrees loc2
    in
    E.all
        [ \_ -> E.within tol x1 x2
        , \_ -> E.within tol y1 y2
        ]
        ()
