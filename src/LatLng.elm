module LatLng exposing
    ( LatLng, Coord, Polyline, Path
    , fromLatLng, fromLatLngDegrees
    , parse, parsePair, toString, coordToString, googleMapsUrl
    , latitude, longitude, coord, distance, toAngle, toTupleDegrees
    , by, eastBy, westBy, northBy, southBy
    , move, moveEast, moveWest, moveNorth, moveSouth
    , eastByCoords, westByCoords, northByCoords, southByCoords
    , center, chain, pathFrom, pathFromCenter, pathFromStart
    , encoder, decoder, decodePair, decodePretty, decodeTuple
    , Angle, Length, parseCoord
    )

{-|


# Location objects

This module provides a basic data structure to represent locations as a pair of latitude/longitude
variables.


## Types

@docs LatLng, Coord, Polyline, Path, Angle, Length


## Creation

@docs fromLatLng, fromLatLngDegrees


## Parsing and rendering

@docs parse, parsePair, parseCoord, toString, coordToString, googleMapsUrl


## Properties and comparison

@docs latitude, longitude, coord, distance, toAngle, toTupleDegrees


## Moving points around

We can move points specifying variations in lat/lon Angles, Meters or in Coords


### By angle

@docs by, eastBy, westBy, northBy, southBy


### By length

@docs move, moveEast, moveWest, moveNorth, moveSouth


### By coord

@docs eastByCoords, westByCoords, northByCoords, southByCoords


## Paths and Polylines

@docs center, chain, pathFrom, pathFromCenter, pathFromStart


## Encoding/Decoding

@docs encoder, decoder, decodePair, decodePretty, decodeTuple

-}

import Angle
import Json.Decode as D
import Json.Encode as E
import Length
import Maybe.Extra as Maybe
import Parser as P exposing ((|.), (|=), Parser)
import Parser.Advanced as PA
import Quantity as Q
import Round


{-| Re-expose Length from elm-units
-}
type alias Length =
    Length.Length


{-| Re-expose Angle from elm-units
-}
type alias Angle =
    Angle.Angle


{-| A Point with latitude/longitude coordinates
-}
type LatLng
    = LatLng { lat : Angle, lng : Angle }


{-| A list of LatLng points
-}
type alias Polyline =
    List LatLng


{-| A list of (x, y) coordinates
-}
type alias Path =
    List ( Length, Length )


{-| Split coordinate component parts
Sign is either -1 or 1 and all other components are positive
-}
type alias Coord =
    { sign : Int
    , deg : Int
    , min : Int
    , sec : Int
    , ms : Int
    }



-------------------------------------------------------------------------------
--- Enconders/Decoders
-------------------------------------------------------------------------------


{-| Encode LatLng as a JSON string
-}
encoder : LatLng -> E.Value
encoder loc =
    E.string (toString loc)


{-| Decode LatLng from a known format
-}
decoder : D.Decoder LatLng
decoder =
    D.oneOf
        [ decodePretty
        , decodePair
        ]


{-| Decode LatLng from a prettified representation in a JSON string
-}
decodePretty : D.Decoder LatLng
decodePretty =
    D.string
        |> D.andThen
            (\s ->
                parse s
                    |> Maybe.unwrap (D.fail "invalid lat/lon location") D.succeed
            )


{-| Decode location from a string with a pair of floats
-}
decodePair : D.Decoder LatLng
decodePair =
    D.string
        |> D.andThen
            (\s ->
                parsePair s
                    |> Maybe.unwrap (D.fail "invalid coordinate pair") D.succeed
            )


{-| Decode location from a string with a pair of floats
-}
decodeTuple : D.Decoder LatLng
decodeTuple =
    D.list D.float
        |> D.andThen
            (\lst ->
                case lst of
                    [ x, y ] ->
                        D.succeed (fromLatLngDegrees x y)

                    _ ->
                        D.fail "invalid pair of coordinates"
            )



-------------------------------------------------------------------------------
--- Basic properties
-------------------------------------------------------------------------------


{-| Create location from lat/lng values
-}
fromLatLng : Angle -> Angle -> LatLng
fromLatLng lat lng =
    LatLng { lat = lat, lng = lng }


{-| Create location from lat/lng values
-}
fromLatLngDegrees : Float -> Float -> LatLng
fromLatLngDegrees lat lng =
    fromLatLng (Angle.degrees lat) (Angle.degrees lng)


{-| Return location\`s latitude
-}
latitude : LatLng -> Angle
latitude (LatLng { lat }) =
    lat


{-| Return location\`s longitude
-}
longitude : LatLng -> Angle
longitude (LatLng { lng }) =
    lng


{-| Render lat/lng as a pretty printed string
-}
toString : LatLng -> String
toString (LatLng { lat, lng }) =
    coordToString lat "N" "S" ++ " " ++ coordToString lng "E" "W"


{-| Convert LatLng to a tuple of floats
-}
toTupleDegrees : LatLng -> ( Float, Float )
toTupleDegrees loc =
    ( latitude loc |> Angle.inDegrees, longitude loc |> Angle.inDegrees )


{-| Format a decimal latitude or longitude coordinate

    coordToString -45.5 "N" "S" ==> "45??30'00.00\"S"

-}
coordToString : Angle -> String -> String -> String
coordToString x pos neg =
    let
        { sign, deg, min, sec, ms } =
            coord x

        s =
            String.fromInt

        sec_ =
            Round.round 2 (toFloat sec + (toFloat ms / 1000))

        sec__ =
            select (String.startsWith "." (String.dropLeft 1 sec_)) ("0" ++ sec_) sec_
    in
    s deg ++ "??" ++ s min ++ "'" ++ sec__ ++ "\"" ++ select (sign == 1) pos neg


{-| Parse a pretty string of Lat/Lon data
-}
parse : String -> Maybe LatLng
parse st =
    st
        |> P.run
            (P.succeed fromLatLng
                |= P.map toAngle (coordParser "N" "S")
                |. P.spaces
                |= P.map toAngle (coordParser "E" "W")
                |. P.end
            )
        |> Result.toMaybe


{-| Convert string with a pair of floats to a LatLng pair
-}
parsePair : String -> Maybe LatLng
parsePair st =
    let
        signedFloat =
            P.oneOf
                [ P.succeed negate
                    |. P.symbol "-"
                    |= P.float
                , P.float
                ]
    in
    st
        |> P.run
            (P.succeed fromLatLngDegrees
                |= signedFloat
                |. P.spaces
                |. P.token ","
                |. P.spaces
                |= signedFloat
                |. P.end
            )
        |> Result.toMaybe


{-| Parse a single coordinate with the given pair of directions

This can be used to specify the N-S or E-W directions or localized
versions of those symbols

Example
parseCoord ("L", "O") "45??10'50.12L"
==> Coord 1 45 10 50 120

-}
parseCoord : ( String, String ) -> String -> Maybe Coord
parseCoord ( a, b ) =
    P.run (coordParser a b |. P.end) >> Result.toMaybe


coordParser : String -> String -> Parser Coord
coordParser pos neg =
    let
        opt x st =
            P.succeed x |. P.symbol st
    in
    P.succeed
        (\deg min secMs sign ->
            let
                sec =
                    truncate secMs

                ms =
                    (secMs - toFloat sec) * 1000 |> truncate
            in
            Coord sign deg min sec ms
        )
        |= P.int
        |. P.symbol "??"
        |= P.int
        |. P.symbol "'"
        |= P.oneOf
            [ P.symbol "0" |> P.andThen (\_ -> P.float)
            , P.float
            ]
        |. P.symbol "\""
        |= P.oneOf [ opt 1 pos, opt -1 neg ]



-------------------------------------------------------------------------------
--- Moving around
-------------------------------------------------------------------------------


{-| Move location by given lat and lng angles
-}
by : Angle -> Angle -> LatLng -> LatLng
by x y (LatLng { lat, lng }) =
    LatLng { lat = Q.sum [ lat, x ], lng = Q.sum [ lng, y ] }


{-| Move point north by given latitude angle
-}
northBy : Angle -> LatLng -> LatLng
northBy x (LatLng { lat, lng }) =
    LatLng { lat = Q.sum [ lat, x ], lng = lng }


{-| Move point south by given latitude angle
-}
southBy : Angle -> LatLng -> LatLng
southBy x =
    northBy (Q.negate x)


{-| Move point east by given longitude angle
-}
eastBy : Angle -> LatLng -> LatLng
eastBy x (LatLng { lat, lng }) =
    LatLng { lat = lat, lng = Q.sum [ lng, x ] }


{-| Move point west by given longitude angle
-}
westBy : Angle -> LatLng -> LatLng
westBy x =
    eastBy (Q.negate x)


{-| Move point south by given latitude cooordinates (in degrees, minutes, seconds, milliseconds)
-}
northByCoords : Int -> Int -> Int -> Int -> LatLng -> LatLng
northByCoords =
    byCoord northBy


{-| Move point south by given latitude cooordinates (in degrees, minutes, seconds, milliseconds)
-}
southByCoords : Int -> Int -> Int -> Int -> LatLng -> LatLng
southByCoords =
    byCoord southBy


{-| Move point west by given longitude cooordinates (in degrees, minutes, seconds, milliseconds)
-}
westByCoords : Int -> Int -> Int -> Int -> LatLng -> LatLng
westByCoords =
    byCoord westBy


{-| Move point east by given longitude cooordinates (in degrees, minutes, seconds, milliseconds)
-}
eastByCoords : Int -> Int -> Int -> Int -> LatLng -> LatLng
eastByCoords =
    byCoord eastBy


{-| Move point by (x, y) distance
-}
move : Length -> Length -> LatLng -> LatLng
move x y loc =
    by
        (Angle.degrees (360 * Length.inMeters x / earthCircumference))
        (Angle.degrees (360 * Length.inMeters y / earthCircumferenceAt (latitude loc)))
        loc


{-| Move point north by given distance
-}
moveNorth : Length -> LatLng -> LatLng
moveNorth x =
    move x zero


{-| Move point south by given distance
-}
moveSouth : Length -> LatLng -> LatLng
moveSouth x =
    moveNorth (Q.negate x)


{-| Move point east by given distance
-}
moveEast : Length -> LatLng -> LatLng
moveEast x =
    move zero x


{-| Move point west by given distance
-}
moveWest : Length -> LatLng -> LatLng
moveWest x =
    moveEast (Q.negate x)


{-| Make a chain of transformations in the given point

Each transformation is typically an operation like `moveNorth (meters 5)`

Return a list of partial applications.

Example:

    chain loc
        [ move (meters 60) (meters 10)
        , northByCoords 0 0 1 256
        , westBy (Angle.degrees 180)
        ]

-}
chain : LatLng -> List (LatLng -> LatLng) -> List LatLng
chain start =
    List.drop 1
        << List.reverse
        << List.foldl
            (\step acc ->
                (List.head acc |> Maybe.withDefault start |> step) :: acc
            )
            [ start ]



-------------------------------------------------------------------------------
--- Conversions
-------------------------------------------------------------------------------


{-| Extract coordinate parts from float
-}
coord : Angle -> Coord
coord x =
    let
        split mult value =
            let
                int =
                    truncate value

                rem =
                    value - toFloat int
            in
            ( int, mult * rem )

        sign =
            select (Angle.inDegrees x >= 0) 1 -1

        ( deg, min_ ) =
            split 60 (abs (Angle.inDegrees x))

        ( min, sec_ ) =
            split 60 min_

        ( sec, ms_ ) =
            split 1000 sec_

        ms =
            truncate ms_
    in
    Coord sign deg min sec ms


{-| Converts Coord to float
-}
toAngle : Coord -> Angle
toAngle { sign, deg, min, sec, ms } =
    let
        f =
            toFloat
    in
    Angle.degrees (f sign * (f deg + f min / 60 + f sec / 3600 + f ms / 3600000))


{-| Google Maps URL associated with the given location
-}
googleMapsUrl : LatLng -> String
googleMapsUrl (LatLng { lat, lng }) =
    let
        fmt =
            Angle.inDegrees >> Round.round 6
    in
    "https://www.google.com/maps/search/?api=1&query=" ++ fmt lat ++ "," ++ fmt lng



-------------------------------------------------------------------------------
--- Mathematical operations
-------------------------------------------------------------------------------


{-| Distance between two points, in meters.

Uses the great circle distance (<https://en.wikipedia.org/wiki/Great-circle_distance>)

This approximates the Earth by a sphere, which in most occasions is good enough.

-}
distance : LatLng -> LatLng -> Length
distance (LatLng p1) (LatLng p2) =
    let
        ( phi1, phi2 ) =
            ( p1.lat, p2.lat )

        dlambd =
            Q.difference p1.lng p2.lng

        numer =
            sqrt ((Angle.cos phi2 * Angle.sin dlambd) ^ 2 + (Angle.cos phi1 * Angle.sin phi2 - Angle.sin phi1 * Angle.cos phi2 * Angle.cos dlambd) ^ 2)

        denom =
            Angle.sin phi1 * Angle.sin phi2 + Angle.cos phi1 * Angle.cos phi2 * Angle.cos dlambd
    in
    Length.meters (abs (earthRadius * atan2 numer denom))


{-| Create linear path starting at the given LatLng point

The conversion assumes the earth is locally flat, i.e., it gives
good results for nearby points and poor conversions if distances
are comensuarable to Earth radius.

-}
pathFrom : LatLng -> Polyline -> Path
pathFrom (LatLng { lat, lng }) =
    let
        toCoord : LatLng -> ( Length, Length )
        toCoord (LatLng pt) =
            ( Length.meters <| earthCircumferenceAt lat * (Q.difference pt.lng lng |> Angle.inTurns)
            , Length.meters <| earthCircumference * (Q.difference pt.lat lat |> Angle.inTurns)
            )
    in
    List.map toCoord


{-| Convert Polyline into path

Works like pathFrom, but uses the first element of list as the origin reference

-}
pathFromStart : Polyline -> Path
pathFromStart poly =
    List.head poly
        |> Maybe.map (\x -> pathFrom x poly)
        |> Maybe.withDefault []


{-| Convert Polyline into path, taking the center point as the origin.
-}
pathFromCenter : Polyline -> Path
pathFromCenter poly =
    pathFrom (center poly) poly


{-| Computes the center point of the polyline
-}
center : Polyline -> LatLng
center poly =
    let
        reducer (LatLng { lat, lng }) ( i, x, y ) =
            ( i + 1, Q.sum [ lat, x ], Q.sum [ lng, y ] )

        ( n, accX, accY ) =
            List.foldl reducer ( 0, Angle.degrees 0.0, Angle.degrees 0 ) poly
    in
    LatLng { lat = accX |> Q.divideBy n, lng = accY |> Q.divideBy n }



-------------------------------------------------------------------------------
--- Internal functions
-------------------------------------------------------------------------------


byCoord : (Angle -> a) -> Int -> Int -> Int -> Int -> a
byCoord f =
    \deg min sec ms -> f (toAngle (Coord 1 deg min sec ms))


zero : Length.Length
zero =
    Length.meters 0


earthRadius : Float
earthRadius =
    6371009


earthCircumference : Float
earthCircumference =
    2 * pi * earthRadius


earthCircumferenceAt : Angle -> Float
earthCircumferenceAt lat =
    earthCircumference * abs (Angle.cos lat)


select : Bool -> c -> c -> c
select a b c =
    if a then
        b

    else
        c
