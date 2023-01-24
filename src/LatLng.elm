module LatLng exposing
    ( Coord
    , LatLng
    , Path
    , Polyline
    , by
    , center
    , chain
    , coord
    , coordToString
    , decoder
    , distance
    , eastBy
    , eastByCoords
    , encoder
    , fromLatLon
    , fromLatLonDegrees
    , googleMapsUrl
    , latitude
    , longitude
    , move
    , moveEast
    , moveNorth
    , moveSouth
    , moveWest
    , northByCoords
    , parse
    , pathFrom
    , pathFromCenter
    , pathFromStart
    , southBy
    , southByCoords
    , toAngle
    , toString
    , westBy
    , westByCoords
    )

import Angle
import Json.Decode as D
import Json.Encode as E
import Length
import Maybe.Extra
import Parser as P exposing ((|.), (|=), Parser)
import Quantity as Q
import Round


type alias Length =
    Length.Length


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


encoder : LatLng -> E.Value
encoder loc =
    E.string (toString loc)


decoder : D.Decoder LatLng
decoder =
    D.string
        |> D.andThen
            (\s ->
                parse s
                    |> Maybe.Extra.unwrap (D.fail "invalid location") D.succeed
            )



-------------------------------------------------------------------------------
--- Basic properties
-------------------------------------------------------------------------------


{-| Create location from lat/lng values
-}
fromLatLon : Angle -> Angle -> LatLng
fromLatLon lat lng =
    LatLng { lat = lat, lng = lng }


{-| Create location from lat/lng values
-}
fromLatLonDegrees : Float -> Float -> LatLng
fromLatLonDegrees lat lng =
    fromLatLon (Angle.degrees lat) (Angle.degrees lng)


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


{-| Format a decimal latitude or longitude coordinate

    coordToString -45.5 "N" "S" ==> "45°30'00.00\"S"

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
    s deg ++ "°" ++ s min ++ "'" ++ sec__ ++ "\"" ++ select (sign == 1) pos neg


{-| Parse a pretty string of Lat/Lon data
-}
parse : String -> Maybe LatLng
parse st =
    st
        |> P.run
            (P.succeed fromLatLon
                |= P.map toAngle (coordP "N" "S")
                |. P.spaces
                |= P.map toAngle (coordP "E" "W")
            )
        |> Result.toMaybe


coordP : String -> String -> Parser Coord
coordP pos neg =
    let
        opt x st =
            P.succeed x |. P.token st

        digit =
            P.oneOf (List.map (\x -> opt x (String.fromInt x)) (List.range 0 9))
    in
    P.succeed (\d m s frac1 frac2 mul -> Coord mul d m s (100 * frac1 + 10 * frac2))
        |= P.int
        |. P.token "°"
        |= P.int
        |. P.token "'"
        |= P.int
        |. P.token "."
        |= digit
        |= digit
        |. P.token "\""
        |= P.oneOf [ opt 1 pos, opt -1 neg ]
        |. P.end



-------------------------------------------------------------------------------
--- Moving around
-------------------------------------------------------------------------------


by : Angle -> Angle -> LatLng -> LatLng
by x y (LatLng { lat, lng }) =
    LatLng { lat = Q.sum [ lat, x ], lng = Q.sum [ lng, y ] }


northBy : Angle -> LatLng -> LatLng
northBy x (LatLng { lat, lng }) =
    LatLng { lat = Q.sum [ lat, x ], lng = lng }


southBy : Angle -> LatLng -> LatLng
southBy x =
    northBy (Q.negate x)


eastBy : Angle -> LatLng -> LatLng
eastBy x (LatLng { lat, lng }) =
    LatLng { lat = lat, lng = Q.sum [ lng, x ] }


westBy : Angle -> LatLng -> LatLng
westBy x =
    eastBy (Q.negate x)


northByCoords : Int -> Int -> Int -> Int -> LatLng -> LatLng
northByCoords =
    byCoord northBy


southByCoords : Int -> Int -> Int -> Int -> LatLng -> LatLng
southByCoords =
    byCoord southBy


westByCoords : Int -> Int -> Int -> Int -> LatLng -> LatLng
westByCoords =
    byCoord westBy


eastByCoords : Int -> Int -> Int -> Int -> LatLng -> LatLng
eastByCoords =
    byCoord eastBy


move : Length -> Length -> LatLng -> LatLng
move x y loc =
    by
        (Angle.degrees (360 * Length.inMeters x / earthCircumference))
        (Angle.degrees (360 * Length.inMeters y / earthCircumferenceAt (latitude loc)))
        loc


moveNorth : Length -> LatLng -> LatLng
moveNorth x =
    move x (Length.meters 0)


moveSouth : Length -> LatLng -> LatLng
moveSouth x =
    moveNorth (Q.negate x)


moveEast : Length -> LatLng -> LatLng
moveEast x =
    move (Length.meters 0) x


moveWest : Length -> LatLng -> LatLng
moveWest x =
    moveEast (Q.negate x)


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


pathFromStart : List LatLng -> Path
pathFromStart poly =
    List.head poly
        |> Maybe.map (\x -> pathFrom x poly)
        |> Maybe.withDefault []


pathFromCenter : Polyline -> Path
pathFromCenter poly =
    pathFrom (center poly) poly


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
