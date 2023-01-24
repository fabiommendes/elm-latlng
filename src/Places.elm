module Places exposing (Country, br)

{-|


# A database of places and their coordinates

A non-comprehensive database of locations of real places.


## Countries

@docs Country, br

-}

import LatLng exposing (..)


{-| Some important points in a Country
-}
type alias Country =
    { center : LatLng
    , north : LatLng
    , south : LatLng
    , east : LatLng
    , west : LatLng
    , capital : LatLng
    }


{-| Brazil
-}
br : Country
br =
    { center = fromLatLngDegrees 0 0
    , north = fromLatLngDegrees 0 0
    , south = fromLatLngDegrees 0 0
    , east = fromLatLngDegrees 0 0
    , west = fromLatLngDegrees 0 0
    , capital = fromLatLngDegrees 0 0
    }
