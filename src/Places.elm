module Places exposing (..)

import LatLng exposing (..)


type alias Country =
    { center : LatLng
    , north : LatLng
    , south : LatLng
    , east : LatLng
    , west : LatLng
    , capital : LatLng
    }


br : Country
br =
    { center = fromLatLonDegrees 10 10
    , north = fromLatLonDegrees 10 10
    , south = fromLatLonDegrees 10 10
    , east = fromLatLonDegrees 10 10
    , west = fromLatLonDegrees 10 10
    , capital = fromLatLonDegrees 10 10
    }
