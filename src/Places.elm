module Places exposing
    ( Country, br
    , origin
    )

{-|


# A database of places and their coordinates

A non-comprehensive database of locations of real places.


## Countries

@docs Country, br


## References

@docs origin

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
    { center = fromLatLngDegrees -15.676745 -47.678328
    , north = fromLatLngDegrees 4.443417 -51.519399
    , south = fromLatLngDegrees -33.751085 -53.394833
    , east = fromLatLngDegrees -7.155056 -34.793143
    , west = fromLatLngDegrees -7.534318 -73.982792
    , capital = fromLatLngDegrees -15.790673 -47.892816
    }


{-| Coordiante 0 0
-}
origin : LatLng
origin =
    fromLatLngDegrees 0 0
