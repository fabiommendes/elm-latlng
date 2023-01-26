# Latitude and Longitude calculations

This module implement types to represent locations as a Latitude/Longitude pair. 

# Examples

We can create LatLng objects from coordinates and perform explicit manipulations. The modules tries to be as type safe as possible and uses units from Elm/Units?? to ensure consistency.

```elm
import LatLng as exposing (..)
import LatLng.Places as Places
import Length exposing kilometers  -- from ianmackenzie/elm-units

loc1 = Places.br.north
loc2 = Places.br.south
distance loc1 loc2 ==> (kilometers 5000)
```