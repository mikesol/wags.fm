module WAGS.FM.Types where

import WAGS.Lib.Tidal (AFuture)

type Playlist = Array { code :: String, wag :: AFuture, duration :: Number }