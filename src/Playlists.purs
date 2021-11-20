module Playlists where

import Nouns as N
import Playlists.Java as Java
import Playlists.LoFi as LoFi

playlists :: Array N.Playlist
playlists =
  [ Java.playlist
  , LoFi.playlist
  ]