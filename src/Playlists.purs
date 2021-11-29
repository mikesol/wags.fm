module Playlists where

import Types as T
import Playlists.Java as Java
import Playlists.LoFi as LoFi
import Playlists.Dots as Dots
import Playlists.Blank as Blank

playlists :: Array T.Playlist
playlists =
  [ Java.playlist
  , LoFi.playlist
  , Dots.playlist
  , Blank.playlist
  ]