module Playlists where

import Types as T
import Playlists.Java as Java
import Playlists.LoFi as LoFi

playlists :: Array T.Playlist
playlists =
  [ Java.playlist
  , LoFi.playlist
  ]