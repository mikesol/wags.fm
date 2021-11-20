module Verbs where

import Prelude

import Data.Variant (Variant)
import Type.Function (type ($))
import Type.Row (type (+))
import Nouns as N

type ChoosePlaylist r = (choosePlaylist :: N.Playlist | r)
type HandleEditorInput r = (input :: N.EditorInput | r)
type HandleEditorOutput r = (handleEditorOutput :: EditorOutput | r)
type HandlePlayerInput r = (input :: N.PlayerInput | r)
type HandlePlayerOutput r = (handlePlayerOutput :: PlayerOutput | r)
type HidePlayer r = (hidePlayer :: Unit | r)
type PressPlay r = (pressPlay :: Unit | r)
type PressStop r = (pressStop :: Unit | r)
type ShowPlayer r = (showPlayer :: Unit | r)

----
type EditorAction = Variant
  $ HandleEditorInput
  + ShowPlayer ()

type EditorOutput = Variant
  $ ShowPlayer
  + ()

type MainAction = Variant
  $ HandlePlayerOutput
  + HandleEditorOutput
  + ()

type PlayerAction = Variant
  $ PressPlay
  + PressStop
  + ChoosePlaylist
  + HandlePlayerInput
  + HidePlayer
  + ()

type PlayerOutput = Variant
  $ PressPlay
  + PressStop
  + ChoosePlaylist
  + HidePlayer
  + ()