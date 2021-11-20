module Util where

import Prelude

import Halogen as H
import Halogen.HTML.Properties as HP

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName