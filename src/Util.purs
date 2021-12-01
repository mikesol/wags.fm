module Util where

import Prelude

import Data.Array (intercalate)
import Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.String as String
import Halogen as H
import Halogen.HTML.Properties as HP

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

classesS :: forall r p. String -> HP.IProp (class :: String | r) p
classesS = classes <<< String.split (String.Pattern " ")

nelmod :: forall a. NEL.NonEmptyList a -> Int -> a
nelmod nel@(NEL.NonEmptyList (h :| _)) i = fromMaybe h $ NEL.index nel (i `mod` NEL.length nel)

-- we remove 0-width spaces, which come from web examples
sanitizePS :: String -> String
sanitizePS = String.replaceAll (String.Pattern "​") (String.Replacement "")
  <<< intercalate "\n"
  <<< map
    ( (if _ then _ else _)
        <$> (eq "module " <<< String.take 7)
        <*> (const "module Main where")
        <*> identity
    )
  <<< String.split (String.Pattern "\n")