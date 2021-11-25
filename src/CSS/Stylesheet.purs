module CSS.MyStyles where

import CSS.Hack.Animation (animation)
import CSS (CSS, TimingFunction(..), display, displayNone, fromString, left, ms, pct, sec)
import CSS.Hack.Animation (AnimationPlayState(..), animation, forwards, infinite, iterationCount, normalAnimationDirection)

spin :: CSS
spin = animation
  (fromString "spin")
  (sec 1.2)
  Linear
  (sec 0.0)
  infinite
  normalAnimationDirection
  forwards
  ARunning