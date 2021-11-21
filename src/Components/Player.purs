module Components.Player where

import Prelude

import CSS (TimingFunction(..), animation, backgroundImage, forwards, fromString, iterationCount, normalAnimationDirection, sec, url)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Variant (Variant, inj, match)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Playlists as Playlists
import SVGIcons as SVGIcons
import Svg.Renderer.Halogen (icon)
import Type.Proxy (Proxy(..))
import Util (classes)
import Types as T

type State = {}

bgStyling =
  [ "bg-gray-200"
  , "bg-opacity-50"
  ] :: Array String

foreign import background :: String

choosePlaylist
  :: forall r
   . T.Playlist
  -> Variant (choosePlaylist :: T.Playlist | r)
choosePlaylist = inj (Proxy :: _ "choosePlaylist")

pressPlay
  :: forall r
   . Variant (pressPlay :: Unit | r)
pressPlay = inj (Proxy :: _ "pressPlay") unit

hidePlayer
  :: forall r
   . Variant (hidePlayer :: Unit | r)
hidePlayer = inj (Proxy :: _ "hidePlayer") unit

pressStop
  :: forall r
   . Variant (pressStop :: Unit | r)
pressStop = inj (Proxy :: _ "pressStop") unit

component :: forall q m. MonadEffect m => H.Component q T.PlayerInput T.PlayerOutput m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< inj (Proxy :: _ "input")
        }
    }
  where
  initialState { playlist, isPlaying, isHidden } =
    { playlist
    , isPlaying
    , isHidden
    , hasPlayedOnce: false
    , hasHiddenOnce: false
    }

  render
    { playlist
    , isPlaying
    , isHidden
    , hasPlayedOnce
    , hasHiddenOnce
    } =
    HH.div
      [ classes
          [ "w-full"
          , "h-full"
          , "absolute"
          , "bg-center"
          , "bg-no-repeat"
          , "bg-cover"
          , "grid"
          , "grid-rows-3"
          , "grid-cols-3"
          ]
      , CSS.style do
          backgroundImage (url background)
          when hasHiddenOnce $ animation
            (fromString $ if isHidden then "flyUp" else "flyDown")
            (sec 0.6)
            EaseInOut
            (sec 0.0)
            (iterationCount 1.0)
            normalAnimationDirection
            forwards
      ]
      [ HH.div
          [ classes
              [ "row-start-1"
              , "row-end-3"
              , "col-start-1"
              , "col-end-1"
              ]
          ]
          [ HH.h1
              [ classes $
                  [ "font-mono"
                  , "text-4xl"
                  , "p-3"
                  ] <> bgStyling
              ]
              [ HH.text "wags.fm" ]
          , HH.ul
              [ classes
                  $ [ "p-3" ] <> bgStyling
              ]
              ( Playlists.playlists <#> \p -> HH.li []
                  [ HH.a
                      [ HE.onClick
                          $ const
                          $ choosePlaylist p
                      , classes [ "underline", "cursor-pointer" ]
                      ]
                      [ HH.text p.title ]
                  ]
              )
          ]
      , HH.div
          [ classes
              [ "row-start-2"
              , "row-end-2"
              , "col-start-2"
              , "col-end-2"
              , "flex"
              , "flex-col"
              ]
          ]
          [ HH.div [ classes [ "flex-grow" ] ] []
          , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
              [ HH.div [ classes [ "flex-grow" ] ] []
              , HH.div [ classes [ "flex-grow-0" ] ]
                  [ HH.div [ classes $ [ "p-3", "text-center" ] <> bgStyling ]
                      [ HH.div
                          [ classes [ "cursor-pointer" ]
                          ]
                          [ icon
                              ( ( if isPlaying then SVGIcons.stop
                                  else SVGIcons.play
                                ) 50 50
                              )
                              [ HE.onClick
                                  $ const
                                  $ if isPlaying then pressStop else pressPlay
                              ]
                          ]
                      , HH.div [] [ HH.text playlist.title ]
                      ]
                  ]
              , HH.div [ classes [ "flex-grow" ] ] []
              ]
          , HH.div [ classes [ "flex-grow" ] ] []
          ]
      , HH.div
          [ classes
              [ "row-start-3"
              , "row-end-3"
              , "col-start-3"
              , "col-end-3"
              , "flex"
              , "flex-col"
              ]
          ]
          [ HH.div [ classes [ "flex-grow" ] ] []
          , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
              ( [ HH.div [ classes [ "flex-grow" ] ] []
                ] <> guard hasPlayedOnce
                  [ HH.div
                      [ classes [ "flex-grow-0" ]
                      , CSS.style do
                          animation
                            (fromString $ if isPlaying then "fadeIn" else "fadeOut")
                            (sec 0.5)
                            Linear
                            (sec 0.0)
                            (iterationCount 1.0)
                            normalAnimationDirection
                            forwards
                      ]
                      [ HH.a
                          [ classes $
                              [ "underline"
                              , "cursor-pointer"
                              , "p-3"
                              ] <> bgStyling
                          , HE.onClick $ const $ hidePlayer
                          ]
                          [ HH.text "Edit me" ]
                      ]
                  ]
              )
          ]
      ]

  handleAction
    :: T.PlayerAction
    -> HalogenM T.PlayerState T.PlayerAction () T.PlayerOutput m Unit
  handleAction = match
    { pressPlay: const $ do
        H.raise pressPlay
    , pressStop: const $ do
        H.raise pressStop
    , hidePlayer: const $ do
        H.raise hidePlayer
    , choosePlaylist: \playlist -> do
        Log.info "choose playlist"
        H.raise (choosePlaylist playlist)
    , input: \{ playlist, isPlaying, isHidden } -> do
        H.modify_ $ \i -> i
          { playlist = playlist
          , isPlaying = isPlaying
          , isHidden = isHidden
          , hasPlayedOnce = i.hasPlayedOnce || isPlaying
          , hasHiddenOnce = i.hasHiddenOnce || isHidden
          }
    }
