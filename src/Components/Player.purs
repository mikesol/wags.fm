module Components.Player where

import Prelude

import CSS (TimingFunction(..), animation, forwards, fromString, iterationCount, normalAnimationDirection, sec)
import Data.Maybe (Maybe(..), isJust, maybe)
import CSS.MyStyles (spin)
import Data.Monoid (guard)
import Data.Variant (Variant, inj, match)
import Effect.Aff.Class (class MonadAff)
-- import Effect.Class.Console as Log
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Playlists as Playlists
import SVGIcons as SVGIcons
import Svg.Renderer.Halogen (icon)
import Type.Proxy (Proxy(..))
import Types as T
import Util (classes)

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
   . { transition :: { duration :: Number, offset :: Number } }
  -> Variant (hidePlayer :: { transition :: { duration :: Number, offset :: Number } } | r)
hidePlayer = inj (Proxy :: _ "hidePlayer")

pressStop
  :: forall r
   . Variant (pressStop :: Unit | r)
pressStop = inj (Proxy :: _ "pressStop") unit

component :: forall q m. MonadAff m => H.Component q T.PlayerInput T.PlayerOutput m
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
  initialState { playlist, isPlaying, hiddenInstr } =
    { playlist
    , isPlaying
    , hiddenInstr
    , hasSentEventsOnce: false
    , hasHiddenOnce: false
    }

  render
    { playlist
    , isPlaying
    , hiddenInstr
    , hasSentEventsOnce
    , hasHiddenOnce
    } =
    HH.div
      [ classes
          [ "w-full"
          , "h-full"
          , "absolute"
          , "z-10"
          , "bg-gradient-to-r"
          , "from-purple-400"
          , "via-pink-500"
          , "to-red-500"
          ]
      , CSS.style do
          -- backgroundImage (url background)
          when hasHiddenOnce $ animation
            (fromString $ if hiddenInstr.hidden then "flyUp" else "flyDown")
            (sec hiddenInstr.transition.duration)
            EaseInOut
            (sec hiddenInstr.transition.offset)
            (iterationCount 1.0)
            normalAnimationDirection
            forwards
      ]
      [ HH.div
          [ classes
              [ "w-full"
              , "h-full"
              , "grid"
              , "grid-rows-3"
              , "grid-cols-3"
              ]
          , CSS.style do
              animation
                (fromString "fadeIn")
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
                  , "col-end-3"
                  , "md:col-end-1"
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
                              [ classes [ "flex", "flex-row", "w-full" ]
                              ]
                              [ HH.div [ classes [ "flex-grow" ] ] []
                              , HH.div [ classes [ "flex-grow-0", "cursor-pointer" ] ]
                                  [ icon
                                      ( ( case isPlaying of
                                            Nothing -> SVGIcons.play
                                            Just { hasSentEvents: true } -> SVGIcons.stop
                                            Just { hasSentEvents: false } -> SVGIcons.spinner
                                        ) 50 50
                                      )
                                      ( ( case isPlaying of
                                            Nothing ->
                                              [ HE.onClick
                                                  $ const
                                                  $ pressPlay
                                              ]
                                            Just { hasSentEvents: true } ->
                                              [ HE.onClick
                                                  $ const
                                                  $ pressStop
                                              ]
                                            Just { hasSentEvents: false } -> []
                                        ) <>
                                          [ CSS.style do
                                              case isPlaying of
                                                Nothing -> pure unit
                                                Just { hasSentEvents: true } -> pure unit
                                                Just { hasSentEvents: false } -> spin
                                          ]
                                      )
                                  ]
                              , HH.div [ classes [ "flex-grow" ] ] []
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
                    ] <> guard hasSentEventsOnce
                      [ HH.div
                          [ classes [ "flex-grow-0" ]
                          , CSS.style do
                              -- opacity 0.0
                              animation
                                (fromString $ if maybe false _.hasSentEvents isPlaying then "fadeIn" else "fadeOut")
                                (sec 0.5)
                                Linear
                                (sec $ 0.0)
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
                              , HE.onClick $ const $ hidePlayer { transition: { duration: 0.6, offset: 0.0 } }
                              ]
                              [ HH.text "Edit me" ]
                          ]
                      ]
                  )
              ]
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
        H.raise $ hidePlayer { transition: { duration: 0.6, offset: 0.0 } }
    , choosePlaylist: \playlist -> do
        -- Log.info "choose playlist"
        H.raise pressStop
        H.raise (choosePlaylist playlist)
    , input: \{ playlist, isPlaying, hiddenInstr } -> do
        H.modify_ $ \i -> i
          { playlist = playlist
          , isPlaying = isPlaying
          , hiddenInstr = hiddenInstr
          , hasSentEventsOnce = i.hasSentEventsOnce || isPlaying == Just { hasSentEvents: true }
          , hasHiddenOnce = i.hasHiddenOnce || hiddenInstr.hidden
          }
    }
