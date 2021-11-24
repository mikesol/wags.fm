module Components.Player where

import Prelude

import CSS (TimingFunction(..), opacity, animation, backgroundImage, forwards, fromString, iterationCount, normalAnimationDirection, sec, url)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Variant (Variant, inj, match)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
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
   . { transitionTime :: Number } ->  Variant (hidePlayer :: { transitionTime :: Number } | r)
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
    , hasPlayedOnce: false
    , hasHiddenOnce: false
    }

  render
    { playlist
    , isPlaying
    , hiddenInstr
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
          , "z-10"
          ]
      , CSS.style do
          backgroundImage (url background)
          when hasHiddenOnce $ animation
            (fromString $ if hiddenInstr.hidden then "flyUp" else "flyDown")
            (sec hiddenInstr.transitionTime)
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
                          opacity 0.0
                          animation
                            (fromString $ if isPlaying then "fadeIn" else "fadeOut")
                            (sec 0.5)
                            Linear
                            (sec $ if isPlaying then 0.0 else 1.0)
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
                          , HE.onClick $ const $ hidePlayer { transitionTime: 0.6 }
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
        H.liftAff $ delay (Milliseconds 500.0)
        { isPlaying, hiddenInstr: { hidden } } <- H.get
        when (isPlaying && not hidden) $ H.raise $ hidePlayer { transitionTime: 0.995 }
    , pressStop: const $ do
        H.raise pressStop
    , hidePlayer: const $ do
        H.raise $ hidePlayer { transitionTime: 0.6}
    , choosePlaylist: \playlist -> do
        -- Log.info "choose playlist"
        H.raise pressStop
        H.raise (choosePlaylist playlist)
    , input: \{ playlist, isPlaying, hiddenInstr } -> do
        H.modify_ $ \i -> i
          { playlist = playlist
          , isPlaying = isPlaying
          , hiddenInstr = hiddenInstr
          , hasPlayedOnce = i.hasPlayedOnce || isPlaying
          , hasHiddenOnce = i.hasHiddenOnce || hiddenInstr.hidden
          }
    }