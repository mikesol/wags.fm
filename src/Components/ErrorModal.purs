module Components.ErrorModal where

import Prelude

import Data.Maybe (maybe)
import Data.Variant (inj, match)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Type.Proxy (Proxy(..))
import Types as T
import Util (classes, classesS)

component :: forall q m. H.Component q T.ModalInput T.ModalOutput m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  initialState
    { text
    , title
    , code
    } =
    { text
    , title
    , code
    }

  render { text, title, code } = HH.div [ classesS "fixed z-10 inset-0 overflow-y-auto", ARIA.labelledBy "modal-title", ARIA.modal "true" ]
    [ HH.div [ classesS "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0" ]
        [ HH.div [ classesS "fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity", ARIA.hidden "true" ] []
        , HH.span [ classesS "hidden sm:inline-block sm:align-middle sm:h-screen", ARIA.hidden "true" ] [ HH.text "​" ]
        , HH.div [ classesS "inline-block align-bottom bg-white rounded-lg text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full" ]
            [ HH.div [ classesS "bg-white px-4 pt-5 pb-4 sm:p-6 sm:pb-4" ]
                [ HH.div [ classesS "sm:flex sm:items-start" ]
                    [ HH.div [ classesS "mt-3 sm:mt-0 sm:ml-4 sm:text-left" ]
                        [ HH.h3 [ classesS "text-lg text-center leading-6 font-medium text-gray-900", HP.id "modal-title" ]
                            [ HH.text title
                            ]
                        , HH.div [ classesS "mt-2" ]
                            ( [ HH.p [ classesS "text-sm text-gray-500" ]
                                  [ HH.text text
                                  ]
                              ] <>
                                ( code #
                                    maybe [] \cd ->
                                      [ HH.pre []
                                          [ HH.code [ classes [ "pt-6", "block", "text-sm", "whitespace-pre" ] ] [ HH.text cd ] ]
                                      ]
                                )
                            )
                        ]
                    ]
                ]
            , HH.div [ classesS "bg-gray-50 px-4 py-3 sm:px-6 sm:flex sm:flex-row-reverse" ]
                [ HH.button
                    [ HE.onClick (const $ inj (Proxy :: _ "close") unit)
                    , HP.type_ ButtonSubmit
                    , classesS "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-red-600 text-base font-medium text-white hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500 sm:ml-3 sm:w-auto sm:text-sm"
                    ]
                    [ HH.text "Close"
                    ]
                ]
            ]
        ]
    ]

  handleAction
    :: T.ModalAction
    -> HalogenM T.ModalState T.ModalAction () T.ModalOutput m Unit
  handleAction = match
    { close: \_ ->
        H.raise (inj (Proxy :: _ "closeMe") unit)
    }
