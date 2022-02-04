module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

type StaticHTML = H.ComponentHTML Unit () Aff

{- Create a Halogen component from an Elm app. -}
component :: forall query input output m. Effect (H.Component query input output m)
component = do
    pure $ H.mkComponent
        { initialState: const 0
        , render: const $ HH.div_ [ HH.text "Hello, world!" ]
        , eval: H.mkEval $ H.defaultEval
        }

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  comp <- H.liftEffect $ component
  runUI comp unit body
