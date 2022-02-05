module Main (main) where

import Prelude

import Control.Monad.State as ST
import Core (ignoreM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Game as G
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

-- Unused types.
type Input
  = Unit

type Output
  = Unit

type Slots :: forall k. Row k
type Slots
  = ()

data Action
  = Init
  | KeyPressed KeyboardEvent

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM G.State Action Slots Output m Unit
handleAction action = case action of
  Init -> do
    document <- liftEffect (map HTMLDocument.toEventTarget $ Web.document =<< Web.window)
    ignoreM $ H.subscribe $ eventListener KET.keydown document (map KeyPressed <<< KE.fromEvent)
  KeyPressed ev -> do
    liftEffect $ E.preventDefault (KE.toEvent ev)
    s <- ST.get
    s' <- liftEffect $ G.handleKey (KE.key ev) s
    ST.put s'

component :: forall query m. MonadEffect m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: const G.initialState
    , render: G.render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    comp <- pure component
    runUI comp unit body
