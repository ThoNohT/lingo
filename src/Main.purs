module Main (main) where

import Prelude
import Affjax (get, printError) as Affjax
import Affjax.ResponseFormat (string) as ResponseFormat
import Control.Monad.State as ST
import Core (ignoreM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Game as G
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (text) as HH
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

data State
  = Loading
  | Loaded G.State
  | Error String

data Action
  = Init
  | KeyPressed KeyboardEvent

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction action = do
  s <- ST.get
  case s of
    Loading -> case action of
      Init -> do
        document <- liftEffect (map HTMLDocument.toEventTarget $ Web.document =<< Web.window)
        ignoreM $ H.subscribe $ eventListener KET.keydown document (map KeyPressed <<< KE.fromEvent)
        dict <- liftAff $ Affjax.get ResponseFormat.string "dict/english.txt"
        case dict of
          Left error -> ST.put $ Error $ Affjax.printError error
          Right response -> do
            gameState <- liftEffect $ G.initialState response.body
            ST.put $ Loaded gameState
      _ -> pure unit
    Loaded gameState -> case action of
      KeyPressed ev -> do
        let
          key = KE.key ev
        if key /= "F5" && not (KE.ctrlKey ev) && not (KE.altKey ev) then do
          liftEffect $ E.preventDefault (KE.toEvent ev)
          gameState' <- liftEffect $ G.handleKey (KE.key ev) gameState
          ST.put $ Loaded gameState'
        else
          pure unit
      _ -> pure unit
    Error _ -> pure unit

render :: forall m. State -> H.ComponentHTML Action Slots m
render st = case st of
  Loading -> HH.text ""
  Loaded s -> G.render s
  Error e -> HH.text e

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: const Loading
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    comp <- pure component
    runUI comp unit body
