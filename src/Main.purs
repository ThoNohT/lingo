module Main (main) where

import Prelude
import Control.Monad.State as ST
import Data.Char (fromCharCode, toCharCode)
import Data.List (mapMaybe, (..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (toUpper)
import Data.String.CodeUnits (toChar, singleton)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

{- | Ignore any value. -}
ignore :: forall a. a -> Unit
ignore _ = unit

{- | Ignore any value in a Monad. -}
ignoreM :: forall a m. Monad m => m a -> m Unit
ignoreM a = ignore <$> a

filterMaybe :: forall a. (a -> Boolean) -> Maybe a -> Maybe a
filterMaybe f a = case a of
  Just a'
    | f a' -> Just a'
  _ -> Nothing

-- Unused types.
type Slots
  = ()

type Input
  = Unit

type Output
  = Unit

data Action
  = Init
  | KeyPressed KeyboardEvent

type State
  = String

letters :: Set Char
letters = Set.fromFoldable $ mapMaybe fromCharCode $ toCharCode 'A' .. toCharCode 'Z'

handleKey :: String -> State -> State
handleKey key state = case filterMaybe (flip Set.member letters) (toChar $ toUpper key) of
  Just letter -> singleton letter
  Nothing -> case key of
    "Enter" -> key
    "Backspace" -> key
    _ -> state

render :: forall m. State -> H.ComponentHTML Action Slots m
render s = HH.div_ [ HH.text $ s ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction action = case action of
  Init -> do
    document <- liftEffect (map HTMLDocument.toEventTarget $ Web.document =<< Web.window)
    ignoreM $ H.subscribe $ eventListener KET.keydown document (map KeyPressed <<< KE.fromEvent)
  KeyPressed ev -> do
    liftEffect $ E.preventDefault (KE.toEvent ev)
    ST.modify_ (handleKey $ KE.key ev)

component :: forall query m. MonadEffect m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: const ""
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    comp <- pure component
    runUI comp unit body
