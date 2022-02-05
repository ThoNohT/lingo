module Game
  ( State
  , handleKey
  , initialState
  , render
  ) where

import Prelude
import Core (filterMaybe)
import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)
import Data.List (List(..), (!!), (..), (:))
import Data.List (fromFoldable, length, mapMaybe) as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (fromFoldable, member) as Set
import Data.String (Pattern(..), toUpper)
import Data.String (codePointFromChar) as String
import Data.String.CodeUnits (dropRight, length, toChar) as String
import Data.String.Common (split) as String
import Data.String.NonEmpty (toString) as String
import Data.String.NonEmpty.CodePoints (snoc) as String
import Effect (Effect)
import Effect.Console as Console
import Effect.Random as Random
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HA

{- The number of attempts the user gets. -}
nAttempts :: Int
nAttempts = 6

{- The word length. -}
wordLength :: Int
wordLength = 5

buildDictionary :: String -> List String
buildDictionary =
  String.split (Pattern "\n")
    >>> map (String.dropRight 1)
    >>> Array.filter (\w -> String.length w == wordLength)
    >>> map toUpper
    >>> List.fromFoldable

getRandomWord :: List String -> Effect (Maybe String)
getRandomWord words = do
  let
    max = List.length words
  idx <- Random.randomInt 0 max
  pure $ words !! idx

initialState :: String -> Effect State
initialState dict = do
  let
    words = buildDictionary dict
  answer <- getRandomWord words
  case answer of
    Just a ->
      pure
        $ Guessing
            { previousAttempts: Nil
            , currentAttempt: ""
            , answer: a
            , allWords: Set.fromFoldable words
            , message: Nothing
            }
    Nothing -> pure NoWords

{- The current state in the game.

Guessing invariants:
  - previousAttempts has max length of nAttempts - 1. Adding the last will immediately move to Finished.
  - currentAttempt has max length of 5, adding more characters will not work.
  - submitting a word only works when currentAttempt has length 5.
-}
data State
  = Guessing GuessingState
  | Finished FinishedState
  | NoWords

-- TODO: Make previousAttempts a list of some type indicating whether it is correct, or wrong place, or incorrect.
type GuessingState
  = { previousAttempts :: List String
    , currentAttempt :: String
    , answer :: String
    , allWords :: Set String
    , message :: Maybe String
    }

{- Indicates whether the current attempt is ready to be submitted. -}
currentAttemptComplete :: GuessingState -> Boolean
currentAttemptComplete s = String.length s.currentAttempt == 5

type FinishedState
  = { attempts :: List String, answer :: String, success :: Boolean }

data Action'
  = InputLetter Char
  | Backspace
  | SubmitWord

{- Convert a string representing a pressed key to an action in the game. -}
keyToAction :: String -> Maybe Action'
keyToAction key = case filterMaybe (flip Set.member letters) (String.toChar $ toUpper key) of
  Just letter -> Just $ InputLetter letter
  Nothing -> case key of
    "Enter" -> Just SubmitWord
    "Backspace" -> Just Backspace
    _ -> Nothing
  where
  letters = Set.fromFoldable $ List.mapMaybe fromCharCode $ toCharCode 'A' .. toCharCode 'Z'

submitWord :: GuessingState -> Effect State
submitWord s =
  if not $ Set.member s.currentAttempt s.allWords then do
    -- Invalid word.
    Console.log "Invalid word."
    pure $ Guessing s { currentAttempt = "", message = Just $ "'" <> s.currentAttempt <> "' is not a valid word." }
  else if s.currentAttempt == s.answer then do
    -- Correct guess.
    Console.log "Correct."
    pure $ Finished { attempts: s.currentAttempt : s.previousAttempts, answer: s.answer, success: true }
  else if List.length s.previousAttempts == nAttempts - 1 then do
    -- Game failed.
    Console.log "Failed."
    pure $ Finished { attempts: s.currentAttempt : s.previousAttempts, answer: s.answer, success: false }
  else do
    -- Next attempt.
    Console.log "Incorrect."
    pure $ Guessing s { previousAttempts = s.currentAttempt : s.previousAttempts, currentAttempt = "", message = Nothing }

handleKey :: String -> State -> Effect State
handleKey key state = case state of
  Guessing s -> case keyToAction key of
    Just Backspace -> pure $ Guessing s { currentAttempt = String.dropRight 1 s.currentAttempt }
    Just SubmitWord
      | currentAttemptComplete s -> submitWord s
    Just (InputLetter l)
      | not (currentAttemptComplete s) ->
        pure
          $ Guessing s { currentAttempt = String.toString $ String.snoc (String.codePointFromChar l) s.currentAttempt }
    _ -> pure state
  _ -> pure state

type GameHtml a m
  = H.ComponentHTML a () m

row :: forall a m. Array (GameHtml a m) -> GameHtml a m
row = HH.div [ HA.class_ $ ClassName "d-flex p-2 flex-fill justify-content-center" ]

render :: forall a m. State -> GameHtml a m
render state =
  HH.div [ HA.class_ $ ClassName "d-flex p-2 bd-highlight flex-fill flex-column" ]
    ([ row [ HH.h1_ [ HH.text "LINGO" ] ] ] <> contents)
  where
  contents = case state of
    Guessing s ->
      [ row [ HH.div_ [ HH.text s.currentAttempt ] ]
      , row [ HH.div_ [ HH.text s.answer ] ]
      ]
    Finished _ -> [ row [ HH.text "Finished." ] ]
    NoWords -> [ row [ HH.text "No words found." ] ]
