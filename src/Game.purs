module Game (State, handleKey, initialState, render) where

import Prelude
import Core (filterMaybe)
import Data.Array (dropEnd, filter, fromFoldable, length, mapMaybe, replicate, reverse, sortWith) as Array
import Data.Array (zip, (!!), (..), (:))
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (class Foldable, elem, find, foldl, length)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set (delete, empty, filter, fromFoldable, insert, member, union) as Set
import Data.String (Pattern(..), toUpper)
import Data.String.CodeUnits (dropRight, fromCharArray, length, singleton, toChar, toCharArray) as String
import Data.String.Common (split) as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
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

buildDictionary :: String -> Array String
buildDictionary =
  String.split (Pattern "\n")
    >>> map (String.dropRight 1)
    >>> Array.filter (\w -> String.length w == wordLength)
    >>> map toUpper

getRandomWord :: Array String -> Effect (Maybe String)
getRandomWord words = do
  let
    max = Array.length words
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
            { previousAttempts: []
            , currentAttempt: []
            , answer: a
            , allWords: Set.fromFoldable words
            , message: Nothing
            }
    Nothing -> pure NoWords

{- The current state in the game.

Guessing invariants:
  - previousAttempts has max length of nAttempts - 1. Adding the last will immediately move to Finished.
  - currentAttempt has max length of wordLength, adding more characters will not work.
  - submitting a word only works when currentAttempt has length wordLength.
-}
data State
  = Guessing GuessingState
  | Finished FinishedState
  | NoWords

{- An entry in a word. -}
data Entry
  = Correct Char
  | WrongPlace Char
  | Incorrect Char
  | NotEvaluated Char
  | Empty

derive instance eqEntry :: Eq Entry

derive instance ordEntry :: Ord Entry

{- Extract the character from an entry, if there is one. -}
entryToChar :: Entry -> Maybe Char
entryToChar entry = case entry of
  Correct c -> Just c
  WrongPlace c -> Just c
  Incorrect c -> Just c
  NotEvaluated c -> Just c
  Empty -> Nothing

{- A word in the list of guesses. -}
type Word
  = Array Entry

{- Convert a word to a string, returning the characters of all entries that represent a character. -}
wordToString :: Word -> String
wordToString = Array.mapMaybe entryToChar >>> String.fromCharArray

data Indexed a
  = Indexed Int a

derive instance eqIndexed :: Eq a => Eq (Indexed a)

derive instance ordIndexed :: Ord a => Ord (Indexed a)

instance functorIndexed :: Functor Indexed where
  map f (Indexed idx a) = Indexed idx $ f a

{- Convert a tuple consisting of an Int an an element to an indexed of the element. -}
indexedFromTuple :: forall a. Tuple Int a -> Indexed a
indexedFromTuple (Tuple idx a) = Indexed idx a

{- Index a foldable by adding indexes to every element and returning it as an array. -}
index :: forall f a. Foldable f => f a -> Array (Indexed a)
index elems = map indexedFromTuple $ zip (0 .. length elems) $ Array.fromFoldable elems

{- Convert a foldable of Indexed elements to an array of elements in the ordering of their index. -}
indexedToArray :: forall f a. Foldable f => Eq a => f (Indexed a) -> Array a
indexedToArray elems = map (\(Indexed _ a) -> a) $ Array.sortWith (\(Indexed idx _) -> idx) $ Array.fromFoldable elems

type ValidationState
  = { answerLetters :: Set (Indexed Char), unmatchedLetters :: Array (Indexed Char), output :: Set (Indexed Entry) }

{- Validates the provided word against the provided string. -}
validateWord :: String -> Word -> Word
validateWord answer word =
  let
    indexedWord = index $ String.toCharArray $ wordToString word

    validationState =
      { answerLetters: Set.fromFoldable $ index $ String.toCharArray answer
      , unmatchedLetters: []
      , output: Set.empty
      }

    matchExact :: ValidationState -> Indexed Char -> ValidationState
    matchExact st letter =
      if elem letter st.answerLetters then
        st
          { answerLetters = Set.delete letter st.answerLetters
          , output = Set.insert (map Correct letter) st.output
          }
        else
        st { unmatchedLetters = letter : st.unmatchedLetters }

    matchNotExact :: ValidationState -> Indexed Char -> ValidationState
    matchNotExact st (letter@(Indexed _ char)) = case find (const true) $ Set.filter (\(Indexed _ l) -> l == char) st.answerLetters of
      Just firstFoundLetter ->
        st
          { answerLetters = Set.delete firstFoundLetter st.answerLetters
          , output = Set.insert (map WrongPlace letter) st.output
          }
      Nothing -> st { unmatchedLetters = letter : st.unmatchedLetters }
  in
    indexedWord
      # foldl matchExact validationState
      # (\ns -> foldl matchNotExact (ns { unmatchedLetters = [] }) (Array.reverse ns.unmatchedLetters))
      # (\ns -> Set.union ns.output (Set.fromFoldable $ map (map Incorrect) $ ns.unmatchedLetters))
      # indexedToArray

type GuessingState
  = { previousAttempts :: Array Word
    , currentAttempt :: Word
    , answer :: String
    , allWords :: Set String
    , message :: Maybe String
    }

{- Indicates whether the current attempt is ready to be submitted. -}
currentAttemptComplete :: GuessingState -> Boolean
currentAttemptComplete s = Array.length s.currentAttempt == wordLength

type FinishedState
  = { attempts :: Array Word, answer :: String, success :: Boolean }

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
  letters = Set.fromFoldable $ Array.mapMaybe fromCharCode $ toCharCode 'A' .. toCharCode 'Z'

submitWord :: GuessingState -> Effect State
submitWord s =
  if not $ Set.member (wordToString s.currentAttempt) s.allWords then do
    -- Invalid word.
    pure
      $ Guessing
          s
            { currentAttempt = []
            , message = Just $ "'" <> wordToString s.currentAttempt <> "' is not a valid word."
            }
  else if elem s.currentAttempt s.previousAttempts then do
    -- Repeated guess.
    pure
      $ Guessing
          s
            { currentAttempt = []
            , message = Just $ "'" <> wordToString s.currentAttempt <> "' has previously been guessed."
            }
  else if wordToString s.currentAttempt == s.answer then do
    -- Correct guess.
    pure
      $ Finished
          { attempts: validateWord s.answer s.currentAttempt : s.previousAttempts
          , answer: s.answer
          , success: true
          }
  else if Array.length s.previousAttempts == nAttempts - 1 then do
    -- Game failed.
    pure
      $ Finished
          { attempts: validateWord s.answer s.currentAttempt : s.previousAttempts
          , answer: s.answer
          , success: false
          }
  else do
    -- Next attempt.
    pure
      $ Guessing
          s
            { previousAttempts = validateWord s.answer s.currentAttempt : s.previousAttempts
            , currentAttempt = []
            , message = Nothing
            }

handleKey :: String -> State -> Effect State
handleKey key state = case state of
  Guessing s -> case keyToAction key of
    Just Backspace -> pure $ Guessing s { currentAttempt = Array.dropEnd 1 s.currentAttempt }
    Just SubmitWord
      | currentAttemptComplete s -> submitWord s
    Just (InputLetter l)
      | not (currentAttemptComplete s) ->
        pure
          $ Guessing s { currentAttempt = s.currentAttempt <> [ NotEvaluated l ] }
    _ -> pure state
  _ -> pure state

type GameHtml a m
  = H.ComponentHTML a () m

row :: forall a m. Array (GameHtml a m) -> GameHtml a m
row = HH.div [ HA.class_ $ ClassName "d-flex p-1 flex-fill justify-content-center" ]

wordBlock :: forall a m. Entry -> GameHtml a m
wordBlock entry =
  let
    letter = maybe ' ' identity $ entryToChar entry

    entryStyle = case entry of
      Correct _ -> "green"
      WrongPlace _ -> "yellow"
      Incorrect _ -> "gray"
      _ -> "white"
  in
    HH.div
      [ HA.class_ $ ClassName "border mx-2 text-center align-middle fs-2"
      , HA.style $ "width: 50px; min-width: 50px; max-width: 50px; "
          <> "height: 50px; min-height: 50px; max-height: 50px; background-color: "
          <> entryStyle
      ]
      [ HH.text $ String.singleton letter ]

wordRow :: forall a m. Word -> GameHtml a m
wordRow word =
  let
    allBlocks = word <> (Array.replicate (wordLength - Array.length word) Empty)
  in
    HH.div [ HA.class_ $ ClassName "d-flex flex-row" ] (map wordBlock allBlocks)

alert :: forall a m. String -> String -> GameHtml a m
alert alertType msg = HH.div [ HA.class_ $ ClassName $ "alert alert-" <> alertType ] [ HH.text msg ]

render :: forall a m. State -> GameHtml a m
render state =
  HH.div
    [ HA.class_ $ ClassName "d-flex p-2 bd-highlight flex-fill flex-column" ]
    ([ row [ HH.h1_ [ HH.text "LINGO" ] ] ] <> contents)
  where
  contents = case state of
    Guessing s ->
      let
        messageRow = Array.fromFoldable $ map (\m -> row [ alert "warning" m ]) s.message

        attemptRows = map (\a -> row [ wordRow a ]) $ Array.reverse s.previousAttempts

        guessRow = [ row [ wordRow s.currentAttempt ] ]

        blankRows =
          map (\a -> row [ wordRow a ])
            $ Array.replicate (nAttempts - Array.length s.previousAttempts - 1) []
      in
        messageRow <> attemptRows <> guessRow <> blankRows
    Finished s ->
      let
        messageRow =
          if s.success then
            [ row [ alert "success" "Success!" ] ]
          else
            [ row [ alert "danger" "Failed!" ] ]

        attemptRows = map (\a -> row [ wordRow a ]) $ Array.reverse s.attempts

        blankRows =
          map (\a -> row [ wordRow a ])
            $ Array.replicate (nAttempts - Array.length s.attempts) []

        answerRow = [ row [ alert "primary" $ "The answer was '" <> s.answer <> "'." ] ]
      in
        messageRow <> attemptRows <> blankRows <> answerRow
    NoWords -> [ row [ alert "danger" "No words found." ] ]
