module Game (State, handleKey, initialState, render) where

import Prelude
import Core (filterMaybe, groupAllWith, padRight)
import Data.Array (concat, dropEnd, filter, fromFoldable, length, mapMaybe, reverse, singleton, sortWith) as Array
import Data.Array (zip, (!!), (..), (:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, reverse, sortWith) as NEA
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (class Foldable, elem, find, foldl, length)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set (delete, empty, fromFoldable, insert, member, union) as Set
import Data.String (Pattern(..), toUpper)
import Data.String.CodeUnits (fromCharArray, length, singleton, toChar, toCharArray) as String
import Data.String.Common (split) as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random as Random
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HA

-- State.
{- The number of attempts the user gets. -}
nAttempts :: Int
nAttempts = 6

{- The word length. -}
wordLength :: Int
wordLength = 5

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

{- Checks an array of used words, and returns a set containing the best match for every letter in those words. -}
usedWords :: Array Word -> Set Entry
usedWords = Set.fromFoldable <<< map bestEntry <<< groupAllWith entryToChar <<< Array.concat

{- Checks an array of used words, a set containing all the correct letters indexed. -}
correctLettersIndexed :: Array Word -> Set (Indexed Entry)
correctLettersIndexed =
  Set.fromFoldable <<< Array.filter (\(Indexed _ e) -> entryScore e == 5) <<< Array.concat <<< map index

-- Initialization
{- Builds an array of words from a large string with words separated by newlines.
Only the words with a length specified in wordLength are kept. -}
buildDictionary :: String -> Array String
buildDictionary =
  String.split (Pattern "\n")
    >>> Array.filter (\w -> String.length w == wordLength)
    >>> map toUpper

getRandomWord :: Array String -> Effect (Maybe String)
getRandomWord words = do
  idx <- Random.randomInt 0 $ Array.length words
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

-- Words.
{- An entry in a word. -}
data Entry
  = Correct Char
  | WrongPlace Char
  | Incorrect Char
  | NotEvaluated Char
  | Empty
  | EmptyPreview Char

derive instance eqEntry :: Eq Entry

derive instance ordEntry :: Ord Entry

{- A score that can be used for getting the entry with the highest or lowest score. -}
entryScore :: Entry -> Int
entryScore e = case e of
  (Correct _) -> 5
  (WrongPlace _) -> 4
  (Incorrect _) -> 3
  (NotEvaluated _) -> 2
  Empty -> 1
  EmptyPreview _ -> 1

{- Extract the character from an entry, if there is one. -}
entryToChar :: Entry -> Maybe Char
entryToChar entry = case entry of
  Correct c -> Just c
  WrongPlace c -> Just c
  Incorrect c -> Just c
  NotEvaluated c -> Just c
  Empty -> Nothing
  EmptyPreview c -> Just c

{- Returns the entry with the highest score. -}
bestEntry :: NonEmptyArray Entry -> Entry
bestEntry = NEA.sortWith entryScore >>> NEA.reverse >>> NEA.head

{- A word in the list of guesses. -}
type Word
  = Array Entry

{- Convert a word to a string, returning the characters of all entries that represent a character. -}
wordToString :: Word -> String
wordToString = Array.mapMaybe entryToChar >>> String.fromCharArray

-- Validation.
{- Adds an index to any type. -}
data Indexed a
  = Indexed Int a

derive instance eqIndexed :: Eq a => Eq (Indexed a)

derive instance ordIndexed :: Ord a => Ord (Indexed a)

instance functorIndexed :: Functor Indexed where
  map f (Indexed idx a) = Indexed idx $ f a

{- Convert a tuple consisting of an Int an an element to an Indexed of the element. -}
indexedFromTuple :: forall a. Tuple Int a -> Indexed a
indexedFromTuple (Tuple idx a) = Indexed idx a

{- Index a foldable by adding indexes to every element and returning it as an array. -}
index :: forall f a. Foldable f => f a -> Array (Indexed a)
index elems = indexedFromTuple <$> zip (0 .. length elems) (Array.fromFoldable elems)

{- Convert a foldable of Indexed elements to an array of elements in the ordering of their index. -}
indexedToArray :: forall f a. Foldable f => Eq a => f (Indexed a) -> Array a
indexedToArray elems = unIndex <$> Array.sortWith getIndex (Array.fromFoldable elems)

{- Extracts a value from an Indexed. -}
unIndex :: forall a. Indexed a -> a
unIndex (Indexed _ a) = a

{- Extracts the index from an Indexed. -}
getIndex :: forall a. Indexed a -> Int
getIndex (Indexed idx _) = idx

{- Validation happens by going through all remaining letters in the input word in multiple passes, updating this state
during every step. The answerLetters are removed once they are matched, unmatchedLetters are built up when letters from
the input word don't match anything, and the ouput contains the end result. -}
type ValidationState
  = { answerLetters :: Set (Indexed Char), unmatchedLetters :: Array (Indexed Char), output :: Set (Indexed Entry) }

{- Validates the provided word against the provided string. First tries to match all exact letters, then the letters
that are at an incorrect position, and then adds the remaining letters as incorrect. -}
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
          , output = Set.insert (Correct <$> letter) st.output
          }
      else
        st { unmatchedLetters = letter : st.unmatchedLetters }

    matchNotExact :: ValidationState -> Indexed Char -> ValidationState
    matchNotExact st (letter@(Indexed _ char)) = case find (\(Indexed _ l) -> l == char) st.answerLetters of
      Just firstFoundLetter ->
        st
          { answerLetters = Set.delete firstFoundLetter st.answerLetters
          , output = Set.insert (WrongPlace <$> letter) st.output
          }
      Nothing -> st { unmatchedLetters = letter : st.unmatchedLetters }
  in
    indexedWord
      # foldl matchExact validationState
      # (\ns -> foldl matchNotExact (ns { unmatchedLetters = [] }) (Array.reverse ns.unmatchedLetters))
      # (\ns -> Set.union ns.output (Set.fromFoldable $ map Incorrect <$> ns.unmatchedLetters))
      # indexedToArray

-- Update.
{- The different types of actions that can be performed in the game. -}
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

{- Submits a word. Checks if the word is in the dictionary, if it was attempted before, if it was correct, and if it
is the last attempt and changes the state accordingly. -}
submitWord :: GuessingState -> State
submitWord s =
  if not $ Set.member (wordToString s.currentAttempt) s.allWords then
    -- Invalid word.
    Guessing
      s
        { currentAttempt = []
        , message = Just $ "'" <> wordToString s.currentAttempt <> "' is not a valid word."
        }
  else if elem s.currentAttempt s.previousAttempts then
    -- Repeated guess.
    Guessing
      s
        { currentAttempt = []
        , message = Just $ "'" <> wordToString s.currentAttempt <> "' has previously been guessed."
        }
  else if wordToString s.currentAttempt == s.answer then
    -- Correct guess.
    Finished { attempts: newAttempts, answer: s.answer, success: true }
  else if Array.length s.previousAttempts == nAttempts - 1 then
    -- Game failed.
    Finished { attempts: newAttempts, answer: s.answer, success: false }
  else
    -- Next attempt.
    Guessing s { previousAttempts = newAttempts, currentAttempt = [], message = Nothing }
  where
  newAttempts = validateWord s.answer s.currentAttempt : s.previousAttempts

{- Handles a key and calls the correct update function. -}
handleKey :: String -> State -> State
handleKey key state = case state of
  Guessing s -> case keyToAction key of
    Just Backspace -> Guessing s { currentAttempt = Array.dropEnd 1 s.currentAttempt }
    Just SubmitWord
      | currentAttemptComplete s -> submitWord s
    Just (InputLetter l)
      | not (currentAttemptComplete s) -> Guessing s { currentAttempt = s.currentAttempt <> [ NotEvaluated l ] }
    _ -> state
  _ -> state

-- Rendering
{- Alias for HTML for this game. -}
type GameHtml a m
  = H.ComponentHTML a () m

{- Render a simple flexbox row. -}
row :: forall a m. GameHtml a m -> GameHtml a m
row = Array.singleton >>> HH.div [ HA.class_ $ ClassName "d-flex p-1 flex-fill justify-content-center" ]

{- Render a word block with all the information from an Entry. -}
wordBlock :: forall a m. Entry -> GameHtml a m
wordBlock entry =
  let
    letter = maybe ' ' identity $ entryToChar entry

    entryBackground = case entry of
      Correct _ -> "green"
      WrongPlace _ -> "yellow"
      Incorrect _ -> "gray"
      _ -> "white"

    entryForeground = case entry of
      EmptyPreview _ -> "; color: lightgray"
      _ -> ""
  in
    HH.div
      [ HA.class_ $ ClassName "border mx-2 text-center align-middle fs-2"
      , HA.style $ "width: 50px; min-width: 50px; max-width: 50px; "
          <> "height: 50px; min-height: 50px; max-height: 50px; background-color: "
          <> entryBackground
          <> entryForeground
      ]
      [ HH.text $ String.singleton letter ]

data WordModifier w
  = Plain w
  | Preview w
  | Button w

derive instance functorWordModifier :: Functor WordModifier

{- Render a row of word blocks. -}
wordRow :: forall a m. Array Word -> WordModifier Word -> GameHtml a m
wordRow previousGuesses modifier =
  let
    correctGuesses = correctLettersIndexed previousGuesses

    previewEntry :: Indexed Entry -> Entry
    previewEntry (Indexed idx e) = case Tuple e
        (entryToChar =<< unIndex <$> find (\(Indexed idx' _) -> idx' == idx) correctGuesses) of
      Tuple Empty (Just letter) -> EmptyPreview letter
      _ -> e

    makePreview :: WordModifier Word -> WordModifier Word
    makePreview (Preview w) = Plain $ previewEntry <$> index w
    makePreview w = w

  in
    HH.div [ HA.class_ $ ClassName "d-flex flex-row" ]
      (wordBlock <$> (makePreview $ padRight wordLength Empty <$> modifier))

{- Render an alert with the specified type and message. -}
alert :: forall a m. String -> String -> GameHtml a m
alert alertType msg = HH.div [ HA.class_ $ ClassName $ "alert alert-" <> alertType ] [ HH.text msg ]

keyboard :: forall a m. Array Word -> Array (GameHtml a m)
keyboard words =
  let
    guessEntries = usedWords words

    keyboardRows = [ "QWERTYUIOP", "ASDFGHJKL", "ZXCVBNM" ]

    entryForLetter l = fromMaybe (NotEvaluated l) $ find (\entry -> entryToChar entry == Just l) guessEntries

    blockRows = keyboardRows <#> String.toCharArray <#> (\l -> Tuple false (l <#> entryForLetter)) <#> wordRow []
  in
    row <$> blockRows

{- Main render function. -}
render :: forall a m. State -> GameHtml a m
render state =
  HH.div
    [ HA.class_ $ ClassName "d-flex p-2 bd-highlight flex-fill flex-column" ]
    ([ row $ HH.h1_ [ HH.text "LINGO" ] ] <> contents)
  where
  contents = case state of
    Guessing s ->
      let
        messageRow = Array.fromFoldable $ row <<< alert "warning" <$> s.message

        letterRows =
          (Tuple true s.currentAttempt) : (Tuple false <$> s.previousAttempts)
            # Array.reverse
            # padRight nAttempts (Tuple false [])
            <#> (row <<< wordRow s.previousAttempts)
      in
        messageRow <> letterRows <> keyboard s.previousAttempts
    Finished s ->
      let
        messageRow =
          if s.success then
            [ row $ alert "success" "Success!" ]
          else
            [ row $ alert "danger" "Failed!" ]

        answerRow = [ row $ alert "primary" $ "The answer was '" <> s.answer <> "'." ]

        letterRows = s.attempts # Array.reverse # padRight nAttempts [] <#> Tuple false <#> row <<< wordRow []
      in
        messageRow <> answerRow <> letterRows <> keyboard s.attempts
    NoWords -> [ row $ alert "danger" "No words found." ]
