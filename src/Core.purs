module Core (filterMaybe, ignore, ignoreM, indexesOf) where

import Prelude
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (drop, indexOf) as String
import Data.String.CodeUnits (singleton) as String
import Data.String.Pattern (Pattern(..))

{- Ignore any value. -}
ignore :: forall a. a -> Unit
ignore _ = unit

{- Ignore any value in a Monad. -}
ignoreM :: forall a m. Monad m => m a -> m Unit
ignoreM a = ignore <$> a

filterMaybe :: forall a. (a -> Boolean) -> Maybe a -> Maybe a
filterMaybe f a = case a of
  Just a'
    | f a' -> Just a'
  _ -> Nothing

indexesOf :: Int -> Char -> String -> Array Int
indexesOf offset ch str = case String.indexOf (Pattern $ String.singleton ch) str of
  Nothing -> []
  Just idx -> (offset + idx) : indexesOf (offset + idx + 1) ch (String.drop (idx + 1) str)
