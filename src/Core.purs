module Core (filterMaybe, groupAllWith, ignore, ignoreM, padRight) where

import Prelude
import Data.Array (groupAllBy, length, replicate) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe(..))

{- Ignore any value. -}
ignore :: forall a. a -> Unit
ignore _ = unit

{- Ignore any value in a Monad. -}
ignoreM :: forall a m. Monad m => m a -> m Unit
ignoreM a = ignore <$> a

{- Filter the the value in a Maybe by a predicate, returning Nothing if the Maybe is Nothing, or it is Just but the
predicate doesn't match. -}
filterMaybe :: forall a. (a -> Boolean) -> Maybe a -> Maybe a
filterMaybe f a = case a of
  Just a'
    | f a' -> Just a'
  _ -> Nothing

{- Group all elements in an array by checking if the elements mapped to an Ord instance are equal. -}
groupAllWith :: forall a b. Ord b => (a -> b) -> Array a -> Array (NonEmptyArray a)
groupAllWith f = Array.groupAllBy (\a b -> compare (f a) (f b))

{- Pads an array with the specified element at the end, until it has reached the specified length. -}
padRight :: forall a. Int -> a -> Array a -> Array a
padRight width elem array = array <> Array.replicate (width - Array.length array) elem
