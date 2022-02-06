module Core (filterMaybe, ignore, ignoreM) where

import Prelude
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
