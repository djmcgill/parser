module Core where

import Control.Applicative
import Control.Monad
import Data.Maybe (fromJust)

-- Parses a 'b' from a list of 'a's. Might succeed and return a value, or fail and not.
-- No characters are consumed if a parser fails.
newtype Parser a b = Parser {parse :: [a] -> Maybe (b,[a])}

result :: Maybe (b,[a]) -> Maybe b
result = fmap fst

unsafeResult = fromJust . result

instance Functor (Parser a) where
  fmap = liftM

instance Applicative (Parser a) where
  pure  = return
  (<*>) = ap

instance Monad (Parser a) where
  return x     = Parser $ \inp -> Just (x,inp)
  parser >>= f = Parser $ \inp -> do
    (v, out) <- parse parser inp
    parse (f v) out

instance Alternative (Parser a) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (Parser a) where
  mzero     = Parser $ const Nothing
  mplus a b = Parser $ \inp -> maybe (parse b inp) Just (parse a inp)
