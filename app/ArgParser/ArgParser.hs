module ArgParser () where

data ArgParser a = ArgParser a | FailedParse

instance Functor ArgParser where
  fmap f (ArgParser a) = ArgParser (f a)
  fmap _ FailedParse = FailedParse

instance Applicative ArgParser where
  liftA2 f (ArgParser a) (ArgParser b) = ArgParser (f a b)
  liftA2 _ FailedParse _ = FailedParse
  liftA2 _ _ FailedParse = FailedParse
  pure = ArgParser

instance Monad ArgParser where
  ArgParser x >>= f = f x
  FailedParse >>= _ = FailedParse
