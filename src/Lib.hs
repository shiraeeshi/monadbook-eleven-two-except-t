module Lib
    ( AppState(..)
      , calc
      , ExceptT(..)
      , Reader(..)
    ) where

import Text.Read (readEither)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
data MyEither e a = MyLeft e | MyRight a deriving Show
----------------------------------

instance Functor (MyEither e) where
  fmap f e =
    case e of
      --l@(MyLeft _) -> l
      (MyLeft l) -> MyLeft l
      (MyRight r) -> MyRight (f r)

instance Applicative (MyEither e) where
  pure x = MyRight x
  e1 <*> e2 =
    case e1 of
      --l@(MyLeft _) -> l
      (MyLeft l) -> MyLeft l
      (MyRight f) -> case e2 of
                       --l2@(MyLeft _) -> l2
                       (MyLeft l2) -> MyLeft l2
                       (MyRight x) -> MyRight (f x)

instance Monad (MyEither e) where
  e >>= f =
    case e of
      --l@(MyLeft _) -> l
      (MyLeft l) -> MyLeft l
      (MyRight a) ->
        let e2 = f a
        in case e2 of
          --l2@(MyLeft _) -> l2
          (MyLeft l2) -> MyLeft l2
          r@(MyRight _) -> r

----------------------------------

fromEither :: Either e a -> MyEither e a
fromEither (Left l) = MyLeft l
fromEither (Right r) = MyRight r

mapLeft :: (a -> b) -> MyEither a r -> MyEither b r
mapLeft f (MyLeft l) = MyLeft (f l)
mapLeft _ (MyRight r) = MyRight r

withErrorMsg :: String -> MyEither a r -> MyEither String r
withErrorMsg msg = mapLeft $ \_ -> msg

----------------------------------
data ExceptT e m a = ExceptT { runExceptT :: m (MyEither e a) }
----------------------------------

instance Monad m => Functor (ExceptT e m) where
  -- fmap f (ExceptT g) = ExceptT $ fmap f g -- f operates on "a", not on "MyEither e a"
  fmap f (ExceptT g) = ExceptT $ do
    ea <- g
    return $ fmap f ea
  -- alternatively,
  -- fmap f (ExceptT g) = ExceptT $ fmap (fmap f) g

instance Monad m => Applicative (ExceptT e m) where
  pure x = ExceptT $ return (MyRight x)
  (ExceptT g) <*> (ExceptT h) = ExceptT $ do
    e1 <- g
    case e1 of
      (MyLeft l) -> return (MyLeft l)
      (MyRight f) -> do
        e2 <- h
        case e2 of
          (MyLeft l2) -> return (MyLeft l2)
          (MyRight x) -> return $ MyRight (f x)

instance Monad m => Monad (ExceptT e m) where
  (ExceptT g) >>= f = ExceptT $ do
    e1 <- g
    case e1 of
      (MyLeft l) -> return (MyLeft l)
      (MyRight a) -> do
        let (ExceptT h) = f a
        e2 <- h
        case e2 of
          (MyLeft l2) -> return (MyLeft l2)
          r@(MyRight _) -> return r

----------------------------------

data AppState = AppState { getX :: String, getY :: String }

----------------------------------

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  --fmap f (Reader g) = Reader $ f . g
  fmap f (Reader g) = Reader $ \i -> f (g i)

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x
  (Reader g) <*> (Reader h) = Reader $ \i ->
    let f = g i
        x = h i
    in f x

instance Monad (Reader r) where
  (Reader g) >>= f = Reader $ \i ->
    let a = g i
        (Reader h) = f a
    in h i

----------------------------------

ask :: Reader r r
ask = Reader $ \i -> i

asks :: (r -> b) -> Reader r b
asks f = Reader $ \i -> f i

----------------------------------

calc :: ExceptT String (Reader AppState) Int
calc = ExceptT $ do
  ex <- asks (withErrorMsg "cannot parse x value" . fromEither . readEither . getX)
  ey <- asks (withErrorMsg "cannot parse y value" . fromEither . readEither . getY)
  Reader $ \_ -> do
    x <- ex
    y <- ey
    if y == 0 then (MyLeft "y is zero")
              else return $ x `div` y

