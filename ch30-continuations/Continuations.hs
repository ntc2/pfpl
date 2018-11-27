{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Continuations where

import Control.Monad
import Text.Printf
import Debug.Trace
import Data.Void

t :: String -> a -> a
t msg = trace (printf "XXX: %s" msg)

data C r a = C { unC :: (a -> r) -> r }

abort :: r -> C r a
abort x = C (const x)

-- | Show a 'C' computation by running it and showing the result.
instance (Show a) => Show (C a a) where
  show m = printf "C %s" (show (unC m id))

instance Functor (C r) where
  fmap f (C m) = C $ \k -> m (k . f)

instance Applicative (C r) where
  pure x = C $ \k -> k x
  (<*>) = ap

instance Monad (C r) where
  (C m) >>= f = C $ \k -> m (\a -> unC (f a) k)

callcc :: forall a r b. ((a -> C r b) -> C r a) -> C r a
callcc f = C $ \(k :: a -> r) ->
  let k' :: a -> C r b
      -- A computation that ignores it's continuation @_@ and uses the
      -- enclosing continuation @k@ instead.
      k' a = C (\_ -> k a)
  in unC (f k') k

-- | Same code as 'callcc', but more polymorphic, so that the cc can
-- be used at any result type.
callcc' :: forall a r. ((forall b. a -> C r b) -> C r a) -> C r a
callcc' f = C $ \(k :: a -> r) ->
  let k' :: a -> C r b
      -- A computation that ignores it's continuation @_@ and uses the
      -- enclosing continuation @k@ instead.
      k' a = C (\_ -> k a)
  in unC (f k') k

test_nocallcc :: C r Int
test_nocallcc = do
  x1 <- t "0" $ pure 1
  x2 <- callcc' $ \_cc -> do
    y1 <- t "1" $ pure 1
    y2 <- t "2" $ pure 1
    y3 <- t "3" $ pure 1
    t "4" $ pure (y1 + y2 + y3)
  x3 <- t "5" $ pure 1
  pure (x1 + x2 + x3)

test_callcc :: C r Int
test_callcc = do
  x1 <- t "0" $ pure 1
  x2 <- callcc' $ \cc -> do
    y1 <- t "1" $ pure 1
    y2 <- t "2" $ pure 1
    -- Use the cc at type 'Int'
    y3 <- t "3" $ cc 1
    -- Use the cc at type 'String'
    y4 <- t "4" $ cc 1
    let z = "asdf" ++ y4
    t "5" $ pure (y1 + y2 + y3)
  x3 <- t "6" $ pure 1
  pure (x1 + x2 + x3)

main :: IO ()
main = do
  forM_ tests $ \(name, T test) -> do
    let s = show $ test
    -- Here @length s@ is a deepseq.
    length s `seq` printf "%s: %s\n" name s
  where
    tests :: [(String, Test)]
    tests = [ ("nocallcc", T test_nocallcc)
            , ("callcc", T test_callcc)
            , ("test1", T test1)
            , ("test_coroutine2", T test_coroutine2)
            ]

data Test where
  T :: Show a => C a a -> Test

----------------------------------------------------------------
-- Implementation of co-routines that doesn't use callcc.
--
-- The implementation is Eric and Iavor's idea. Compared to my broken
-- implementation above, this one uses @Result@ as the @r@ param of
-- the continuation monad, not as result type.

data Result i o = Done o | Yield o (i -> Result i o) | DoIO (IO (Result i o))

type M i o a = C (Result i o) a

done :: o -> M i o a
done o = C (const (Done o))

yield :: o -> M i o i
yield o = C (Yield o)

io :: IO a -> M i o a
io act = C $ \k -> DoIO $ do
  a <- act
  pure (k a)

runM :: M i o Void -> Result i o
runM (C f) = f absurd

-- | Interpreter for 'Result' computations.
--
-- Given a list of inputs it computes a list of outputs.
interp :: [i] -> Result i o -> IO [o]
interp ins r = case r of
  Done o -> pure [o]
  Yield o k -> case ins of
    (i:ins') -> (o :) <$> interp ins' (k i)
    _ -> pure [o]
  DoIO act -> do
    r <- act
    interp ins r

rsum :: M Int Int a
rsum = loop 0
  where
    loop total = do
      io (printf "%i" total)
      i <- yield total
      loop (i + total)

test3 :: IO [Int]
test3 = interp [2,4] (runM rsum)

test2 :: [Int]
test2 = [o1,o2,o3]
  where
    Yield o1 r1 = runM rsum
    Yield o2 r2 = r1 2
    Yield o3 _  = r2 4

----------------------------------------------------------------
-- Implementation of co-routines using callcc

callcc'' :: forall a r. String -> ((forall b. a -> C r b) -> C r a) -> C r a
callcc'' msg f = C $ \(k :: a -> r) ->
  let k' :: a -> C r b
      -- A computation that ignores it's continuation @_@ and uses the
      -- enclosing continuation @k@ instead.
      k' a = t msg $ C (\_ -> k a)
  in unC (f k') k

newtype ContRec r i o =
  ContRec { unContRec :: forall b. (i, Result' r i o -> C r b) }

data Result' r i o = Done' o
                   | Yield' o (forall b. ContRec r i o -> C r b)

runningSum :: (i ~ Int, o ~ Int) =>
  (forall b. Result' r i o -> C r b) -> C r (Result' r i o)
runningSum cc0 = do
  ContRec (i1, cc1) <- callcc'' "callcc 1" $ \c1 -> cc0 (Yield' 0 c1)
  ContRec (i2, cc2) <- callcc'' "callcc 2" $ \c2 -> cc1 (Yield' 1 c2)
  cc2 (Done' (i1 + i2))

test1 :: C (String,Int) (String,Int)
test1 = do
  x <- callcc'' "running" $ \c -> runningSum c
  case x of
    Done' i -> pure ("0", i)
    Yield' i r -> t "first yield" $ do
      y <- callcc'' "yield1" $ \mc1 -> r (ContRec (8, mc1))
      case y of
        Done' i -> pure ("1", i)
        Yield' i r -> t "second yield" $ do
          z <- callcc'' "yield2" $ \mc2 -> r (ContRec (900, mc2))
          case z of
            Done' i -> pure ("2", i)
            _ -> error "foo"

test_coroutine2 :: C (String, Int) (String, Int)
test_coroutine2 = return ("1", 1)

{-
runningSum :: C r (Result' r Int Int)
runningSum = coroutine $ \done yield ->
  let loop total = do
        inc <- yield total
        loop (inc + total)
  in loop 0

test_runningSum :: C [Int] [Int]
test_runningSum = do
  Yield' o1 r1 <- t "runningSum" $ runningSum
  Yield' o2 r2 <- t "r1" $ r1 2
  Yield' o3 r3 <- t "r2" $ r2 4
  Yield' o4 r4 <- t "r3" $ r3 8
  pure [o1,o2,o3,o4]

test_runningSum' :: C [Int] [Int]
test_runningSum' = loop 5 runningSum
  where
    loop :: Int -> C r (Result' r Int Int) -> C r [Int]
    loop 0 r = return []
    loop n r = do
      Yield' o r' <- r
      os <- loop (n-1) (r' n)
      pure (o : os)
-}
