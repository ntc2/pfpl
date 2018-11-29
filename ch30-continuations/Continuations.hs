{-# LANGUAGE BangPatterns #-}
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
import Control.Monad.Trans.State
import Control.Monad.Trans ( lift )

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
      -- A computation that ignores its continuation @_@ and uses the
      -- enclosing continuation @k@ instead.
      k' a = C (\_ -> k a)
  in unC (f k') k

-- | Same code as 'callcc', but more polymorphic, so that the cc can
-- be used at any result type.
callcc' :: forall a r. ((forall b. a -> C r b) -> C r a) -> C r a
callcc' f = C $ \(k :: a -> r) ->
  let k' :: a -> C r b
      -- A computation that ignores its continuation @_@ and uses the
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
            , ("test_runningSum", T test_runningSum)
            , ("test_runningSum'", T test_runningSum')
            , ("test_runningSum2", T test_runningSum2)
            , ("test_runningSum2'", T test_runningSum2')
            , ("test_xrange", T test_xrange)
            ]

data Test where
  T :: Show a => C a a -> Test

----------------------------------------------------------------
-- Implementation of co-routines using callcc

-- | A version of 'callcc'' with trace messages when the cc is run.
callcc'' :: forall a r. String -> (Cc r a -> C r a) -> C r a
callcc'' msg f = C $ \(k :: a -> r) ->
  let k' :: a -> C r b
      -- A computation that ignores its continuation @_@ and uses the
      -- enclosing continuation @k@ instead.
      k' a = t msg $ C (\_ -> k a)
  in unC (f k') k

-- | Type of polymorphic current continuations, as provided by
-- 'callcc''. These continuations consume @a@ values and produce any
-- type (the @forall b@), since they never return to the call site.
type Cc r a = forall b. a -> C r b

-- | Data for resuming a coroutine: an input and continuation to
-- receive the next output of the coroutine.
data Resume r i o = Resume i (Cc r (Result' r i o))

resume :: Cc r (Resume r i o) -> i -> C r (Result' r i o)
resume r i = callcc' $ \cc -> r (Resume i cc)

-- | An output from a coroutine.
data Result' r i o = Done' o
                     -- | An output and cc that resumes the coroutine.
                   | Yield' o (Cc r (Resume r i o))

yield' :: Cc r (Result' r i o) -> o -> C r (Resume r i o)
yield' cc o = callcc' $ \r -> cc (Yield' o r)

done' :: Cc r (Result' r i o) -> o -> C r (Resume r i o)
done' cc o = cc (Done' o)

----------------------------------------------------------------
-- Test using the 'resume', 'done'', and 'yield'' helper functions

runningSum :: Cc r (Result' r Int Int) -> C r (Result' r Int Int)
runningSum cc0 =
  let loop :: Cc r (Result' r Int Int) -> Int -> C r (Result' r Int Int)
      loop cc !total = do
        Resume inc cc' <- yield' cc total
        loop cc' (inc + total)
  in loop cc0 0

test_runningSum :: C [Int] [Int]
test_runningSum = do
  Yield' o1 r1 <- t "runningSum" $ callcc' runningSum
  Yield' o2 r2 <- t "r1" $ resume r1 2
  Yield' o3 r3 <- t "r2" $ resume r2 4
  Yield' o4 r4 <- t "r3" $ resume r3 8
  pure [o1,o2,o3,o4]

test_runningSum' :: C [Int] [Int]
test_runningSum' = loop 1 6 (callcc' runningSum)
  where
    loop :: Int -> Int -> C r (Result' r Int Int) -> C r [Int]
    loop k stop r
      | k == stop = return []
      | otherwise = do
        Yield' o r' <- r
        os <- loop (k+1) stop (resume r' k)
        pure (o : os)

----------------------------------------------------------------
-- Coroutines with implicit caller continuations

-- Can't use our polymorphic 'Cc' continuation here directly as the
-- state type in 'CoroutineM' because GHC complains about
-- "impredicativity". So, we wrap the 'Cc' type in 'WrappedCc' to hide
-- the polymorphism.
newtype WrappedCc r a = Wrap { unwrap :: Cc r a }
type CoroutineM r i o a = StateT (WrappedCc r (Result' r i o)) (C r) a
type Coroutine r i o = forall a. CoroutineM r i o a

-- | Like 'yield'', but with implicit caller continuation in the state
-- monad.
yield2 :: o -> CoroutineM r i o i
yield2 o = do
  caller <- get
  Resume i caller' <- lift (callcc' $ \r -> unwrap caller (Yield' o r))
  put (Wrap caller')
  return i

-- | Like 'done'', but with implicit caller continuation in the state
-- monad.
done2 :: o -> forall a. CoroutineM r i o a
done2 o = do
  caller <- get
  lift (unwrap caller (Done' o))

-- | Start a coroutine. The same 'resume' from above still works for
-- resuming here.
start :: Coroutine r i o -> C r (Result' r i o)
start cr = callcc' $ \caller -> evalStateT cr (Wrap caller)

----------------------------------------------------------------

-- | Yields the running sum of all inputs so far. Never 'Done''s.
runningSum2 :: Coroutine r Int Int
runningSum2 =
  let loop :: Int -> Coroutine r Int Int
      loop !total = do
        inc <- yield2 total
        loop (inc + total)
  in loop 0

test_runningSum2 :: C [Int] [Int]
test_runningSum2 = do
  Yield' o1 r1 <- t "runningSum" $ start runningSum2
  Yield' o2 r2 <- t "r1" $ resume r1 2
  Yield' o3 r3 <- t "r2" $ resume r2 4
  Yield' o4 r4 <- t "r3" $ resume r3 8
  pure [o1,o2,o3,o4]

test_runningSum2' :: C [Int] [Int]
test_runningSum2' = loop 1 6 (start runningSum2)
  where
    loop :: Int -> Int -> C r (Result' r Int Int) -> C r [Int]
    loop k stop r
      | k == stop = return []
      | otherwise = do
        Yield' o r' <- r
        os <- loop (k+1) stop (resume r' k)
        pure (o : os)

-- | Like Python xrange, except it returns the first value greater
-- than the stop condition, i.e. the sequence is never empty.
--
-- This is a coroutine that doesn't receive inputs from the caller
-- (the input type is unit).
xrange :: Int -> Int -> Int -> Coroutine r () Int
xrange start stop step
  | start >= stop =
      done2 start
  | otherwise = do
      yield2 start
      xrange (start + step) stop step

toList :: Coroutine r () a -> C r [a]
toList cr = do
  result <- start cr
  go result
  where
    go :: Result' r () a -> C r [a]
    go (Done' x) = return [x]
    go (Yield' x r) = do
      result <- resume r ()
      xs <- go result
      pure (x:xs)

test_xrange :: C [Int] [Int]
test_xrange = toList (xrange 1 20 4)

----------------------------------------------------------------
-- Test using the 'Resume' and 'Result'' constructors directly.

sumTwice :: (i ~ Int, o ~ Int) =>
  (Cc r (Result' r i o)) -> C r (Result' r i o)
sumTwice cc0 = do
  Resume i1 cc1 <- callcc'' "callcc 1" $ \c1 -> cc0 (Yield' 0 c1)
  Resume i2 cc2 <- callcc'' "callcc 2" $ \c2 -> cc1 (Yield' 1 c2)
  cc2 (Done' (i1 + i2))

test1 :: C (String,Int) (String,Int)
test1 = do
  x <- callcc'' "running" $ \c -> sumTwice c
  case x of
    Done' i -> pure ("0", i)
    Yield' i r -> t "first yield" $ do
      y <- callcc'' "yield1" $ \mc1 -> r (Resume 8 mc1)
      case y of
        Done' i -> pure ("1", i)
        Yield' i r -> t "second yield" $ do
          z <- callcc'' "yield2" $ \mc2 -> r (Resume 900 mc2)
          case z of
            Done' i -> pure ("2", i)
            _ -> error "foo"

test_coroutine2 :: C (String, Int) (String, Int)
test_coroutine2 = return ("1", 1)

----------------------------------------------------------------
-- Implementation of co-routines that doesn't use callcc.
--
-- The implementation is Eric and Iavor's idea. Compared to my
-- callcc-using implementation above, this one uses @Result@ as the
-- @r@ param of the continuation monad, not as result type, which
-- means all the coroutines need to have the same input and output
-- types, which is the problem we were trying to avoid in the first
-- place vs the PFPL example.

data Result i o = Done o | Yield o (i -> Result i o) | DoIO (IO (Result i o))

type M i o a = C (Result i o) a

done :: o -> M i o a
done o = C (const (Done o))

yield :: o -> M i o i
yield o = C (\k -> Yield o k)

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
