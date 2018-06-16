{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PFPL Ch 15: Inductive and Coinductive Types.
--
-- Run 'main' to print examples.
--
-- __Note on Exercises__: the point is to write programs in the language
-- Harper describes in Ch 15, not to write Haskell programs. We're
-- using Haskell to model the language in Ch 15, but Haskell has many
-- features that the Ch 15 language does not have, such as pattern
-- matching and general recursion, so we need to be careful that the
-- solutions we write here correspond to programs in Harper's Ch 15
-- language.
--
-- __Rules__ that make sure our solutions correspond to Ch 15
-- programs:
--
-- - we can apply 'Fold', 'Gen', 'rec', 'unfold', and 'fmap'.
--
-- - we can't use pattern match on 'Fold', 'Gen', or use Haskell
--   recursion.
--
-- - we can pattern match on the polynomial @t.tau@ functors we
--   define, like 'T_NatF' __[1]__.
--
-- These rules keep our programs in the language described by Harper,
-- altho embedded in Haskell. Of course, in defining our model of
-- Harper's language, we are allowed to break the "rules" to
-- /implement/ the primitives such as 'unfold' and 'rec'; the rules
-- apply to /using/ the primitives to write programs in the modeled
-- language.
--
-- __Footnote [1]__: why are we allowed to pattern match on the
-- @t.tau@ functors we define when Harper's Ch 15 language doesn't
-- have pattern matching? The reason this pattern matching is OK is
-- that the polynomial @t.tau@ functors we define correspond to
-- instances of 'E', and pattern matching on them corresponds to using
-- the elimination functions 'l', 'r', and 'caseOf' (and 'v' and 'b',
-- but these are purely modeling artifacts and corresponding to
-- nothing in Harper's language). Indeed, if we use 'E' to model our
-- @t.tau@ functors, then no pattern matching is necessary. For
-- example, as an 'E' functor 'T_NatF' is 'Nat'F', and pattern
-- matching on these 'T_NatF' is modeled by 'caseNat'F', which is a
-- program in Harper's language, implemented in terms of 'l', 'r', and
-- 'caseOf'.
--
-- __Note on Notation__: I can't use dots ('.') in Haskell names, so I'm
-- using underscores instead. E.g. you should read @t_tau@ as
-- @t.tau@. Also, in Haskell we do substitution by application, so
-- e.g. @[tau'/t]tau@ is written as application @(t.tau) tau'@, which
-- in turn is @t_tau tau'@ in the Haskell below.
module Ch15 where

import Text.Printf

----------------------------------------------------------------
-- * Inductive types

-- | The inductive types.
--
-- Compare with Eq 15.8a.
--
-- The fixed point @Ind t.tau@ of the functor @t.tau@ has terms @Fold
-- e@ for @e : (t.tau) (Ind t.tau)@.
--
-- Note that @(t.tau) (Ind t.tau) = [(Ind t.tau)/t]tau@.
data Ind t_tau = Fold (t_tau (Ind t_tau))

deriving instance Show (f (Ind f)) => Show (Ind f)

-- | The recursor.
--
-- Compare with Eq 15.8b and Eq 15.9c.
--
-- Note that
--
-- > x_e1 : t_tau tau' -> tau'
--
-- is
--
-- > x : [tau'/t]tau |- e1 tau'
rec :: Functor t_tau => (t_tau tau' -> tau') -> Ind t_tau -> tau'
rec x_e1 (Fold e2) = x_e1 $ fmap (rec x_e1) e2

----------------------------------------------------------------
-- * Coinductive types

-- | Coinductive types.
--
-- Compare with Eq 15.8d.
data Coi t_tau = forall tau2. Gen { _x_e1 :: tau2 -> t_tau tau2
                                  , _e2   :: tau2 }

-- | The unfold.
unfold :: Functor t_tau => Coi t_tau -> t_tau (Coi t_tau)
unfold (Gen x_e1 e2) = fmap (\y -> Gen x_e1 y) $ x_e1 e2

----------------------------------------------------------------
-- * Inductive naturals

-- | Functor for defining inductive naturals.
--
-- Isomorphic to @t.() + t@.
data T_NatF t = NatZ | NatS t deriving (Functor, Show)

-- | The naturals are the inductive type generated from the functor
-- @T_NatF@.
type Nat = Ind T_NatF

-- | Zero.
z :: Nat
z = Fold NatZ

-- | Successor.
s :: Nat -> Nat
s n = Fold (NatS n)

-- | Pretty print a nat.
prettyNat :: Nat -> String
prettyNat = rec @T_NatF x_e1
  where
    x_e1 NatZ = "z"
    x_e1 (NatS a) = "s "++a

threeNat :: Nat
threeNat = s (s (s z))

addNat :: Nat -> Nat -> Nat
addNat m n = rec @T_NatF x_e1 m
  where
    x_e1 NatZ = n
    x_e1 (NatS a) = Fold (NatS a)

sixNat :: Nat
sixNat = addNat threeNat threeNat

mainNat :: IO ()
mainNat = do
  printf "threeNat = %s\n" (prettyNat threeNat)
  printf "sixNat = %s\n" (prettyNat sixNat)

-- | Exercise: define multiplication on nats.
multNat :: Nat -> Nat -> Nat
multNat m n = rec @T_NatF undefined undefined

-- | Exercise: define subtraction on nats.
--
-- Returns zero if @n@ is greater than @m@.
subNat :: Nat -> Nat -> Nat
subNat m n = rec @T_NatF undefined undefined

----------------------------------------------------------------
-- * Inductive binary trees
--
-- Exercise: define 'Ind' versions of the following Haskell
-- definitions:

data HTree = HLeaf Int | HFork HTree HTree deriving Show

reverseHTree :: HTree -> HTree
reverseHTree (HLeaf x) = HLeaf x
reverseHTree (HFork l r) = HFork (reverseHTree l) (reverseHTree r)

-- | Exercise: define functor for binary trees of 'Int's corresponding
-- to 'HTree' above.
--
-- Use regular Hakell 'Int' or 'Nat' from above, as you like.
data T_TreeF t = TreeUndefined deriving Functor

type Tree = Ind T_TreeF

-- | Exercise: define tree reversal using recursor.
reverseTree :: Tree -> Tree
reverseTree t = rec @T_TreeF undefined undefined

----------------------------------------------------------------
-- * Coinductive naturals

type Conat = Coi T_NatF

-- | Compare with bottom of page 131.
omega :: Conat
omega = Gen NatS () -- The state '()' here is totally irrelevant;
                    -- even undefined works.

-- | Exercise 15.1: embed @Nat@ into @Conat@.
--
-- This is an embedding in the sense that if you keep unfolding the
-- 'Conat' you get until it turns into a 'NatZ' constructor, you get
--
-- > s (s ... (s z) ... ) |-> NatS (NatS ... (NatS NatZ) ...)
natToConat :: Nat -> Conat
natToConat = indToCoi

-- | Embed an inductive term into the corresponding coinductive term.
--
-- The inverse is '_coiToInd', but that function is not definable in
-- Harper's Ch 15 language, because it doesn't terminate in general.
indToCoi :: Functor t_tau => Ind t_tau -> Coi t_tau
indToCoi = Gen unFold

-- | Turn a coinductive term into the corresponding inductive term, if
-- any.
--
-- This is __not__ a valid program in Harper's language, and loops
-- forever on "properly" coinductive terms like 'omega'. However, it's
-- very useful for testing our definitions, since it allows us to
-- visualize coinductive terms.
_coiToInd :: Functor t_tau => Coi t_tau -> Ind t_tau
_coiToInd c = Fold $ _coiToInd <$> unfold c

mainConat :: IO ()
mainConat = do
  flip mapM_ [z, s z, s (s z), s (s (s z))] $ \n -> do
    printf "expand (natToConat %s) = %s\n"
      (prettyNat n)
      (prettyNat . _coiToInd . natToConat $ n)
  printf "take 100 (prettyNat (expand omega)) = %s\n"
    (take 100 . prettyNat . _coiToInd $ omega)

----------------------------------------------------------------
-- * Isomorphisms
--
-- As discussed in Section 15.4.

-- | Hard half of isomorphism between @Ind t_tau@ and @t_tau (Ind t_tau)@.
--
-- The other direction is just 'Fold'.
unFold :: forall t_tau. Functor t_tau => Ind t_tau -> t_tau (Ind t_tau)
unFold = snd . go
  where
    go :: Ind t_tau -> (Ind t_tau, t_tau (Ind t_tau))
    go = rec @t_tau go'
    -- My @fmap fst@, @fmap snd@ fix was essentially right, but I also
    -- needed to add 'Fold's in the right places.
    go' :: t_tau (Ind t_tau, t_tau (Ind t_tau)) -> (Ind t_tau, t_tau (Ind t_tau))
    go' x = (Fold $ fmap fst x, fmap (Fold . snd) x)

mainIsom :: IO ()
mainIsom = do
  printf "unFold threeNat = %s\n" (show $ fmap prettyNat $ unFold threeNat)
  printf "Fold (unFold threeNat) = %s\n" (prettyNat $ Fold (unFold threeNat))

-- | Exercise: hard half of isomorphism between @Coi t_tau@ and @t_tau (Coi t_tau)@.
--
-- The other direction is just 'unfold'.
ununfold :: forall t_tau. Functor t_tau => t_tau (Coi t_tau) -> Coi t_tau
ununfold = undefined

----------------------------------------------------------------
-- * Deep embedding of positive functors

-- | Data kind of strictly positive polynomial functors without
-- arrows.
data{-kind-} F = F :+: F
               | F :*: F
               | forall b. T b -- ^ Base type.
               | X             -- ^ Type variable

-- | Expressions of type @[a/X]tau@, where @tau@ is a polynomial
-- functor.
data E (tau :: F) a where
  -- | Left sum intro.
  L :: E f1 a -> E (f1 :+: f2) a
  -- | Right sum intro.
  R :: E f2 a -> E (f1 :+: f2) a
  -- | Pair intro.
  P :: E f1 a -> E f2 a -> E (f1 :*: f2) a
  -- | Base type intro.
  B :: Show b => b -> E (T b) a
  -- | Term of variable type intro.
  V :: a -> E X a
deriving instance Functor (E tau)
deriving instance Show a => Show (E tau a)

----------------------------------------------------------------
-- * Eliminators for 'E'
--
-- Using these eliminators allows us to avoid pattern matching on 'E'
-- expressions in our solutions, which ensures we're working in
-- Harper's Ch 15 language.

-- | Left product elimination.
l :: E (f1 :*: f2) a -> E f1 a
l (P x _) = x

-- | Right product elimination.
r :: E (f1 :*: f2) a -> E f2 a
r (P _ y) = y

-- | Sum elimination.
caseOf :: (E f1 a -> b) -> (E f2 a -> b) -> E (f1 :+: f2) a -> b
caseOf x_e1 y_e2 e = case e of
  L x -> x_e1 x
  R y -> y_e2 y

-- | Variable type elimination.
v :: E X a -> a
v (V x) = x

-- | Base type elimination.
b :: E (T b) a -> b
b (B x) = x

----------------------------------------------------------------
-- * Inductive nats via 'E'-functors
--
-- This repeats the examples from the first definition of nats above,
-- but using 'E'-functors.

-- | Polynomial functor for nats.
type Nat'F = T () :+: X
-- | Nats.
type Nat' = Ind (E Nat'F)

-- | Eliminator for 'Nat'F'.
--
-- Note that 'Nat'F' isomorphic to 'Maybe'!
--
-- We can use this to simplify pattern matching on 'Nat'F'; compare
-- in 'addNat''.
caseNat'F :: b -> (a -> b) -> E Nat'F a -> b
caseNat'F caseZ caseS =
  caseOf (const caseZ) (caseS . v)

-- | Zero.
z' :: Nat'
z' = Fold (L (B ()))

-- | Successor.
s' :: Nat' -> Nat'
s' n = Fold (R (V n))

-- | Pretty print a nat.
prettyNat' :: Nat' -> String
prettyNat' = rec @(E Nat'F) x_e1
  where
    x_e1 :: E Nat'F String -> String
    -- Equivalent 'caseNat'F' version:
    -- x_e1 = caseNat'F "z'" ("s' "++)
    x_e1 = caseOf (\_ -> "z'")
                  (\x -> "s' "++v x)

threeNat' :: Nat'
threeNat' = s' (s' (s' z'))

addNat' :: Nat' -> Nat' -> Nat'
addNat' m n = rec @(E Nat'F) x_e1 m
  where
    x_e1 :: E Nat'F Nat' -> Nat'
    -- Equivalent 'caseNat'F' version:
    -- x_e1 = caseNat'F n s'
    x_e1 = caseOf (\_ -> n)
                  (\x -> s' (v x))
sixNat' :: Nat'
sixNat' = addNat' threeNat' threeNat'

mainNat' :: IO ()
mainNat' = do
  printf "threeNat' = %s\n" (prettyNat' threeNat')
  printf "sixNat' = %s\n" (prettyNat' sixNat')
  printf "unFold threeNat' = %s\n" (show $ fmap prettyNat' $ unFold threeNat')
  printf "Fold (unFold threeNat') = %s\n" (prettyNat' $ Fold (unFold threeNat'))

----------------------------------------------------------------
-- * Print examples

main :: IO ()
main = do
  mapM_ (uncurry present) actions
  where
    actions :: [(String, IO ())]
    actions =
      [ ("Nat", mainNat)
      , ("Isom", mainIsom)
      , ("Nat'", mainNat')
      , ("Conat", mainConat) ]

    -- Present a main.
    present :: String -> IO () -> IO ()
    present name action = do
      printf "%s\n" (title name)
      action
      printf "\n"

    -- Print a name centered in a line of hashes.
    title :: String -> String
    title name = do
      let len = length name
      let leftLen = len `div` 2
      let rightLen = (len + 1) `div` 2
      let halfWidth = 40
      printf "%s %s %s"
        (replicate (halfWidth - leftLen + 1) '#')
        name
        (replicate (halfWidth - rightLen + 1) '#')
