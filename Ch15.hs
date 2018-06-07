{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PFPL Ch 15: Inductive and Coinductive Types.
import Text.Printf

-- Note on notation: I can't use dots ('.') in Haskell names, so I'm
-- using underscores instead. E.g. you should read @t_tau@ as
-- @t.tau@. Also, in Haskell we do substitution by application, so
-- e.g. @[tau'/t]tau@ is written as application @(t.tau) tau'@, which
-- in turn is @t_tau tau'@ in the Haskell below.

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
omega = Gen (\x -> NatS x) (NatZ)

-- | Exercise 15.1: embed @Nat@ into @Conat@.
--
-- Not sure how to do this one ...

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
-- * Print examples

main :: IO ()
main = do
  mainNat
  mainIsom
