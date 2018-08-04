import Text.Printf

data E = Var V
       | App E E
       | Lam T E
  deriving (Eq, Show)

type V = Int

data T = Base Char
       | Arr T T
  deriving (Eq, Show)

type C = [T]

type Error = String

die :: Error -> Either Error a
die = Left

infer :: C -> E -> Either Error T
infer c e = case e of
  Var i -> lookupVar c i
  App e1 e2 -> do
    t1 <- infer c e1
    t2 <- infer c e2
    case t1 of
      Arr t11 t12 | t11 == t2 -> return t12
      _ -> die $ printf "Expecting '%s' but got '%s'!"
           (show (Arr t2 (Base '_'))) (show t1)
  Lam t e -> do
    t2 <- infer (extendCtx c t) e
    return $ Arr t t2

lookupVar :: C -> V -> Either Error T
lookupVar c0 i0 = go c0 i0
  where
    go c i = case (c, i) of
      ([], _) -> die $ printf "Variable '%i' not bound in context '%s'!"
                 i0 (show c0)
      ((t:_), 0) -> return t
      ((t:c'), i) -> go c' (i-1)

extendCtx :: C -> T -> C
extendCtx c t = t:c

emptyCtx :: C
emptyCtx = []

eval :: E -> Either Error E
eval e = case e of
  Var i -> return $ Var i
  App e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    case v1 of
      Lam _ b -> eval $ sub 0 v2 b
      _ -> return $ App v1 v2
  Lam t b -> do
    -- We evaluate the bodies of lambdas, which is not typical.
    v <- eval b
    return $ Lam t v

sub :: V -> E -> E -> E
sub i e b = case b of
  Var j | i == j -> e
  -- Variable bound in an enclosing scope is now under one less
  -- lambda.
  Var j | j > i -> Var (j-1)
  Var j -> Var j
  App b1 b2 -> App (sub i e b1) (sub i e b2)
  -- Going under a binder so distance to binding site of substituted
  -- variable increases by one, and so substituted variable and free
  -- variables in substituted expression must be incremented.
  Lam t b' -> Lam t (sub (i+1) (weaken 0 e) b')

weaken :: Int -> E -> E
weaken i e = case e of
  Var j | j < i -> Var j
  Var j -> Var (j+1)
  App e1 e2 -> App (weaken i e1) (weaken i e2)
  Lam t b -> Lam t $ weaken (i+1) b

main :: IO ()
main = do
  print $ infer emptyCtx (Lam a (Var 0))
  print $ infer emptyCtx (Lam a (Var 1))
  print $ infer emptyCtx (Lam a (Lam b (Var 0)))
  print $ infer emptyCtx (Lam a (Lam b (Var 1)))

  -- (\x.\y.x y) (\x.x) |->* (\x.x)
  print $ eval (Lam (a `Arr` a) (Lam a (Var 1 `App` Var 0)) `App`
                Lam a (Var 0))
  where
    a,b,c :: T
    (a,b,c) = (Base 'A', Base 'B', Base 'C')
