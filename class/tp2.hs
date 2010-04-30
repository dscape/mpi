module TP2 where

import Data.Maybe
import Control.Monad

data Exp = Const Int | Var String | Prod Exp Exp | Div Exp Exp

-- 3*(4/x)
exp1 = Prod (Const 3) (Div (Const 4) (Var "x"))
-- (3/(x/2))*2
exp2 = Prod (Div (Const 3) (Div (Var "x") (Const 2))) (Const 2) 

instance Show Exp where
    show (Const x)   = show x
    show (Var x)     = x
    show (Prod l r)  = "("++(show l)++"*"++(show r)++")"
    show (Div l r)   = "("++(show l)++"/"++(show r)++")"

type Dict = [(String,Int)]

eval :: Dict -> Exp -> Int
eval d (Const x)   = x
eval d (Var x)     = fromJust (lookup x d)
eval d (Prod l r)  = (eval d l) * (eval d r)
eval d (Div l r)   = (eval d l) `div` (eval d r)

-- Identidade

infix 5 >.>

(>.>) :: a -> (a -> b) -> b
x >.> f = f x

eval' :: Dict -> Exp -> Int
eval' d (Const x)  = x
eval' d (Var x)    = fromJust $ lookup x d
eval' d (Prod l r) = eval' d l >.> (\x -> eval' d r >.> (\y -> x * y))
eval' d (Div l r)  = eval' d l >.> (\x -> eval' d r >.> (\y -> x `div` y))


-- Parcialidade

evalm :: Dict -> Exp -> Maybe Int
evalm d (Const x)  = Just x
evalm d (Var x)    = lookup x d
evalm d (Prod l r) = case evalm d l
		     of Nothing -> Nothing
			Just x  -> case evalm d r
				   of Nothing -> Nothing
		 		      Just y  -> Just (x * y)
evalm d (Div l r)  = case evalm d l
		     of Nothing -> Nothing
			Just x  -> case evalm d r
				   of Nothing -> Nothing
				      Just y  -> if y==0 
						 then Nothing
						 else Just (x `div` y)

infix 5 >?>

(>?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >?> _ = Nothing
Just x  >?> f = f x 

evalm' :: Dict -> Exp -> Maybe Int
evalm' d (Const x)  = Just x
evalm' d (Var x)    = lookup x d
evalm' d (Prod l r) = evalm' d l >?> (\x -> evalm' d r >?> (\y -> Just (x * y)))
evalm' d (Div l r)  = evalm' d l >?> (\x -> evalm' d r >?> 
		      (\y -> if y==0 then Nothing else Just (x `div` y)))

-- Propagação de erros

data Erro a = Erro String | OK a

instance Show a => Show (Erro a) where
    show (Erro s) = "Erro: "++s
    show (OK x)   = show x

evale :: Dict -> Exp -> Erro Int
evale d (Const x)  = OK x
evale d (Var x)    = if (x `elem` map fst d)
		     then OK $ fromJust $ lookup x d
		     else Erro "Variável não definida!" 
evale d (Prod l r) = case evale d l
		     of Erro e -> Erro e
			OK x   -> case evale d r
				  of Erro m -> Erro m
		 		     OK y   -> OK (x * y)
evale d (Div l r)  = case evale d l
		     of Erro e -> Erro e
			OK x   -> case evale d r
				  of Erro m -> Erro m
				     OK y   -> if y==0 
					       then Erro "Divisão por zero!"
						    else OK (x `div` y)

infix 5 >!>

(>!>) :: Erro a -> (a -> Erro b) -> Erro b
Erro e >!> _ = Erro e
OK x   >!> f = f x 

evale' :: Dict -> Exp -> Erro Int
evale' d (Const x)  = OK x
evale' d (Var x)    = if (x `elem` map fst d)
		      then OK $ fromJust $ lookup x d
		      else Erro "Variável não definida!" 
evale' d (Prod l r) = evale' d l >!> (\x -> evale' d r >!> (\y -> OK (x * y)))
evale' d (Div l r)  = evale' d l >!> (\x -> evale' d r >!> (\y -> if y==0 then Erro "Divisão por zero!" else OK (x `div` y)))


-- Não determinismo

procura :: Eq a => a -> [(a,b)] -> [b]
procura x d = [v | (y,v) <- d, x==y]

evall :: Dict -> Exp -> [Int]
evall d (Const x)  = [x]
evall d (Var x)    = procura x d
evall d (Prod l r) = [x * y | x <- evall d l, y <- evall d r]
evall d (Div l r)  = [x `div` y | x <- evall d l, y <- evall d r, y/=0]

infix 5 >*>

(>*>) :: [a] -> (a -> [b]) -> [b]
l >*> f = [y | x <- l, y <- f x]

evall' :: Dict -> Exp -> [Int]
evall' d (Const x)  = [x]
evall' d (Var x)    = procura x d
evall' d (Prod l r) = evall' d l >*> (\x -> evall' d r >*> (\y -> [x * y]))
evall' d (Div l r)  = evall' d l >*> (\x -> evall' d r >*> (\y -> if y==0 then [] else [x `div` y]))

{-
class Monad m where
    (>>=)   :: m a -> (a -> m b) -> m b
    return  :: a -> m a  

instance Monad Maybe where
    (>>=)  = (>?>)
    return = Just

instance Monad [] where
    (>>=)  = (>*>)
    return = (:[])

d >>= \x -> e == do {x <- d; e}
-}

evalM :: Dict -> Exp -> Maybe Int
evalM d (Const x)  = return x
evalM d (Var x)    = lookup x d
evalM d (Prod l r) = do x <- evalM d l
			y <- evalM d r
			return (x * y)
evalM d (Div l r)  = do x <- evalM d l
			y <- evalM d r
			if (y==0) then Nothing
			          else return (x `div` y)

evalL :: Dict -> Exp -> [Int]
evalL d (Const x)  = return x
evalL d (Var x)    = procura x d
evalL d (Prod l r) = do x <- evalL d l
			y <- evalL d r
			return (x * y)
evalL d (Div l r)  = do x <- evalL d l
			y <- evalL d r
			if (y==0) then []
			          else return (x `div` y)

instance Monad Erro where
    (>>=)  = (>!>)
    return = OK

evalE :: Dict -> Exp -> Erro Int
evalE d (Const x)  = return x
evalE d (Var x)    = if (x `elem` map fst d)
		     then OK $ fromJust $ lookup x d
		     else Erro "Variável não definida!"
evalE d (Prod l r) = do x <- evalE d l
			y <- evalE d r
			return (x * y)
evalE d (Div l r)  = do x <- evalE d l
			y <- evalE d r
			if (y==0) then Erro "Divisão por zero!"
			          else return (x `div` y)

-- IO

evalIO :: Dict -> Exp -> IO Int
evalIO d (Const x)  = return x
evalIO d (Var x)    = if (x `elem` map fst d)
		      then return $ fromJust $ lookup x d
		      else do putStr $ "Qual o valor de "++x++"? "
			      readLn
evalIO d (Prod l r) = do x <- evalIO d l
			 y <- evalIO d r
			 return (x * y)
evalIO d (Div l r)  = do x <- evalIO d l
			 y <- evalIO d r
			 return (x `div` y)

{-
class Monad m => MonadPlus m where
    mzero  :: m a
    mplus  :: m a -> m a -> m a

instance MonadPlus Maybe where
    mzero = Nothing
    Just x  `mplus` _ = Just x
    Nothing `mplus` y = y

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: MonadPlus m => Bool -> m ()
guard False = mzero
guard True  = return ()
-}

lookupM :: (MonadPlus m, Eq a) => a -> [(a,b)] -> m b
lookupM _ []    = mzero
lookupM x ((y,v):t) | x==y = return v `mplus` lookupM x t
		    | otherwise = lookupM x t

geval :: MonadPlus m => Dict -> Exp -> m Int
geval d (Const x)  = return x
geval d (Var x)    = lookupM x d
geval d (Prod l r) = do x <- geval d l
			y <- geval d r
			return (x * y)
geval d (Div l r)  = do x <- geval d l
			y <- geval d r
			guard  (y/=0)
			return (x `div` y)
