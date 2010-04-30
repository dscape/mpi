module TP3 where

import Data.Maybe
import Control.Monad
import Prelude hiding (getLine,putStr)
import System.IO.Unsafe

-- IO

caos = let x = unsafePerformIO (getInt)
       in  x + x

putStr :: String -> IO ()
putStr [] = return ()
putStr (h:t) = putChar h >>= \_ -> putStr t
--putStr (h:t) = putChar h >> putStr t
--putStr (h:t) = do putChar h
--		  putStr t

{-
(>>) :: Monad m => m a -> m b -> m b
f >> g = f >>= (\_ -> g)
-}

getLine :: IO String
getLine = getChar >>= 
	     \c -> if c == '\n'
		   then return ""
		   else getLine >>= (\s -> return (c:s)) 
{-
getLine = do c <- getChar
	     if c == '\n' 
		then return ""
		else do s <- getLine
			return (c:s)
-}

getInt :: IO Int
getInt = getLine >>= (\s -> return (read s))


data Exp = Const Int | Var String | Prod Exp Exp | Div Exp Exp

-- 3*(4/x)
exp1 = Prod (Const 3) (Div (Const 4) (Var "x"))
-- (3/(x/2))*y
exp2 = Prod (Div (Const 3) (Div (Var "x") (Const 2))) (Var "y") 

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


evalio :: Dict -> Exp -> IO Int
evalio d (Const x)  = return x
evalio d (Var v)    = case lookup v d
		      of Just x  -> return x
			 Nothing -> do putStr $ v++"? " 
				       getInt
evalio d (Prod l r) = do x <- evalio d l
			 y <- evalio d r
			 return (x * y)
evalio d (Div l r)  = do x <- evalio d l
			 y <- evalio d r
			 return (x `div` y)

-- Reader

eval' :: Exp -> (Dict -> Int)
eval' (Const x)   = \d -> x
eval' (Var x)     = \d -> fromJust (lookup x d)
eval' (Prod l r)  = \d -> (eval' l d) * (eval' r d)
eval' (Div l r)   = \d -> (eval' l d) `div` (eval' r d)

newtype Reader c a = Reader {runReader :: c -> a}

-- runReader :: Reader c a -> (c -> a)

instance Monad (Reader c) where
    -- return :: a -> Reader c a
    return x = Reader (\c -> x)
    -- (>>=) :: Reader c a -> (a -> Reader c b) -> Reader c b
    Reader f >>= g = Reader (\c -> runReader (g (f c)) c)

ask :: Reader c c
ask = Reader id

evalr :: Exp -> Reader Dict Int
evalr (Const x)  = return x
evalr (Var v)    = do d <- ask
		      return (fromJust (lookup v d))
evalr (Prod l r) = do x <- evalr l
		      y <- evalr r
		      return (x * y)
evalr (Div l r)  = do x <- evalr l
		      y <- evalr r
		      return (x `div` y)

teste = runReader (evalr exp1) [("x",2)]


data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

tree1 = Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty (Node 'd' Empty Empty))

decora :: b -> Tree a -> Tree b
decora x t = runReader (aux t) x
    where aux :: Tree a -> Reader b (Tree b)
	  aux Empty         = return Empty
	  aux (Node _ l r)  = do e <- aux l
				 d <- aux r
				 x <- ask
				 return (Node x e d)
