module TP1 where

import Prelude hiding (sum,length,reverse)

-- Listas

sum :: [Int] -> Int
sum [] = 0
sum (h:t) = h + sum t
-- sum (h:t) = (\x y -> x + y) h (sum t)

reverse :: [a] -> [a]
reverse [] = []
reverse (h:t) = reverse t ++ [h]
-- reverse (h:t) = (\x y -> y ++ [x]) h (reverse t)


length :: [a] -> Int
length []    = 0
length (h:t) = 1 + length t

cat :: [a] -> [a] -> [a]
cat []    l = l
cat (h:t) l = h : cat t l

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (h:t) = f h (foldr f z t)
-}

len' :: [a] -> Int
len' = foldr (\h r ->  1 + r) 0

reverse' :: [a] -> [a]
reverse' = foldr (\x y -> y ++ [x]) []

sum' :: [Int] -> Int
sum' = foldr (\x y -> x + y) 0

cat' :: [a] -> [a] -> [a]
cat' l r = foldr (:) r l

cat'' :: [a] -> [a] -> [a]
cat'' = foldr (\h r -> (h:) . r) id

-- Será que esta é possível?

qsort :: Ord a => [a] -> [a]
qsort []    = []
qsort (h:t) = (qsort menores) ++ [h] ++ (qsort maiores)
      where menores = filter (<h)  t
            maiores = filter (>=h) t

-- Árvores

data Tree a = Empty | Node a (Tree a) (Tree a)

tree :: Tree Int
tree = Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)

height :: Tree a -> Int
height Empty        = 0
height (Node _ l r) = 1 + max (height l) (height r)

inorder :: Tree a -> [a]
inorder Empty        = []
inorder (Node x l r) = (inorder l) ++ [x] ++ (inorder r)

foldt :: (a -> b -> b -> b) -> b -> Tree a -> b
foldt f z Empty        = z
foldt f z (Node x l r) = f x (foldt f z l) (foldt f z r)

height' :: Tree a -> Int
height' = foldt (\_ l r -> 1 + max l r) 0

inorder' :: Tree a -> [a]
inorder' = foldt (\x l r -> l ++ [x] ++ r) []

-- Naturais

data Nat = Zero | Succ Nat deriving Show

one   = Succ Zero
two   = Succ one
three = Succ two

toint :: Nat -> Int
toint Zero     = 0
toint (Succ n) = 1 + toint n

soma :: Nat -> Nat -> Nat
soma n Zero     = n
soma n (Succ m) = Succ (soma n m)

foldn :: (a -> a) -> a -> Nat -> a
foldn f z Zero     = z
foldn f z (Succ n) = f (foldn f z n)

toint' :: Nat -> Int
toint' = foldn (+1) 0

soma' :: Nat -> Nat -> Nat
soma' n = foldn Succ n

-- Será que esta é possível?

downto :: Nat -> [Nat]
downto Zero     = []
downto (Succ n) = Succ n : downto n 