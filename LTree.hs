-- (c) M.P.I 1998/99-2001/02 -- Date: 05.11.17

module LTree where

import Mpi

-- (1) Datatype definition -----------------------------------------------------

data LTree a = Leaf a | Fork (LTree a, LTree a) deriving Show

inLTree = either Leaf Fork

outLTree :: LTree a -> Either a (LTree a,LTree a)
outLTree (Leaf a)     = i1   a
outLTree (Fork (t1,t2)) = i2    (t1,t2)

-- (2) Ana + cata + hylo -------------------------------------------------------

cataLTree a = a . (recLTree (cataLTree a)) . outLTree

anaLTree f = inLTree . (recLTree (anaLTree f) ) . f

hyloLTree a c = cataLTree a . anaLTree c

baseLTree g f = g -|- (f >< f)

recLTree f = baseLTree id f

-- (3) Map ---------------------------------------------------------------------

instance Functor LTree
         where fmap f = cataLTree ( inLTree . baseLTree f id )

-- (4) Examples ----------------------------------------------------------------

-- (4.0) Inversion (mirror) ----------------------------------------------------

invLTree = cataLTree (inLTree . (id -|- swap))

{-- Cf:
invLTree (Leaf a) = Leaf a
invLTree (Fork (a,b)) = Fork (invLTree b,invLTree a)
--}

-- (4.1) Flatten ---------------------------------------------------------------

tips = cataLTree (either singl conc)

-- where

singl   x = [x]
conc(l,r)= l ++ r

-- (4.2) Double factorial ------------------------------------------------------

dfac 0 = 1
dfac n = (hyloLTree (either id mul) dfacd) (1,n) where mul(x,y)=x*y

dfacd(n,m) | n==m      = i1   n
           | otherwise = i2   ((n,k),(k+1,m))
                         where k = div (n+m) 2

-- (4.3) Double square function ------------------------------------------------

-- recall sq' in RList.hs in...

dsq' 0 = 0
dsq' n = (cataLTree (either id add) . fmap (\n->2*n-1) . (anaLTree dfacd)) (1,n)

--where

add(x,y)=x+y

-- that is

dsq 0 = 0
dsq n = (hyloLTree (either id add) (fdfacd nthodd)) (1,n)
	where	nthodd n = 2*n - 1 
		fdfacd f (n,m) | n==m  = i1   (f n)
				  | otherwise = i2   ((n,k),(k+1,m))
						where k = div (n+m) 2

-- (4.4) Fibonacci -------------------------------------------------------------

fib =  hyloLTree (either (const 1) add) fibd

-- where

fibd n | n < 2     = i1   ()
       | otherwise = i2   (n-1,n-2)

-- (4.5) Merge sort ------------------------------------------------------------

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort l = hyloLTree (either singl merge) lsplit l

--where

merge (l,[])                  = l
merge ([],r)                  = r
merge (x:xs,y:ys) | x < y     = x : merge(xs,y:ys) 
                  | otherwise = y : merge(x:xs,ys)

lsplit [x] = i1 x
lsplit l   = i2 (sep l)

sep []    = ([],[])
sep (h:t) = let (l,r) = sep t in (h:r,l)  -- a List cata

{-- pointwise version:

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort l = let (l1,l2) = sep l
          in merge(mSort l1, mSort l2)
--}

--------------------------------------------------------------------------------

