-- (c) M.P.I 1998/99-2006/07 -- Last Update: 06.10.31

module List  where

import Mpi
import Nat

-- (1) Datatype definition -----------------------------------------------------

--- Haskell lists are already defined, so this is a dummy declaration:
--- data [a] = [] | (a : [a])

inl = either (const []) (uncurry (:))

out []    = i1 ()
out (a:x) = i2(a,x)

-- (2) Ana + cata + hylo -------------------------------------------------------

cata g   = g . rec (cata g) . out   

ana h    = inl . (rec (ana h) ) . h

hylo g h = cata g . ana h

rec f    = id -|- id >< f

-- (3) Examples ----------------------------------------------------------------

-- (3.1) Factorial--------------------------------------------------------------

fac = hylo multiplying nats

-- where

multiplying = either (const 1) mul
mul = uncurry (*)

nats = (id -|- (split succ id)) . outNat

-- (3.1.1) Factorial (alternative) ---------------------------------------------

fac' = hylo (either (const 1) (mul . (succ >< id)))
                 ((id -|- (split id id)) . outNat)

{-- cf:

fac' = hylo (either (const 1) g) nats'
       where g(n,m) = (n+1) * m
             nats' 0 = i1 ()
             nats' (n+1) = i2 (n,n)
--}

-- (3.2) Square function -------------------------------------------------------

sq = hylo summing odds

summing = either (const 0) (uncurry (+))

odds 0 = i1 ()
odds (n+1) = i2 (2*n+1,n)

{-- pointwise:
sq 0 = 0
sq (n+1) = 2*n+1 + sq n

cf. Newton's binomial: (n+1)^2 = n^2 + 2n + 1

--}

-- (3.2.1) Square function reusing ana of factorial ----------------------------

sq' = (cata summing) . fmap (\n->2*n-1) . (ana nats)

-- (3.3) Insertion sort --------------------------------------------------------

iSort :: Ord a => [a] -> [a]
iSort = hylo (either (const []) insert) out

-- where

insert(x,[])              = [x]
insert(x,a:l) | x < a     = [x,a]++l
              | otherwise = a:(insert(x,l))

-- (3.4)  inversion -------------------------------------------------------

invl = cata (either (const [])(conc . swap . (singl >< id)))
       where singl a = [a]
	     conc = uncurry (++)

-- (3.5) Look-up function ------------------------------------------------------

look :: Eq a => a -> [(a,b)] -> Maybe b
look k = cata (either (const Nothing) aux)
         where aux((a,b),r)
                      | a == k    = Just b
                      | otherwise = r

--------------------------------------------------------------------------------

