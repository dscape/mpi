-- (c) M.P.I 1998/99-2005/06 -- Date: 05.11.14

module BTree where

import Mpi
import HugsList

-- (1) Datatype definition -----------------------------------------------------

data BTree a = Empty | Node(a, (BTree a, BTree a)) deriving Show

inBTree :: Either () (b,(BTree b,BTree b)) -> BTree b
inBTree = either (const Empty) Node

outBTree :: BTree a -> Either () (a,(BTree a,BTree a))
outBTree Empty              = Left ()
outBTree (Node (a,(t1,t2))) = Right(a,(t1,t2))

-- (2) Ana + cata + hylo -------------------------------------------------------

baseBTree f g = id -|- (f >< (g >< g))

cataBTree g = g . (recBTree (cataBTree g)) . outBTree

anaBTree g = inBTree . (recBTree (anaBTree g) ) . g

hyloBTree h g = cataBTree h . anaBTree g

recBTree f = baseBTree id f

-- (3) Map ---------------------------------------------------------------------

instance Functor BTree
         where fmap f = cataBTree ( inBTree . baseBTree f id )

-- (4) Examples ----------------------------------------------------------------

-- (4.1) Quicksort -------------------------------------------------------------

qSort :: Ord a => [a] -> [a]
qSort = hyloBTree inord qsep -- = (cataBTree inord) . (anaBTree qsep)

-- where

inord = either (const []) join

join(x,(l,r))=l++[x]++r

qsep []    = Left ()
qsep (h:t) = Right (h,(s,l)) where (s,l) = part (<h) t

part:: (a -> Bool) -> [a] -> ([a], [a])
part p []                = ([],[])
part p (h:t) | p h       = let (s,l) = part p t in (h:s,l)
             | otherwise = let (s,l) = part p t in (s,h:l)

{-- pointwise versions:
qSort [] = []
qSort (h:t) = let (t1,t2) = part (<h) t
	      in  qSort t1 ++ [h] ++ qSort t2

or, using list comprehensions:

qSort [] = []
qSort (h:t) = qSort [ a | a <- t , a < h ] ++ [h] ++ qSort [ a | a <- t , a >= h ]

--}

-- (4.2) Traces ----------------------------------------------------------------

traces :: Eq a => BTree a -> [[a]]
traces = cataBTree (either (const [[]]) put)

-- where

put(a,(l,r)) = union (map (a:) l) (map (a:) r) 

-- (4.3) Towers of Hanoi -------------------------------------------------------

hanoi = hyloBTree present strategy

--- where

present = either (const []) join

strategy(d,0) = Left ()
strategy(d,n+1) = Right ((n,d),((not d,n),(not d,n)))

{--
    The Towers of Hanoi problem comes from a puzzle marketed in 1883
    by the French mathematician Édouard Lucas, under the pseudonym
    Claus. The puzzle is based on a legend according to which
    there is a temple, apparently in Bramah rather than in Hanoi as
    one might expect, where there are three giant poles fixed in the
    ground. On the first of these poles, at the time of the world's
    creation, God placed sixty four golden disks, each of different
    size, in decreasing order of size. The Bramin monks were given
    the task of moving the disks, one per day, from one pole to another
    subject to the rule that no disk may ever be above a smaller disk.
    The monks' task would be complete when they had succeeded in moving
    all the disks from the first of the poles to the second and, on
    the day that they completed their task the world would come to
    an end!
    
    There is a well­known inductive solution to the problem given
    by the pseudocode below. In this solution we make use of the fact
    that the given problem is symmetrical with respect to all three
    poles. Thus it is undesirable to name the individual poles. Instead
    we visualize the poles as being arranged in a circle; the problem
    is to move the tower of disks from one pole to the next pole in
    a specified direction around the circle. The code defines H n d
    to be a sequence of pairs (k,d') where n is the number of disks,
    k is a disk number and d and d' are directions. Disks are numbered
    from 0 onwards, disk 0 being the smallest. (Assigning number 0
    to the smallest rather than the largest disk has the advantage
    that the number of the disk that is moved on any day is independent
    of the total number of disks to be moved.) Directions are boolean
    values, true representing a clockwise movement and false an anti­clockwise
    movement. The pair (k,d') means move the disk numbered k from
    its current position in the direction d'. The semicolon operator
    concatenates sequences together, [] denotes an empty sequence
    and [x] is a sequence with exactly one element x. Taking the pairs
    in order from left to right, the complete sequence H n d prescribes
    how to move the n smallest disks one­by­one from one pole to the
    next pole in the direction d following the rule of never placing
    a larger disk on top of a smaller disk.
    
    H 0     d = [],
    H (n+1) d = H n ¬d ; [ (n, d) ] ; H n ¬d.
    
    (excerpt from R. Backhouse, M. Fokkinga / Information Processing
    Letters 77 (2001) 71--76)
    
--}

--------------------------------------------------------------------------------

