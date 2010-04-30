-- (c) M.P.I 01/02 -- Last Update: 01.11.15
-- NB: this is not a library, it is just a script of demos.
-- The useful material can be found in library BTree.hs

import BTree
import Exp

--- Aux ------------------------------------------------------------------------
cBTree2Exp :: BTree a -> Exp [Char] a
cBTree2Exp = cataBTree (either (const (Var "nil")) h)
	     where h(a,(b,c)) = Term a [c,b] 

--- Quicksort ------------------------------------------------------------------
qSort_visual x = ((expDisplay "_.html") . cBTree2Exp .
                  (fmap show) . (anaBTree qsep)) x

--- Towers of Hanoi ------------------------------------------------------------
hanoi_visual =
	(expDisplay "_.html" .
        cBTree2Exp .
        (fmap show) .
        (anaBTree strategy))
--------------------------------------------------------------------------------

