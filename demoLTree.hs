import LTree
import Exp

--- Aux ------------------------------------------------------------------------
cLTree2Exp = cataLTree (either Var h)
	     where h(a,b) = Term "Split" [a,b] 

-- Double factorial -----------------------------------------------------------
dfac_visual 0 = (expDisplay "_.html") (Var "1")
dfac_visual n = ((expDisplay "_.html") . cLTree2Exp .
		 (fmap show) . (anaLTree dfacd)) (1,n)
--- Double factorial -----------------------------------------------------------
fib_visual n = ((expDisplay "_.html") . cLTree2Exp .
		 (fmap show) . (anaLTree fibd)) n
--- Mergesort ------------------------------------------------------------------
mSort_visual [] = (expDisplay "_.html") (Var " ")
mSort_visual l = ((expDisplay "_.html") . cLTree2Exp .
		 (fmap show) . (anaLTree lsplit )) l

