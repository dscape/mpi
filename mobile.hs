-- (c) M.P.I 2005/02 -- Date: 05.12.14
---
--- mobile phone directory example,
--- cf. slides 10 to 12 in www.di.uminho.pt/~jno/ps/msdn02.zip
---

import Mpi
import Smonad
import SMmonad

-- (a) functional layer ----------------------------------------

npush10 :: Eq a => a -> [a] -> [a]
npush10 n = (take 10) . (npush n)
            where npush n s = [n] ++ (filter (/=n) s)

-- (b) reactive layer (ST monadic) ------------------------------

store n = ST((split length id) . npush10 n)

del n = ST((split length id) . tail . npush10 n)

call n = ST((split head id) . npush10 n)

storebatch l = sequence_ (do i <- l ; return (store i))
                          -- cf [ store i | i <- l ] 

clear = update (const [])

-- (c) interactive layer (M=IO + ST) ------------------------------

mob :: MST IO [String] ()
mob = do { x <- iM(do { putStr "\n Call which number? " ;
			 getLine });
		 -- record call in list
         iST (call x) ;
		 -- actual call involves IO
         iM (writeFile "_call_number.txt" x) ; -- emulates actual calling protocol
		 -- show call list
         y <- iST (read_) ;
	 iM(putStr (show y))
       }

-- (d) main process ---------------------------------------------

main :: IO a
main = runMST (loop mob) []

