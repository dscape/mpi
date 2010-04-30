-- (c) M.P.I 2005/06 -- Created: 05.12.14

module SMmonad where

import Mpi
import Smonad

data MST m s a = MST { mst :: (s -> m(a, s)) }

instance (Monad m) => Monad (MST m s) where
   return a = MST (return . (split (const a) id))
   (MST f) >>= g = MST (\s -> do { (a,s') <- f s ;
                                   let MST k = g a
				   in  k s' })

-- inject ST into MST

iST :: (Monad m) => ST s a -> MST m s a
iST = MST . (return .) . st

-- inject M into MST

iM :: (Monad m) => m a -> MST m s a
iM x = MST(\s -> do { a <- x ; return (a,s) })

-- extract M from MST

xM :: (Monad m,Functor m) => MST m s a -> s -> m a
xM x = (fmap p1) . (mst x)

-- that is: xM x s = do { (a,_) <- mst x s ; return a }

runMST :: (Monad m, Functor m) => MST m s a -> s -> m a
runMST x i  = (xM x) i
