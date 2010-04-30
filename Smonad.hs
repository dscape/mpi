-- (c) M.P.I 2003/04 - 2006/07

module Smonad where

import Mpi

data ST s a = ST { st :: (s -> (a, s)) }

-- cf. com data Func s a = F { func  ::  s -> a  }


instance Monad (ST s) where
	 return a    = ST (split (const a) id)
-- ie:   return a    = ST (\s -> (a,s))
         (ST t) >>= f = ST (ap . ( (st . f) >< id) . t )
{-- ie:
         (ST f) >>= g = ST (\s -> let (a,s') = f s
                                    ST k    = g a
                                in k s')
--}

instance Functor (ST s) where
         fmap f t = do { a <- t ; return (f a) }
-- ie:   fmap f (ST g) = ST(\s -> let (a,s') = g s in (f a,s'))


-- program execution

exec (ST prog) s = prog s

-- generic state transformers 

read_   :: ST a a
read_ = ST(split id id)

update :: (a -> a) -> ST a a
update f = ST(split id f)

query :: (a -> b) -> ST a b
query f = ST(split f id)

write :: a  -> ST a a
write a = update (const a)

trans :: (a -> a) -> (a -> b) -> ST a b
-- a simple transation
trans g f = do { update g ; query f }

itrans :: (a -> b -> b) -> (a -> b -> c) -> a -> ST b c
-- a transation with input
itrans g f a = do { update (g a) ; query (f a)}

loop :: Monad t => t a -> t b
loop m = do { m ; loop m }

