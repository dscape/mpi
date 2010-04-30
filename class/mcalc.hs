-- MP1 2006-07 ---  Aula 2 (Monads)
-- lsb@di.uminho.pt

-- Calculador de expressoes (versao monadica)

-- Termo

data Termo = Ct Int | Prd Termo Termo | Quo Termo Termo

instance  Show Termo where
  show (Ct x) = "Ct(" ++ show x  ++ ")"
  show (Prd x y) = "Prd(" ++ show x ++ " " ++ show y  ++ ")"
  show (Quo x y) = "Quo(" ++ show x ++ " " ++ show y ++ ")"

t1 = Quo (Ct 400) (Quo (Ct 8) (Ct 2))
t2 = Prd (Ct 4) (Quo (Ct 400) (Quo (Ct 8) (Ct 0)))
t3 = Prd (Ct 5) (Quo (Ct 400) (Quo (Ct 8) (Ct 2)))



-- Monad m = identidade

newtype Id a  = Virgem a

instance Monad Id where
   return x          = Virgem x
   (Virgem x) >>= c  = c x

instance Show a => Show (Id a) where
  show (Virgem x)   = "Valor: " ++ show x


mAvalia ::  Termo -> Id Int
mAvalia (Ct x)  =  return x
mAvalia (Prd x y) = do i <- mAvalia x
                       j <- mAvalia y
                       return (i * j)
mAvalia (Quo x y) = do i <- mAvalia x
                       j <- mAvalia y
                       return (i `div` j)



-- Monad m = excepcao

data Excep a = Sinaliza Erro | Retorna a
type Erro = String

instance Show a => Show (Excep a) where
  show (Sinaliza e)  = "Erro:" ++ e
  show (Retorna x)   = "Valor:" ++ show x


instance Monad Excep where
   return x    = Retorna x
   (Sinaliza e) >>= c = Sinaliza e
   (Retorna x)  >>= c = c x


mAvalia1 :: Termo -> Excep Int
mAvalia1 (Ct x)  =  return x
mAvalia1 (Prd x y) = do i <- mAvalia1 x
                        j <- mAvalia1 y
                        return (i * j)
mAvalia1 (Quo x y) = do i <- mAvalia1 x
                        j <- mAvalia1 y
                        if j == 0 
                           then Sinaliza "Divisao por zero"
                           else return (i `div` j)


-- Monad m = estado

newtype Memoria a = Mem (Estado -> (a, Estado))
type Estado = Int

trans :: Memoria a -> Estado -> (a, Estado)
trans (Mem f) s = f s

instance Show a => Show (Memoria a) where
  show f = "Valor:" ++ show x ++ ", Contador:" ++ show s
           where (x,s) = trans f 0


instance Monad Memoria where
   return x  = Mem f
               where f s = (x,s)
   p >>= q   = Mem f
               where f s = trans (q x) s'
                           where (x,s') = trans p s

incMem :: Memoria ()
incMem = Mem f where  f s = ((), s+1)


mAvalia2 :: Termo -> Memoria Int
mAvalia2 (Ct x)  =  return x
mAvalia2 (Prd x y) = do i <- mAvalia2 x
                        j <- mAvalia2 y
                        incMem
                        return (i * j)
mAvalia2 (Quo x y) = do i <- mAvalia2 x
                        j <- mAvalia2 y
                        incMem
                        return (i `div` j)



-- Monad m = output

newtype Out a  = Mostra (Output, a)
type Output    = String

instance Show a => Show (Out a) where
   show (Mostra (t,v)) = t ++ "Valor: " ++ show v

linha  :: Termo -> Int -> Output
linha t x = "O termo " ++ show t ++ " reduz a " ++ show x ++ "\n"



instance Monad Out where
   return x = Mostra (" ", x)
   p >>= q  = Mostra (outx ++ outy, y)
              where Mostra (outx, x) = p
                    Mostra (outy, y) = q x


mAvalia3 :: Termo -> Out Int
mAvalia3 (Ct x)  = do Mostra (linha (Ct x) x, ())
                      return x
mAvalia3 (Prd x y) = do i <- mAvalia3 x
                        j <- mAvalia3 y
                        Mostra (linha (Prd x y) (i * j), ())
                        return (i * j)
mAvalia3 (Quo x y) = do i <- mAvalia3 x
                        j <- mAvalia3 y
                        Mostra (linha (Quo x y) (i `div` j), ())
                        return (i `div` j)


