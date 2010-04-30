-- MP1 2006-07 ---  Aula 2 (Monads)
-- lsb@di.uminho.pt



-- Calculador de expressoes (versao nao monadica)

data Termo = Ct Int | Prd Termo Termo | Quo Termo Termo

instance  Show Termo where
  show (Ct x) = "Ct(" ++ show x  ++ ")"
  show (Prd x y) = "Prd(" ++ show x ++ " " ++ show y  ++ ")"
  show (Quo x y) = "Quo(" ++ show x ++ " " ++ show y ++ ")" 

t1 = Quo (Ct 400) (Quo (Ct 8) (Ct 2))
t3 = Prd (Ct 5) (Quo (Ct 400) (Quo (Ct 8) (Ct 2)))
t2 = Prd (Ct 4) (Quo (Ct 400) (Quo (Ct 8) (Ct 0)))



-- Calculador simples

avalia :: Termo -> Int
avalia (Ct x)    = x
avalia (Prd x y) = (avalia x) * (avalia y)
avalia (Quo x y) = (avalia x) `div` (avalia y)



-- Calculador  com tratamento de excepcoes

data Excep a = Sinaliza Erro | Retorna a
type Erro = String

instance Show a => Show (Excep a) where
  show (Sinaliza e)  = "Erro:" ++ e
  show (Retorna x)   = "Valor:" ++ show x


avalia1 :: Termo -> Excep Int
avalia1 (Ct x) = Retorna x
avalia1 (Prd x y)  
  = f (avalia1 x)
    where 
    f (Sinaliza e) = Sinaliza e
    f (Retorna m)  = f' (avalia1 y)
                     where 
                     f' (Sinaliza e') = Sinaliza e'
                     f' (Retorna n)  =  Retorna (m * n)
avalia1 (Quo x y)   
  = f (avalia1 x)
    where 
    f (Sinaliza e) = Sinaliza e
    f (Retorna m)  = f' (avalia1 y)
                     where 
                     f' (Sinaliza e') = Sinaliza e'
                     f' (Retorna n)  =  if (n == 0) 
                                        then Sinaliza " Divisao por zero " 
                                        else Retorna (m `div` n)




-- Calculador  com contador de operacoes

newtype Memoria a = Mem (Estado -> (a, Estado))
type Estado = Int

trans :: Memoria a -> Estado -> (a, Estado)
trans (Mem f) s = f s

instance Show a => Show (Memoria a) where
  show f = "Valor:" ++ show x ++ ", Contador:" ++ show s
           where (x,s) = trans f 0


avalia2 :: Termo -> Memoria Int
avalia2 (Ct x) = Mem f
                 where f s = (x,s)
avalia2 (Prd x y) = Mem f
                    where
                    f s = (n * m, s2+1)
                          where
                          (n,s1)  = trans (avalia2 x) s
                          (m,s2)  = trans (avalia2 y) s1
avalia2 (Quo x y) = Mem f
                    where
                    f s = (n `div` m, s2+1)
                          where
                          (n,s1)  = trans (avalia2 x) s
                          (m,s2)  = trans (avalia2 y) s1




-- Calculador  com debug

newtype Out a  = Mostra (Output, a)
type Output    = String

instance Show a => Show (Out a) where
   show (Mostra (t,v)) = t ++ "Valor: " ++ show v 

linha  :: Termo -> Int -> Output
linha t x = "O termo " ++ show t ++ " reduz a " ++ show x ++ "\n"


avalia3 :: Termo -> Out Int
avalia3 (Ct x) = Mostra (linha (Ct x) x, x)
avalia3 (Prd x y) = Mostra (sm ++ sn ++ linha (Prd x y) z, z)
                    where Mostra (sm, m) = avalia3 x
                          Mostra (sn, n) = avalia3 y
                          z              = m * n
avalia3 (Quo x y) = Mostra (sm ++ sn ++ linha (Quo x y) z, z)
                    where Mostra (sm, m) = avalia3 x
                          Mostra (sn, n) = avalia3 y
                          z              = m `div` n

                     
