-- NB: unofficial, unstructured library (!) to be re-written soon

module Exp where

import Mpi

data Exp v o = Var v                -- expressões ou são variáveis
               | Term o [ Exp v o ] -- ou termos envolvendo operadores e
                                    -- subtermos
               deriving Show

data Cell a = ICell a Int Int | LCell a Int Int deriving Show

type Html a = [ Cell a ]

data Txt = Str String | Blk [ Txt ] deriving Show

inds :: [a] -> [Int]
inds [] = []
inds (h:t) = inds t ++ [succ (length t)]

seq2ff :: [a] -> [(Int,a)]
seq2ff = (uncurry zip) . (split inds id)

ff2seq m = map p2 m

txtFlat :: Txt -> [[Char]]
txtFlat (Str s) = [s]
txtFlat (Blk []) = []
txtFlat (Blk (a:l)) = txtFlat a ++ txtFlat (Blk l)

expFold :: (a -> b) -> (c -> [b] -> b) -> Exp a c -> b
expFold f g (Var e) = f e
expFold f g (Term o l) = g o (map (expFold f g) l)

expPara :: (a -> b) -> (Exp a c -> c -> [b] -> b) -> Exp a c -> b
expPara f g (Var e) = f e
expPara f g (Term o l) = g (Term o l) o (map (expPara f g) l)

expBiFunctor f g = expFold (Var . f) h
                   where h a b = Term (g a) b

expLeaves :: Exp a b -> [a]
expLeaves = expFold singl h
            where singl a = [a]
                  h o l = foldr (++) [] l 

expWidth :: Exp a b -> Int
expWidth = length . expLeaves

expDepth :: Exp a b -> Int
expDepth = expFold (const 1) h
           where h o x = (succ . (foldr max 0)) x

exp2Html n (Var v) = [ LCell v n 1 ]
exp2Html n (Term o l) = g (expWidth (Term o l)) o (map (exp2Html (n-1)) l)
                        where g i o k = [ ICell o 1 i ] ++ (foldr (++) [] k)

html2Txt :: (a -> Txt) -> Html a -> Txt
html2Txt f = html . table . (foldr g u) 
             where u = Str "\n</tr>"
                   g c (Str s) = g c (Blk [Str s])
                   g (ICell a x y) (Blk b) = Blk ([ cell (f a) x y ] ++ b)
                   g (LCell a x y) (Blk b) = Blk ([ cell (f a) x y,  Str "\n</tr>\n<tr>"] ++ b)
                   html t = Blk [ Str("<html>\n<body bgcolor=\"#F4EFD8\" " ++
                                        "text=\"#260000\" link=\"#008000\" " ++
                                        "vlink=\"#800000\">\n"),
                                   t,
                                   Str "<html>\n"
                                 ]
                   table t = Blk [ Str "<table border=1 cellpadding=1 cellspacing=0>",
                               t,
                               Str "</table>\n"
                             ]
                   cell c x y = Blk [ Str("\n<td rowspan=" ++
                                            itoa y ++
                                            " colspan=" ++
                                            itoa x ++
                                            " align=\"center\"" ++
                                            ">\n"),
                                       c,
                                            Str "\n</td>"
                                     ]
                   itoa x = show x

expDisplay fn =
      (writeFile fn) . (foldr (++) []) . txtFlat . (html2Txt Str) .
      (uncurry exp2Html . (split expDepth id))

{--
--import Ffun
exp2ExpTar (Var v) =  [[1] |-> v]
exp2ExpTar (Term o l) = [[1] |-> o] `plus`
           let m = map exp2ExpTar l
               n = seq2ff m
               k = map f n
           in mPLUS k
--}

