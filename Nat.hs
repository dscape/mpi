-- (c) M.P.I 1998/99-2006/07 -- Date: 05.11.09

module Nat  where

import Mpi

-- (1) Datatype definition -----------------------------------------------------

-- data Nat = 0 | succ Nat   -- in fact: Haskell Integer is used as carrier type

inNat = either (const 0) succ

outNat 0 = Left ()
outNat (n+1) = Right n

-- (2) Ana + cata + hylo -------------------------------------------------------
-- to be completed
