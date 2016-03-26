{-# LANGUAGE MultiParamTypeClasses #-}
module Types.PrimeField() where

import Structures.BasicStructures(Group,AbelianGroup,Ring,CR1,PID,ED,Field,
                                  add,minus,neg,zero,mul,one,
                                  pidgcd,pidxgcd,divrem,inv,fieldDiv)

data PrimeField = PrimeField

data PrimeFieldElt = PrimeFieldElt


