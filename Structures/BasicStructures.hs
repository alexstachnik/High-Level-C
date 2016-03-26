{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Structures.BasicStructures(Group,AbelianGroup,Ring,CR1,PID,ED,Field,
                                  add,minus,neg,zero,mul,one,
                                  pidgcd,pidxgcd,divrem,inv,fieldDiv) where

import Monads.Code(Code)

class Group s elt | elt -> s where
  add :: s -> elt -> elt -> Code elt
  minus :: s -> elt -> elt -> Code elt
  neg :: s -> elt -> Code elt
  zero :: s -> Code elt

class (Group s elt) => AbelianGroup s elt | elt -> s

class (AbelianGroup s elt) => Ring s elt | elt -> s where
  mul :: s -> elt -> elt -> Code elt

class (Ring s elt) => CR1 s elt | elt -> s where
  one :: s -> Code elt

class (CR1 s elt) => PID s elt | elt -> s where
  pidgcd :: s -> elt -> elt -> Code elt
  pidxgcd :: s -> elt -> elt -> Code (elt,elt,elt)

class (PID s elt) => ED s elt | elt -> s where
  divrem :: s -> elt -> elt -> Code (elt,elt)

class (ED s elt) => Field s elt | elt -> s where
  inv :: s -> elt -> Code elt
  fieldDiv :: s -> elt -> elt -> Code elt
