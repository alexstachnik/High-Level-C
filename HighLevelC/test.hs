{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

newtype M a = M {runM :: StateT Int Identity a}
            deriving (Monad,
                      Applicative,
                      Functor,
                      MonadState Int,
                      MonadFix)


runM_ c = runIdentity $ evalStateT (runM c) 0

f a = do
  x <- get
  case x of
    0 -> do
      put 1
      y <- get
      n <- f a
      case n of
        500 -> return 15
        _ -> return (y+2)
    1 -> return a



