{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Fleet.Array.MutVar where

import GHC.Exts
import Data.Kind
import GHC.Base

import Fleet.Array.Lift

type MutVar :: UnliftedType -> Type
data MutVar a = MV (MutVar# RealWorld a)

newMutVar :: Lift a -> IO (MutVar a)
newMutVar (Lift x) = IO $ \s ->
  case newMutVar# x s of
    (# s', v #) -> (# s', MV v #)

readMutVar :: MutVar a -> IO (Lift a)
readMutVar (MV v) = IO $ \s -> case readMutVar# v s of (# s', x #) -> (# s', Lift x #)

writeMutVar :: MutVar a -> Lift a -> IO ()
writeMutVar (MV v) (Lift x) = IO $ \s -> (# writeMutVar# v x s, () #)

-- This module is intended to be combined with domain specific pattern synonyms
-- like this:
--
-- type ArrayData a = Lift (ArrayData# a)
-- pattern Current :: MutArray a -> ArrayData a
-- pattern Current x = Lift (Current# x)
-- pattern Diff :: Op a -> ArrayVar a -> ArrayData a
-- pattern Diff op v = Lift (Diff# op v)
-- {-# COMPLETE Current, Diff #-}
--
-- This is a zero-cost abstraction, as long as you always immediately pattern
-- match using these synonyms and you enable -O2.