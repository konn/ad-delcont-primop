module Data.PRef (PRef, newPRef, readPRef, writePRef, modifyPRef') where

import Control.Monad.Primitive
import Data.Primitive

newtype PRef s a = PRef {unPRef :: MutablePrimArray s a}

newPRef :: (PrimMonad m, Prim a) => a -> m (PRef (PrimState m) a)
{-# INLINE newPRef #-}
newPRef = \x -> do
  arr <- newPrimArray 1
  writePrimArray arr 0 x
  pure $ PRef arr

readPRef :: (PrimMonad m, Prim a) => PRef (PrimState m) a -> m a
{-# INLINE readPRef #-}
readPRef = flip readPrimArray 0 . unPRef

writePRef :: (PrimMonad m, Prim a) => PRef (PrimState m) a -> a -> m ()
{-# INLINE writePRef #-}
writePRef = flip writePrimArray 0 . unPRef

modifyPRef' :: (PrimMonad m, Prim a) => PRef (PrimState m) a -> (a -> a) -> m ()
{-# INLINE modifyPRef' #-}
modifyPRef' pr f = writePRef pr . f =<< readPRef pr
