module Data.URef (URef, newURef, readURef, writeURef, modifyURef') where

import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

newtype URef s a = URef {unURef :: MU.MVector s a}

newURef :: (MU.Unbox a, PrimMonad m) => a -> m (URef (PrimState m) a)
{-# INLINE newURef #-}
newURef = fmap URef . U.unsafeThaw . U.singleton

writeURef ::
  (MU.Unbox a, PrimMonad m) =>
  URef (PrimState m) a ->
  a ->
  m ()
writeURef = flip MU.unsafeWrite 0 . unURef

modifyURef' ::
  (MU.Unbox a, PrimMonad m) =>
  URef (PrimState m) a ->
  (a -> a) ->
  m ()
{-# INLINE modifyURef' #-}
modifyURef' = fmap ($ 0) . MU.unsafeModify . unURef

readURef ::
  (MU.Unbox a, PrimMonad m) =>
  URef (PrimState m) a ->
  m a
readURef = flip MU.unsafeRead 0 . unURef
