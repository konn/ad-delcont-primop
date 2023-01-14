{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Numeric.AD.DelCont.Native.Internal (
  PromptTag,
  newPromptTag,
  withPrompt,
  prompt,
  control0,
  shift,
) where

import Control.Arrow
import GHC.IO
import GHC.Prim
import GHC.ST

data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: ST s (PromptTag a)
{-# INLINE newPromptTag #-}
newPromptTag =
  unsafeIOToST $ IO $ \s ->
    case newPromptTag# s of
      (# s', tag #) -> (# s', PromptTag tag #)

prompt :: PromptTag a -> ST s a -> ST s a
{-# INLINE prompt #-}
prompt (PromptTag tag) =
  unsafeSTToIO >>> \case
    IO f -> unsafeIOToST $ IO $ prompt# tag f

withPrompt :: (PromptTag a -> ST s a) -> ST s a
withPrompt = (=<< newPromptTag)

shift :: PromptTag a -> ((ST s p -> ST s a) -> ST s a) -> ST s p
{-# INLINE shift #-}
shift p f = control0 p $ \k ->
  prompt p $ f $ prompt p . k

control0 :: PromptTag a -> ((ST s p -> ST s a) -> ST s a) -> ST s p
{-# INLINE control0 #-}
control0 (PromptTag tag) f =
  unsafeIOToST $
    IO $
      control0# tag $ \k ->
        case unsafeSTToIO $ f (unsafeSTToIO >>> \(IO a) -> unsafeIOToST $ IO $ k a) of
          IO p -> p
