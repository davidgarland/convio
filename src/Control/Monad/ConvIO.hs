-- |
-- Module      :  Control.Monad.ConvIO
-- Copyright   :  (c) David Garland 2020
-- License     :  MIT
-- 
-- Maintainer  :  David Garland <davidrgarland@me.com>
--
-- This module exposes a simple class `ConvIO` that comprises IO and ST, for
-- when you want to write abstractions over either. This is unsafe, obviously.
-- That being said, this module does value safety; `unsafeConvIfDet` and
-- `unsafeConvWhenDet` allow you to do things like only zeroing arrays in the ST
-- monad, so as to preserve determinism only where it's needed. Additionally,
-- this library is smaller than its main competitor, which would be the
-- primitive package with PrimMonad, which has a broader scope.

module Control.Monad.ConvIO where

import GHC.IO (IO(..))
import GHC.ST (ST(..))
import GHC.Exts (unsafeCoerce#)

class Monad m => ConvIO m where
  -- | Convert from IO to a ConvIO monad.
  unsafeConvFromIO :: IO a -> m a
  -- | Convert a ConvIO monad to IO.
  unsafeConvToIO :: m a -> IO a
  -- | Run the first monadic action if the monad is deterministic (e.g. in ST) or the second otherwise (e.g. in IO).
  unsafeConvIfDet :: m a -> m a -> m a

instance ConvIO IO where
  unsafeConvFromIO = id
  {-# INLINE unsafeConvFromIO #-}
  unsafeConvToIO = id
  {-# INLINE unsafeConvToIO #-}
  unsafeConvIfDet = \_ b -> b
  {-# INLINE unsafeConvIfDet #-}

instance ConvIO (ST s) where
  unsafeConvFromIO (IO a) = ST (unsafeCoerce# a)
  {-# INLINE unsafeConvFromIO #-}
  unsafeConvToIO (ST a) = IO (unsafeCoerce# a)
  {-# INLINE unsafeConvToIO #-}
  unsafeConvIfDet = \a _ -> a
  {-# INLINE unsafeConvIfDet #-}

-- | Run a monadic action if the monad is deterministic.
unsafeConvWhenDet :: ConvIO m => m a -> m ()
unsafeConvWhenDet m = unsafeConvIfDet (m >> pure ()) $ pure ()
{-# INLINE unsafeConvWhenDet #-}
