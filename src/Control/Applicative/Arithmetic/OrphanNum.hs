{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Control.Applicative.Arithmetic.OrphanNum
-- Description: Orphan instance of 'Num' for 'Applicative' functors
--
-- This module defines an orphan instance of 'Num' for 'Applicative' functors.
-- This module is incompatible with the "Control.Applicative.Arithmetic"
-- module. It exports nothing besides that instance, so import it like so:
--
-- @@
-- import Control.Applicative.Arithmetic.OrphanNum ()
-- @@
module Control.Applicative.Arithmetic.OrphanNum () where

instance (Applicative m, Num a) => Num (m a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}
