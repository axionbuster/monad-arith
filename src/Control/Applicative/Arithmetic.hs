{-# OPTIONS_GHC -Wno-missing-methods #-}

-- |
-- Module: Control.Applicative.Arithmetic
-- Description: Lifted arithmetic classes
--
-- Use the provided 'Arithmetic' newtype to lift various numeric class
-- instances to 'Functor', 'Applicative', or 'Monad'. Or, use it to
-- instantiate these classes using the DerivingVia language extension.
--
-- Template Haskell-style lifting of the classes facilitates writing
-- imperative code.
module Control.Applicative.Arithmetic where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Zip

-- | Lifting of arithmetic classes
newtype Arithmetic m a = Arithmetic {runArithmetic :: m a}
  deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadFix
    , MonadIO
    , MonadPlus
    , MonadZip
    )
    via m

-- | Identity
instance MonadTrans Arithmetic where
  lift = Arithmetic
  {-# INLINE lift #-}

instance (Applicative m, Num a) => Num (Arithmetic m a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  signum = fmap signum
  abs = fmap abs
  fromInteger = pure . fromInteger
  negate = fmap negate
  (-) = liftA2 (-)
  {-# INLINE (+) #-}
  {-# INLINE (*) #-}
  {-# INLINE signum #-}
  {-# INLINE abs #-}
  {-# INLINE fromInteger #-}
  {-# INLINE negate #-}
  {-# INLINE (-) #-}

instance (Applicative m, Fractional a) => Fractional (Arithmetic m a) where
  fromRational = pure . fromRational
  recip = fmap recip
  (/) = liftA2 (/)
  {-# INLINE fromRational #-}
  {-# INLINE recip #-}
  {-# INLINE (/) #-}

instance (Applicative m, Floating a) => Floating (Arithmetic m a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
  {-# INLINE pi #-}
  {-# INLINE exp #-}
  {-# INLINE log #-}
  {-# INLINE sin #-}
  {-# INLINE cos #-}
  {-# INLINE asin #-}
  {-# INLINE acos #-}
  {-# INLINE atan #-}
  {-# INLINE sinh #-}
  {-# INLINE cosh #-}
  {-# INLINE asinh #-}
  {-# INLINE acosh #-}
  {-# INLINE atanh #-}

-- | Completely defective
instance Eq (Arithmetic m a)

-- | Lifted 'Eq'
class MEq m where
  -- | Lifted '=='; default implementation uses 'liftA2'.
  (^==) :: (Eq a) => m a -> m a -> m Bool
  default (^==) :: (Applicative m, Eq a) => m a -> m a -> m Bool
  (^==) = liftA2 (==)

  -- | Lifted '/='; default implementation refers to '^=='.
  (^/=) :: (Eq a) => m a -> m a -> m Bool
  default (^/=) :: (Applicative m, Eq a) => m a -> m a -> m Bool
  (^/=) = liftA2 (/=)

-- | Completely defective
instance Ord (Arithmetic m a)

-- | Lifted 'Ord'
class MOrd m where
  -- | Lifted 'compare'; default implementation uses 'liftA2'.
  compareM :: (Ord a) => m a -> m a -> m Ordering
  default compareM :: (Applicative m, Ord a) => m a -> m a -> m Ordering
  compareM = liftA2 compare

  -- | Lifted '<='; default implementation uses 'liftA2'.
  (^<=) :: (Ord a) => m a -> m a -> m Bool
  default (^<=) :: (Applicative m, Ord a) => m a -> m a -> m Bool
  (^<=) = liftA2 (<=)

  -- | Lifted '>='; default implementation uses 'liftA2'.
  (^>=) :: (Ord a) => m a -> m a -> m Bool
  default (^>=) :: (Applicative m, Ord a) => m a -> m a -> m Bool
  (^>=) = liftA2 (>=)

  -- | Lifted '<'; default implementation uses 'liftA2'.
  (^<) :: (Ord a) => m a -> m a -> m Bool
  default (^<) :: (Applicative m, Ord a) => m a -> m a -> m Bool
  (^<) = liftA2 (<)

  -- | Lifted '>'; default implementation uses 'liftA2'.
  (^>) :: (Ord a) => m a -> m a -> m Bool
  default (^>) :: (Applicative m, Ord a) => m a -> m a -> m Bool
  (^>) = liftA2 (>)

  -- | Lifted 'min'; default implementation uses 'liftA2'.
  minM :: (Ord a) => m a -> m a -> m a
  default minM :: (Applicative m, Ord a) => m a -> m a -> m a
  minM = liftA2 min

  -- | Lifted 'max'; default implementation uses 'liftA2'.
  maxM :: (Ord a) => m a -> m a -> m a
  default maxM :: (Applicative m, Ord a) => m a -> m a -> m a
  maxM = liftA2 max

-- | Completely defective
instance (Applicative m, Num a) => Real (Arithmetic m a)

-- | Lifted 'Real'
class MReal m where
  -- | Lifted 'toRational'; default implementation uses 'fmap'.
  toRationalM :: (Real a) => m a -> m Rational
  default toRationalM :: (Functor m, Real a) => m a -> m Rational
  toRationalM = fmap toRational

-- | 'fromEnum' is defective.
instance (Applicative m, Enum a) => Enum (Arithmetic m a) where
  toEnum = pure . toEnum

-- | Lifted 'Enum'
class MEnum m where
  -- | Lifted 'toEnum'
  toEnumM :: (Enum a) => m Int -> m a
  default toEnumM :: (Enum a, Functor m) => m Int -> m a
  toEnumM = fmap toEnum

  -- | Lifted 'fromEnum'
  fromEnumM :: (Enum a) => m a -> m Int
  default fromEnumM :: (Enum a, Functor m) => m a -> m Int
  fromEnumM = fmap fromEnum

-- | `fromInteger` is defective; others are functional.
instance (Applicative m, Integral a) => Integral (Arithmetic m a) where
  quot = liftA2 quot
  rem = liftA2 rem
  div = liftA2 div
  mod = liftA2 mod
  quotRem x y = (x `quot` y, x `rem` y)
  divMod x y = (x `div` y, x `mod` y)
  {-# INLINE quot #-}
  {-# INLINE rem #-}
  {-# INLINE div #-}
  {-# INLINE mod #-}
  {-# INLINE quotRem #-}
  {-# INLINE divMod #-}

-- | Lifted 'Integral'
class IntegralM m where
  quotM, remM, divM, modM :: (Integral a) => m a -> m a -> m a
  default quotM :: (Applicative m, Integral a) => m a -> m a -> m a
  quotM = liftA2 quot
  default remM :: (Applicative m, Integral a) => m a -> m a -> m a
  remM = liftA2 rem
  default divM :: (Applicative m, Integral a) => m a -> m a -> m a
  divM = liftA2 div
  default modM :: (Applicative m, Integral a) => m a -> m a -> m a
  modM = liftA2 mod

  quotRemM, divModM :: (Integral a) => m a -> m a -> m (a, a)
  default quotRemM :: (Applicative m, Integral a) => m a -> m a -> m (a, a)
  quotRemM = liftA2 quotRem
  default divModM :: (Applicative m, Integral a) => m a -> m a -> m (a, a)
  divModM = liftA2 divMod

-- | Completely defective
instance (Applicative m, RealFrac a) => RealFrac (Arithmetic m a)

-- | Lifted 'RealFrac'
class RealFracM m where
  -- | Lifted 'properFraction'
  properFractionM :: (Integral b, RealFrac a) => m a -> m (b, a)
  default properFractionM ::
    (Functor m, Integral b, RealFrac a) => m a -> m (b, a)
  properFractionM = fmap properFraction

  truncateM, roundM, ceilingM, floorM :: (Integral b, RealFrac a) => m a -> m b
  default truncateM :: (Functor m, Integral b, RealFrac a) => m a -> m b
  truncateM = fmap truncate
  default roundM :: (Functor m, Integral b, RealFrac a) => m a -> m b
  roundM = fmap round
  default ceilingM :: (Functor m, Integral b, RealFrac a) => m a -> m b
  ceilingM = fmap ceiling
  default floorM :: (Functor m, Integral b, RealFrac a) => m a -> m b
  floorM = fmap floor

-- | Only `encodeFloat`, `significand`, `atan2` are defined
instance (Applicative m, RealFloat a) => RealFloat (Arithmetic m a) where
  encodeFloat = (pure .) . encodeFloat
  significand = fmap significand
  atan2 = liftA2 atan2
  {-# INLINE encodeFloat #-}
  {-# INLINE significand #-}
  {-# INLINE atan2 #-}

-- | Lifted 'RealFloat'
class RealFloatM m where
  -- | Lifted 'floatRadix'
  floatRadixM :: (RealFloat a) => m a -> m Integer
  default floatRadixM :: (Functor m, RealFloat a) => m a -> m Integer
  floatRadixM = fmap floatRadix

  -- | Lifted 'floatDigits'
  floatDigitsM :: (RealFloat a) => m a -> m Int
  default floatDigitsM :: (Functor m, RealFloat a) => m a -> m Int
  floatDigitsM = fmap floatDigits

  -- | Lifted 'floatRange'
  floatRangeM :: (RealFloat a) => m a -> m (Int, Int)
  default floatRangeM :: (Functor m, RealFloat a) => m a -> m (Int, Int)
  floatRangeM = fmap floatRange

  -- | Lifted 'decodeFloat'
  decodeFloatM :: (RealFloat a) => m a -> m (Integer, Int)
  default decodeFloatM :: (Functor m, RealFloat a) => m a -> m (Integer, Int)
  decodeFloatM = fmap decodeFloat

  -- | Lifted 'exponent'
  exponentM :: (RealFloat a) => m a -> m Int
  default exponentM :: (Functor m, RealFloat a) => m a -> m Int
  exponentM = fmap exponent

  -- | Lifted 'significand'
  significandM :: (RealFloat a) => m a -> m a
  default significandM :: (Functor m, RealFloat a) => m a -> m a
  significandM = fmap significand

  -- | Lifted 'scaleFloat'
  scaleFloatM :: (RealFloat a) => m Int -> m a -> m a
  default scaleFloatM :: (Applicative m, RealFloat a) => m Int -> m a -> m a
  scaleFloatM = liftA2 scaleFloat

  -- | Lifted 'isNaN'
  isNaNM :: (RealFloat a) => m a -> m Bool
  default isNaNM :: (Functor m, RealFloat a) => m a -> m Bool
  isNaNM = fmap isNaN

  -- | Lifted 'isInfinite'
  isInfiniteM :: (RealFloat a) => m a -> m Bool
  default isInfiniteM :: (Functor m, RealFloat a) => m a -> m Bool
  isInfiniteM = fmap isInfinite

  -- | Lifted 'isDenormalized'
  isDenormalizedM :: (RealFloat a) => m a -> m Bool
  default isDenormalizedM :: (Functor m, RealFloat a) => m a -> m Bool
  isDenormalizedM = fmap isDenormalized

  -- | Lifted 'isNegativeZero'
  isNegativeZeroM :: (RealFloat a) => m a -> m Bool
  default isNegativeZeroM :: (Functor m, RealFloat a) => m a -> m Bool
  isNegativeZeroM = fmap isNegativeZero

  -- | Lifted 'isIEEE'
  isIEEEM :: (RealFloat a) => m a -> m Bool
  default isIEEEM :: (Functor m, RealFloat a) => m a -> m Bool
  isIEEEM = fmap isIEEE

  -- | Lifted 'atan2'
  atan2M :: (RealFloat a) => m a -> m a -> m a
  default atan2M :: (Applicative m, RealFloat a) => m a -> m a -> m a
  atan2M = liftA2 atan2

instance (Applicative m) => MEq (Arithmetic m)

instance (Applicative m) => MOrd (Arithmetic m)

instance (Functor m) => MReal (Arithmetic m)

instance (Applicative m) => MEnum (Arithmetic m)

instance (Applicative m) => IntegralM (Arithmetic m)

instance (Functor m) => RealFracM (Arithmetic m)

instance (Applicative m) => RealFloatM (Arithmetic m)
