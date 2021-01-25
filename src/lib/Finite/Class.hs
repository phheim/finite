-----------------------------------------------------------------------------
-- |
-- Module      :  Finite.Class
-- Maintainer  :  Felix Klein
--
-- 'Finite' main class decleration including generics support.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    MultiWayIf
  , TypeOperators
  , DefaultSignatures
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances

  #-}

-----------------------------------------------------------------------------

module Finite.Class
  ( T
  , Finite(..)
  , GFinite(..)
  ) where

-----------------------------------------------------------------------------

import Control.Exception
  ( assert
  )

import Finite.Type
  ( T
  , FiniteBounds
  , (#<<)
  , (<<#)
  , v2t
  , (\#)
  , (#)
  )

import GHC.Generics
  ( Generic
  , Rep
  , (:*:)(..)
  , (:+:)(..)
  , U1(..)
  , M1(..)
  , K1(..)
  , from
  , to
  )

import qualified Data.IntSet as S
  ( toList
  , fromList
  , fromAscList
  , difference
  )

-----------------------------------------------------------------------------

-- | The 'Finite' class.

class Finite b a where

  -- | Returns the number of elements associated with the given type.
  elements
    :: FiniteBounds b
    => T a -> Int

  default elements
    :: (Generic a, GFinite b (Rep a), FiniteBounds b)
    => T a -> Int

  elements t = gelements #<< from <<# t

  -- | Turns the value in the associated range into an Int uniquely
  -- identifiying the value.
  index
    :: FiniteBounds b
    => a -> Int

  default index
    :: (Generic a, GFinite b (Rep a), FiniteBounds b)
    => a -> Int

  index v = (+ (offset $ v2t v)) $ gindex $ from v

  -- | Turns an Int back to the value that is associated with it.
  value
    :: FiniteBounds b => Int -> a

  default value
    :: (Generic a, GFinite b (Rep a), FiniteBounds b)
    => Int -> a

  value v =
    let
      o = offset $ v2t r
      e = elements $ v2t r
      r = to $ gvalue (v - o)
    in
      assert (v >= o && v < o + e) r

  -- | Allows to put an offset to the integer mapping. Per default the
  -- offset is zero.
  offset
    :: FiniteBounds b
    => T a -> Int

  offset _ = 0

  -- | Returns a finite list of all elements of that type.
  values
    :: FiniteBounds b
    => [a]

  values =
    let
      rs = map value xs
      o = offset $ f rs
      n = elements $ f rs
      xs = [o, o + 1 .. o + n - 1]
    in
      rs

    where
      f :: [a] -> T a
      f _ = (#)

  -- | Complements a given list of elements of that type
  complement
    :: FiniteBounds b
    => [a] -> [a]

  complement xs =
    let
      o = offset $ f rs
      n = elements $ f rs
      s = S.fromList $ map index xs
      a = S.fromAscList [o, o + 1 .. o + n - 1]
      rs = map value $ S.toList $ S.difference a s
    in
      rs

    where
      f :: [a] -> T a
      f _ = (#)

  -- | Less than operator according to the implicit total index order.
  (|<|)
    :: FiniteBounds b
    => a -> a -> Bool

  (|<|) x y =
    index x < index y

  infixr |<|

  -- | Less or equal than operator according to the implicit total
  -- index order.
  (|<=|)
    :: FiniteBounds b
    => a -> a -> Bool

  (|<=|) x y =
    index x <= index y

  infixr |<=|

  -- | Greater or equal than operator according to the implicit total
  -- index order.
  (|>=|)
    :: FiniteBounds b
    => a -> a -> Bool

  (|>=|) x y =
    index x >= index y

  infixr |>=|

  -- | Greater than operator according to the implicit total index
  -- order.
  (|>|)
    :: FiniteBounds b
    => a -> a -> Bool

  (|>|) x y =
    index x > index y

  infixr |>|

  -- | Equal operator according to the implicit total index order.
  (|==|)
    :: FiniteBounds b
    => a -> a -> Bool

  (|==|) x y =
    index x == index y

  infixr |==|

  -- | Unequal operator according to the implicit total index order.
  (|/=|)
    :: FiniteBounds b
    => a -> a -> Bool

  (|/=|) x y =
    index x /= index y

  infixr |/=|


  -- | First element according to the total index order.
  initial
    :: FiniteBounds b
    => T a -> a

  initial t =
    value $ offset t

  -- | Last element according to the total index order.
  final
    :: FiniteBounds b
    => T a -> a

  final t =
    value $ offset t + elements t - 1

  -- | Next element according to the total index order (undefined for
  -- the last element).
  next
    :: FiniteBounds b
    => a -> a

  next x =
    let i = index x
    in assert (i < offset (v2t x) + elements (v2t x) - 1)
       $ value (i + 1)

  -- | Previous element according to the total index order (undefined
  -- for the first element).
  previous
    :: FiniteBounds b
    => a -> a

  previous x =
    let i = index x
    in assert (i > offset (v2t x))
       $ value (i - 1)

  -- | The upper and lower bounds of the instance.
  bounds
    :: FiniteBounds b
    => T a -> (a, a)

  bounds t =
    (initial t, final t)

-----------------------------------------------------------------------------

-- | Generics implementation for the 'Finite' class. The
-- realization is closely related to the one presented at
-- https://wiki.haskell.org/GHC.Generics.

class GFinite b f where
  gelements :: FiniteBounds b => T (f a) -> Int
  gindex :: FiniteBounds b => f a -> Int
  gvalue :: FiniteBounds b => Int -> f a

-----------------------------------------------------------------------------

-- | :*: instance.

instance
  (GFinite b f, GFinite b g)
    => GFinite b (f :*: g) where

  gelements x =
    gelements (((\#) :: T ((f :*: g) a) -> T (f a)) x) *
    gelements (((\#) :: T ((f :*: g) a) -> T (g a)) x)

  gindex (f :*: g) =
    (gindex f * (gelements #<< g)) + gindex g

  gvalue n =
    let
      m = gelements #<< g
      f = gvalue (n `div` m)
      g = gvalue (n `mod` m)
    in
     (f :*: g)

-----------------------------------------------------------------------------

-- | :+: instance.

instance
  (GFinite b f, GFinite b g)
    => GFinite b (f :+: g) where

  gelements x =
    gelements (((\#) :: T ((f :+: g) a) -> T (f a)) x) +
    gelements (((\#) :: T ((f :+: g) a) -> T (g a)) x)

  gindex x = case x of
    R1 y -> gindex y
    L1 y -> gindex y + gelements (((\#) :: (f :+: g) a -> T (g a)) x)

  gvalue n =
    let
      m = gelements #<< g
      g = gvalue (n `mod` m)
      f = gvalue (n - m)
    in if
      | n < m     -> R1 g
      | otherwise -> L1 f

-----------------------------------------------------------------------------

-- | U1 instance.

instance
  GFinite c U1 where

  gelements _ = 1

  gindex U1 = 0

  gvalue _ = U1

-----------------------------------------------------------------------------

-- | M1 instance.

instance
  (GFinite c f)
    => GFinite c (M1 i v f) where

  gelements =
    gelements . ((\#) :: T ((M1 i v f) p) -> T (f p))

  gindex (M1 x) = gindex x

  gvalue = M1 . gvalue

-----------------------------------------------------------------------------

-- | K1 instance.

instance
  (Finite b a)
    => GFinite b (K1 i a) where

  gelements =
    elements . ((\#) :: T ((K1 i a) c) -> T a)

  gindex (K1 x) = index x - (offset #<< x)

  gvalue n =
    let
      m = offset #<< x
      x = value (n + m)
    in
      K1 x

-----------------------------------------------------------------------------
