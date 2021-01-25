-----------------------------------------------------------------------------
-- |
-- Module      :  Finite.Type
-- Maintainer  :  Felix Klein
--
-- Type association to pass types via functions.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ImplicitParams
  , ConstraintKinds

  #-}

-----------------------------------------------------------------------------

module Finite.Type
  ( T
  , FiniteBounds
  , (#)
  , (\#)
  , (<<#)
  , (#<<)
  , t2v
  , v2t
  ) where

-----------------------------------------------------------------------------

-- | A better looking constraint specifier.

type FiniteBounds b = (?bounds :: b)

-----------------------------------------------------------------------------

-- | A type dummy.

newtype T a = T ()

-----------------------------------------------------------------------------

-- | The type dummy instance.

(#) :: T a
(#) = T ()

-----------------------------------------------------------------------------

-- | A type dummy returning function. Intended to use the type engine
-- for accessing the type of the argument. Note that "@(\\#) :: a -> T
-- a@" is just a special instance.

(\#) :: b -> T a
(\#) _ = (#)

-----------------------------------------------------------------------------

-- | Get some undefined value of the given type. Intended to be used
-- for extracting type information of polymorph types only.

t2v :: T a -> a
t2v _ = undefined

-----------------------------------------------------------------------------

-- | Replace a function's argument by its type dummy. Intended to be used
-- for extracting type information of polymorph types only.

infixr <<#

(<<#) :: (a -> b) -> T a -> b
(<<#) f _ = f undefined

-----------------------------------------------------------------------------

-- | Get the type of a given value.

v2t :: a -> T a
v2t = (\#)

-----------------------------------------------------------------------------

-- | Replace a function's dummy type argument with its value taking
-- equivalent.

infixr #<<

(#<<) :: (T a -> b) -> a -> b
(#<<) f _ = f $ T ()

-----------------------------------------------------------------------------
