-----------------------------------------------------------------------------
-- |
-- Module      :  Finite.Collection
-- Maintainer  :  Felix Klein
--
-- Allows to extend a finite instance from a single bound to a
-- collection of bounds, given as a finite ranged array.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    MultiParamTypeClasses
  , LambdaCase
  , ImplicitParams

  #-}

-----------------------------------------------------------------------------

module Finite.Collection where

-----------------------------------------------------------------------------

import Finite.Type
  ( T
  , v2t
  , (#<<)
  , FiniteBounds
  )

import Finite.Class
  ( Finite
  , elements
  , offset
  , value
  , index
  )

import Data.Array.IArray
  ( Array
  , Ix
  , (!)
  , inRange
  , assocs
  , range
  , bounds
  )

import Control.Exception
  ( assert
  )

-----------------------------------------------------------------------------

-- | The 'Collection' type provides a set of items, each assigning an
-- index of type @i@ to a value of type @a@.

data Collection i a =
  Item i a
  deriving
    ( -- | Equality can be checked for collections, if the index type
      -- and the elements can be checked for equality.
      Eq
    , -- | Order can be checked for collections, if the index type and
      -- the elements can be oredered.
      Ord
    , -- | Show a collection through its default constructor.
      Show
    )

-----------------------------------------------------------------------------

-- | Collections are used to extend Finite-Type / Context-Bounds pairs
-- to an array of bounds. At the same time the finite type is extended
-- to a collection of items that range over the same set of indices as
-- the bounds. Since the 'FiniteBounds' parameter always gives a
-- finite sized array of bounding parameters, it is guaranteed that
-- the connected collection has a finite bound as well.

instance (Ix i, Finite b a) => Finite (Array i b) (Collection i a) where

  elements t =
    sum $ map (elms t) $ assocs ?bounds

    where
      conv
        :: T (Collection i a) -> T a

      conv = undefined


      elms
        :: Finite b a => T (Collection i a) -> (i, b) -> Int

      elms t (_,b) =
        let ?bounds = b
        in elements $ conv t

  index (Item j v) =
    let
      -- array bounds
      (l,u) = bounds ?bounds
      -- list of indicies that appear before j
      ys = assert (inRange (l,u) j) $ init $ range (l,j)
      -- offset induces by these indices
      o = sum $ map ((elms v .) (?bounds !)) ys
      -- index of v with the bounds at position j
      idx = let ?bounds = ?bounds ! j
            in index v - offset #<< v
    in
      o + idx

    where
      elms
        :: Finite b a => a -> b -> Int

      elms v b =
        let ?bounds = b
        in elements $ v2t v

  value n =
    let
      -- elements of the whole collection
      e = elements $ v2t r
      -- array bounds
      b = bounds ?bounds
      -- target array index and reminder used as sub-index
      (j,m) = position (conv r) n (range b)
      -- result
      r = let ?bounds = ?bounds ! j
          in Item j $ value (m + offset (conv r))
    in
      assert (n >= 0 && n < e) r

    where
      conv
        :: Collection i a -> T a

      conv = undefined


      position
        :: (Ix i, Finite b a, FiniteBounds (Array i b))
        => T a -> Int -> [i] -> (i,Int)

      position t n = \case
        []   -> assert False undefined
        x:xr ->
          let m = let ?bounds = ?bounds ! x in elements t
          in if m <= n then position t (n - m) xr else (x,n)

-----------------------------------------------------------------------------
