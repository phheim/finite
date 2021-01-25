-----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Felix Klein
--
-- Simple TestSuite.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , ImplicitParams
  , RecordWildCards
  , DeriveGeneric
  , TemplateHaskell
  , MultiParamTypeClasses

  #-}

-----------------------------------------------------------------------------

module Test
  ( tests
  ) where

-----------------------------------------------------------------------------

import Distribution.TestSuite
  ( TestInstance(..)
  , Progress(..)
  , Result(..)
  , Test(..)
  )

import Data.Hashable
  ( hash
  )

import Data.Ix
  ( range
  )

import Control.Exception
  ( assert
  )

import GHC.Generics
  ( Generic
  )

import Test.QuickCheck
  ( Result
      ( Success
      , Failure
      , reason
      )
  , quickCheckResult
  )

import Finite.TH

import Finite

-----------------------------------------------------------------------------

newInstance "AInst"

data Bounds = Bounds { size :: Int }

baseInstance [t|Bounds|] [|size|] "AInst"

newBaseInstance [t|Bounds|] [|size|] "BInst"

data BBounds = BBounds { bnds :: Bounds }

extendInstance [t|AInst|] [t|BBounds|] [|bnds|]

data GInst =
    AData
  | BData AInst
  | CData AInst BInst
  deriving (Eq, Ord, Generic)

instance Finite Bounds GInst

newtype OInst = OInst { oInst :: Int } deriving (Eq, Ord)

instance Finite Bounds OInst where
  elements _ = size ?bounds
  offset _ = 3
  value = OInst
  index = oInst

data TInst =
    DData
  | EData OInst
  | FData AInst BInst
  deriving (Eq, Ord, Generic)

instance Finite Bounds TInst

-----------------------------------------------------------------------------

tests
  :: IO [Test]

tests = return
  [ Test t01
  , Test t02
  , Test t03
  , Test t04
  , Test t05
  , Test t06
  ]

  where
    t01 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> x == aInst (AInst x)
              , \x -> AInst x == AInst x
              , \x -> AInst x < AInst (x + 1)
              , \x -> show (AInst x) == show (AInst x)
              , \x -> show (AInst x) == show x
              , \x -> hash (AInst x) == hash (AInst x)
              , \x -> not $ null $ range (AInst 0, AInst $ abs x)
              , \x -> (AInst x) + (AInst 1) == (AInst 1) + (AInst x)
              ] ++
              [ quickCheckResult $ \x -> aInst x == aInst x
              ])
      , name = "TH: newInstance"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t01
      }

    t02 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> let ?bounds = Bounds $ abs x + 1 in
                      elements ((#) :: T AInst) == abs x + 1
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      offset ((#) :: T AInst) == 0
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map index (values :: [AInst]) == [0,1..abs x]
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map value [0,1..abs x] == (values :: [AInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      all (\x -> x == value (index x)) (values :: [AInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      complement (filter (odd . index) (values :: [AInst]))
                      == filter (even . index) (values :: [AInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      initial ((#) :: T AInst) |<=| final ((#) :: T AInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else next (initial ((#) :: T AInst))
                             |>=| initial ((#) :: T AInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else previous (final ((#) :: T AInst))
                             |/=| final ((#) :: T AInst)
              ])
      , name = "TH: baseInstance"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t02
      }

    t03 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> x == bInst (BInst x)
              , \x -> BInst x == BInst x
              , \x -> BInst x < BInst (x + 1)
              , \x -> show (BInst x) == show (BInst x)
              , \x -> show (BInst x) == show x
              , \x -> hash (BInst x) == hash (BInst x)
              , \x -> not $ null $ range (BInst 0, BInst $ abs x)
              , \x -> (BInst x) + (BInst 1) == (BInst 1) + (BInst x)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      elements ((#) :: T BInst) == abs x + 1
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      offset ((#) :: T BInst) == 0
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map index (values :: [BInst]) == [0,1..abs x]
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map value [0,1..abs x] == (values :: [BInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      all (\x -> x == value (index x)) (values :: [BInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      complement (filter (odd . index) (values :: [BInst]))
                      == filter (even . index) (values :: [BInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      initial ((#) :: T BInst) |<=| final ((#) :: T BInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else next (initial ((#) :: T BInst))
                             |>=| initial ((#) :: T BInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else previous (final ((#) :: T BInst))
                             |/=| final ((#) :: T BInst)
              ] ++
              [ quickCheckResult $ \x -> bInst x == bInst x
              ])
      , name = "TH: newBaseInstance"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t03
      }

    t04 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      elements ((#) :: T AInst) == abs x + 1
              , \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      offset ((#) :: T AInst) == 0
              , \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      map index (values :: [AInst]) == [0,1..abs x]
              , \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      map value [0,1..abs x] == (values :: [AInst])
              , \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      all (\x -> x == value (index x)) (values :: [AInst])
              , \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      complement (filter (odd . index) (values :: [AInst]))
                      == filter (even . index) (values :: [AInst])
              , \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      initial ((#) :: T AInst) |<=| final ((#) :: T AInst)
              , \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else next (initial ((#) :: T AInst))
                             |>=| initial ((#) :: T AInst)
              , \x -> let ?bounds = BBounds $ Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else previous (final ((#) :: T AInst))
                             |/=| final ((#) :: T AInst)
              ])
      , name = "TH: extendInstance"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t04
      }

    t05 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> let ?bounds = Bounds $ abs x + 1 in
                      elements ((#) :: T GInst)
                        == 1 + (abs x + 1) + (abs x + 1) * (abs x + 1)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      offset ((#) :: T GInst) == 0
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map index (values :: [GInst])
                        == [0,1..elements ((#) :: T GInst) - 1]
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map value [0,1..elements ((#) :: T GInst) - 1]
                        == (values :: [GInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      all (\x -> x == value (index x)) (values :: [GInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      complement (filter (odd . index) (values :: [GInst]))
                      == filter (even . index) (values :: [GInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      initial ((#) :: T GInst) |<=| final ((#) :: T GInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else next (initial ((#) :: T GInst))
                             |>=| initial ((#) :: T GInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else previous (final ((#) :: T GInst))
                             |/=| final ((#) :: T GInst)
              ])
      , name = "Generics"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t05
      }

    t06 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> let ?bounds = Bounds $ abs x + 1 in
                      elements ((#) :: T OInst) == abs x + 1
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      offset ((#) :: T OInst) == 3
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map index (values :: [OInst]) == [3,4..abs x + 3]
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map value [3,4..abs x + 3] == (values :: [OInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      all (\x -> x == value (index x)) (values :: [OInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      complement (filter (odd . index) (values :: [OInst]))
                      == filter (even . index) (values :: [OInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      initial ((#) :: T OInst) |<=| final ((#) :: T OInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else next (initial ((#) :: T OInst))
                             |>=| initial ((#) :: T OInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else previous (final ((#) :: T OInst))
                             |/=| final ((#) :: T OInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      elements ((#) :: T TInst)
                        == 1 + (abs x + 1) + (abs x + 1) * (abs x + 1)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      offset ((#) :: T TInst) == 0
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map index (values :: [TInst])
                        == [0,1..elements ((#) :: T TInst) - 1]
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      map value [0,1..elements ((#) :: T TInst) - 1]
                        == (values :: [TInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      all (\x -> x == value (index x)) (values :: [TInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      complement (filter (odd . index) (values :: [TInst]))
                      == filter (even . index) (values :: [TInst])
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      initial ((#) :: T TInst) |<=| final ((#) :: T TInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else next (initial ((#) :: T TInst))
                             |>=| initial ((#) :: T TInst)
              , \x -> let ?bounds = Bounds $ abs x + 1 in
                      if abs x < 1 then True
                      else previous (final ((#) :: T TInst))
                             |/=| final ((#) :: T TInst)
              ])
      , name = "Offset"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t06
      }

    allPass xs = case dropWhile (isSuccess) xs of
      []  -> Pass
      x:_ -> case x of
        Failure{..} -> Fail reason
        _           -> assert False undefined

    isSuccess = \case
      Success {} -> True
      _          -> False

-----------------------------------------------------------------------------
