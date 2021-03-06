{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE DuplicateRecordFields #-}

--TODO name Total Preorder since we allow for ties
module TotalPreorder (
  Comparison
  , LtEqBy
  , GtEqBy, EqBy, LtBy, GtBy
  , IsCmpOf
  , CmpByCase(..)
  , classify
  , cmpIsCmpBy
  , ltEqByRefl, gtEqByRefl
  , eqByRefl, eqBySymmetric, eqByTransitive
  , ltByNotGtEq, gtByNotLtEq, eqByNotLtGt
  , ltByTransitiveL
) where

import qualified Data.Ord as ORD
import Data.Function (on)
import Data.Coerce

import Control.Arrow ((&&&))

import qualified GDP
import ClassicalExtras

type Comparison e = e -> e -> Ordering

newtype LtEqBy cmpName x1 x2 = LtEqBy GDP.Defn
type role LtEqBy nominal nominal nominal

type GtEqBy cmpName x1 x2 = LtEqBy cmpName x2 x1

type EqBy cmpName x1 x2 = (LtEqBy cmpName x1 x2)
                            GDP.&& (GtEqBy cmpName x1 x2)

type LtBy cmpName x1 x2 = (LtEqBy cmpName x1 x2)
                          GDP.&& (GDP.Not (GtEqBy cmpName x1 x2))

type GtBy cmpName x1 x2 = (GtEqBy cmpName x1 x2)
                          GDP.&& (GDP.Not (LtEqBy cmpName x1 x2))

newtype IsCmpOf a cmpName = IsCmpBy GDP.Defn
type role IsCmpOf nominal nominal

data CmpByCase cmpName x1 x2 = 
  EqByCase (GDP.Proof (EqBy cmpName x1 x2)) 
  | LtByCase (GDP.Proof (LtBy cmpName x1 x2))
  | GtByCase (GDP.Proof (GtBy cmpName x1 x2))

classify :: (Comparison a GDP.~~ cmpName) 
  -> (a GDP.~~ x1Name) 
  -> (a GDP.~~ x2Name) 
  -> CmpByCase cmpName x1Name x2Name
classify (GDP.The cmp) (GDP.The x1) (GDP.The x2) = case cmp x1 x2 of
  EQ -> EqByCase GDP.axiom
  LT -> LtByCase GDP.axiom
  GT -> GtByCase GDP.axiom

cmpIsCmpBy :: (Comparison a GDP.~~ cmpName) -> GDP.Proof (IsCmpOf a cmpName)
cmpIsCmpBy _ = GDP.axiom

ltEqByRefl :: GDP.Fact (IsCmpOf a cmpName)
  => (a GDP.~~ xName) 
  -> GDP.Proof (LtEqBy cmpName xName xName)
ltEqByRefl _ = GDP.axiom

instance GDP.Transitive (LtEqBy cmpName) where
  transitive _ _ = GDP.axiom

--nb derived
gtEqByRefl :: GDP.Fact (IsCmpOf a cmpName)
  => (a GDP.~~ xName) 
  -> GDP.Proof (GtEqBy cmpName xName xName)
gtEqByRefl = ltEqByRefl

eqByRefl :: GDP.Fact (IsCmpOf a cmpName)
  => (a GDP.~~ xName)  
  -> GDP.Proof (EqBy cmpName xName xName)
eqByRefl x = GDP.introAnd (ltEqByRefl x) (gtEqByRefl x)

eqBySymmetric :: GDP.Proof (EqBy cmpName x1 x2)
  -> GDP.Proof (EqBy cmpName x2 x1)
eqBySymmetric = GDP.symmetric

eqByTransitive :: GDP.Proof (EqBy cmpName x1 x2)
  -> GDP.Proof (EqBy cmpName x2 x3)
  -> GDP.Proof (EqBy cmpName x1 x3)
eqByTransitive p1 p2 = 
  GDP.introAnd (GDP.transitive pl1 pl2) (GDP.transitive pr2 pr1)
  where
    (pl1, pr1) = (GDP.elimAndL &&& GDP.elimAndR) p1
    (pl2, pr2) = (GDP.elimAndL &&& GDP.elimAndR) p2

-- mutual exclusive proofs

ltByNotGtEq :: GDP.Proof (LtBy cmpName x1 x2)
  -> GDP.Proof (GDP.Not (GtEqBy cmpName x1 x2))
ltByNotGtEq = GDP.elimAndR

gtByNotLtEq :: GDP.Proof (GtBy cmpName x1 x2)
  -> GDP.Proof (GDP.Not (LtEqBy cmpName x1 x2))
gtByNotLtEq = ltByNotGtEq

eqByNotLtGt :: forall cmpName x1 x2. GDP.Classical
  => GDP.Proof (EqBy cmpName x1 x2)
  -> GDP.Proof ((GDP.Not (LtBy cmpName x1 x2)) GDP.&& (GDP.Not (GtBy cmpName x1 x2)))
eqByNotLtGt eqpf =  GDP.introAnd notLtByPf notGtByPf
  where 
    notLtByPf :: GDP.Proof (GDP.Not (LtBy cmpName x1 x2))
    notLtByPf = deMorgansFactorNotOr . GDP.introOrR . introNotNot . GDP.elimAndR  $ eqpf
    notGtByPf :: GDP.Proof (GDP.Not (GtBy cmpName x1 x2))
    notGtByPf = deMorgansFactorNotOr . GDP.introOrR . introNotNot . GDP.elimAndL $ eqpf


ltByTransitiveL :: forall cmpName x1 x2 x3. GDP.Classical
  => GDP.Proof ((LtBy cmpName x1 x2) GDP.&& (LtEqBy cmpName x2 x3))
  -> GDP.Proof (LtBy cmpName x1 x3)
ltByTransitiveL p = GDP.introAnd ltEqByPf13 $ GDP.elimOr case2Eq3 case2Lt3 GDP.lem 
  where
    ltEqByPf13 :: GDP.Proof (LtEqBy cmpName x1 x3)
    ltEqByPf13 = GDP.transitive (GDP.elimAndL pl) pr
    case2Eq3Helper :: GDP.Proof (GtEqBy cmpName x2 x3) -> GDP.Proof (GtEqBy cmpName x1 x3) -> GDP.Proof (EqBy cmpName x1 x2)
    case2Eq3Helper gtEqPf23 gtEqPf13 = eqByTransitive (GDP.introAnd ltEqByPf13 gtEqPf13) . eqBySymmetric $ GDP.introAnd pr gtEqPf23
    case2Eq3 :: GDP.Proof (GtEqBy cmpName x2 x3) -> GDP.Proof (GDP.Not (GtEqBy cmpName x1 x3))--GDP.Proof (LtBy a cmpName x1 x3)
    case2Eq3 gtEqPf23 = GDP.introNot (\gtEqPf13 -> GDP.contradicts (GDP.elimAndR $ case2Eq3Helper gtEqPf23 gtEqPf13) (GDP.elimAndR pl))
    case2Lt3 :: GDP.Proof (GDP.Not (GtEqBy cmpName x2 x3)) -> GDP.Proof (GDP.Not (GtEqBy cmpName x1 x3))
    case2Lt3 notGtEqPf23 = GDP.introNot (\gtEqPf13 -> flip GDP.contradicts notGtEqPf23 $ GDP.transitive gtEqPf13 (GDP.elimAndL pl))
    (pl, pr) = (GDP.elimAndL &&& GDP.elimAndR) p


