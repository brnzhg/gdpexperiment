{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}

--TODO split out IsFieldOf definition into internal module
module RecordField (
  IsFieldOf(..)
  , HasNamedField(..)
  , PropHasField(..)
) where


import GDP
import GHC.TypeLits
import Data.Proxy

newtype IsFieldOf (s :: Symbol) recordName = IsFieldOf GDP.Defn


class HasNamedField (s :: Symbol) r a | s r -> a where
  getNamedField :: Proxy s -> (r ~~ recordName) -> (a ~~ IsFieldOf s recordName)

-- if uncommented this should only be used by definition modules
--getterToNamedGetter :: Defining (f recordName) => (r -> a) -> (r ~~ recordName) -> (a ~~ f recordName)
--getterToNamedGetter g = defn . g . the

class PropHasField (s :: Symbol) prop fieldName | s prop -> fieldName where
  matchPropField :: Proxy s 
    -> GDP.Proof (prop recordName) 
    -> GDP.Proof (IsFieldOf s recordName == fieldName)


--example
newtype A = A { getA :: Int }
newtype B = B { getB :: Double }
data C = C { getCA :: A, getCB :: B }

newtype IsCOf aName bName cName = IsCOf Defn


makeC :: (A ~~ aName) -> (B ~~ bName) -> (C ? IsCOf aName bName)
makeC (The x) (The y) = assert $ C { getCA = x, getCB = y }

instance HasNamedField "CA" C A where
  getNamedField _ = defn . getCA . GDP.the

instance HasNamedField "CB" C B where
  getNamedField _ = defn . getCB . GDP.the


instance PropHasField "CA" (IsCOf aName bName) aName where
  matchPropField _ _ = GDP.axiom

instance PropHasField "CB" (IsCOf aName bName) bName where
  matchPropField _ _ = GDP.axiom



