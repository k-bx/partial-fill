{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens
import GHC.TypeLits (Symbol)
import Data.Type.Set
import GHC.TypeLits
import Data.Proxy

data Thing = Thing
    { fieldOne     :: String
    , fieldTwo     :: String
    , fieldThree   :: String }
    deriving (Show, Eq)

data Partially :: * -> * -> * where
    Partially :: a -> Partially missing a

-- Hack needed because I don't know how can 'Set' use non-'*' kind
data FieldName (s::Symbol)

type instance Cmp (FieldName v) (FieldName u) = CmpSymbol v u

-- ** Initial data with everything undefined yet. Will be TH-generated.

initial :: Partially (Set '[FieldName "fieldOne",
                            FieldName "fieldTwo",
                            FieldName "fieldThree"]) Thing
initial =
    Partially (Thing undefined undefined undefined)

-- ** Setter class

type FieldDel xs v fname = Partially (Delete (FieldName fname) xs) v

class PartialSetter v fname val where
    partiallySet :: Proxy fname -> val -> Partially xs v -> FieldDel xs v fname

-- ** Setter instances. Will be TH-generated

instance PartialSetter Thing "fieldOne" String where
    partiallySet _ val (Partially d) = Partially (d { fieldOne = val })

instance PartialSetter Thing "fieldTwo" String where
    partiallySet _ val (Partially d) = Partially (d { fieldTwo = val })

instance PartialSetter Thing "fieldThree" String where
    partiallySet _ val (Partially d) = Partially (d { fieldThree = val })

-- ** Finalizer

finalizePartial :: Partially (Set '[]) a -> a
finalizePartial (Partially a) = a

-- ** Let's see some action!

type F = Proxy
fld :: Proxy a
fld = Proxy

main :: IO ()
main = do
  print (finalizePartial
           (partiallySet (fld::F "fieldThree") "value three"
              (partiallySet (fld::F "fieldTwo") "value two"
                 (partiallySet (fld::F "fieldOne") "value one" initial))))

  -- might be slightly more readable
  print (initial & partiallySet (fld::F "fieldThree") "value three"
                 & partiallySet (fld::F "fieldTwo") "value two"
                 & partiallySet (fld::F "fieldOne") "value one"
                 & finalizePartial)

  -- Boom!
  -- print (initial & partiallySet (fld::F "fieldThree") "value three"
  --                & finalizePartial)
