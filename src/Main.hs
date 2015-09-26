{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import GHC.TypeLits (Symbol)

data SomeData = SomeData
    { fieldOne     :: String
    , fieldTwo :: String
    , fieldThree       :: String }
    deriving (Show, Eq)
$(makeLensesWith abbreviatedFields ''SomeData)

data Partially :: [Symbol] -> * -> * where
    Partially :: a -> Partially missing a

initial :: Partially '["one", "two", "three"] SomeData
initial =
    Partially (SomeData undefined undefined undefined)

setOne :: Partially '["one", "two", "three"] SomeData
       -> Partially '["two", "three"] SomeData
setOne (Partially d) = Partially (d { fieldOne = "first value" })

setTwo :: Partially '["two", "three"] SomeData
       -> Partially '["three"] SomeData
setTwo (Partially d) = Partially (d { fieldTwo = "second value" })

setThree :: Partially '["three"] SomeData
         -> Partially '[] SomeData
setThree (Partially d) = Partially (d { fieldThree = "third value" })

finalizePartial :: Partially '[] a -> a
finalizePartial (Partially a) = a

buildSomeBig :: SomeData
buildSomeBig = finalizePartial (setThree (setTwo (setOne initial)))

main :: IO ()
main = do
  print (finalizePartial (setThree (setTwo (setOne initial))))
  -- print (finalizePartial (setTwo (setOne initial)))
