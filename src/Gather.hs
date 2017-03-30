{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}

module Gather where

import ConCat.Category

import Control.Arrow (Kleisli(..))
import Control.Monad.Writer
import Data.Set
import Prelude hiding (id, (.), curry, uncurry, const)

newtype Gather a b = Gather { runGather :: Kleisli (Writer (Set Int)) a b }

instance Category Gather where
    id = Gather id
    Gather f . Gather g = Gather (f . g)

instance ProductCat Gather where
    exl = Gather exl
    exr = Gather exr
    Gather f &&& Gather g = Gather (f &&& g)

instance Num a => NumCat Gather a where
  negateC = Gather negateC
  addC    = Gather addC
  subC    = Gather subC
  mulC    = Gather mulC
  powIC   = Gather powIC

instance ConstCat Gather Int where
  const b = Gather $ Kleisli $ const $ b <$ tell (singleton b)

equation :: Num a => a -> a -> a
equation x y = x + y - y

gather :: Gather a b -> a -> (b, Set Int)
gather f a = runWriter $ flip runKleisli a $ runGather f
