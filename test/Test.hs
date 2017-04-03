{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

-- stack build :categories-test

module Main where

import Prelude hiding (id,(.))

import ConCat.AltCat (ccc,id,(.))
-- import ConCat.Syntactic (Syn,render)

import ConCat.Categories.Gather
import ConCat.Categories.Poly

main :: IO ()
main =
  do putStrLn ""
     print (gather (ccc (\ (x,y) -> x - 3 + 7 * y :: Int)) (10,20))
     print (unPolyC (ccc (\ (x :: Int) -> x)))
     print (unPolyC (PolyC [1,2] . PolyC [3,4]) :: [Int])

-- (\ x -> 1 + 2 * x) . (\ x -> 3 + 4 * x)
-- \ x -> 1 + 2 * (3 + 4 * x)
-- \ x -> 7 + 8 * x)
