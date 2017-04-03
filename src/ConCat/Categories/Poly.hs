{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP

-- | Polynomial category

module ConCat.Categories.Poly where

import Prelude hiding (id,(.))

import ConCat.Category (Category(..))

{--------------------------------------------------------------------
    List of coefficients (ascending degree)
--------------------------------------------------------------------}

evalL :: Num s => [s] -> s -> s
evalL ss s = foldr (\ w z -> w + s * z) 0 ss

zeroP :: Num s => [s]
zeroP = []

constP :: Num s => s -> [s]
constP s = [s]

idP :: Num s => [s]
idP = [0,1]

infixl 6 @+
infixl 7 @*
(@+), (@*) :: Num s => [s] -> [s] -> [s]

scaleP :: Num s => s -> [s] -> [s]
scaleP s = map (s *)

negateP :: Num s => [s] -> [s]
negateP = map negate

[]     @+ ds     = ds
cs     @+ []     = cs
(c:cs) @+ (d:ds) = (c + d) : (cs @+ ds)

[]     @* _ = []
(c:cs) @* ds = scaleP c ds @+ (0 : cs @* ds)

infixl 9 @.
(@.) :: Num s => [s] -> [s] -> [s]
[] @. _ = zeroP
(d:ds) @. cs = constP d @+ (ds @. cs) @* cs

{--------------------------------------------------------------------
    Polynomial category
--------------------------------------------------------------------}

data PolyC a b = a ~ b => PolyC [a]

deriving instance Show a => Show (PolyC a b)

unPolyC :: PolyC a b -> [a]
unPolyC (PolyC as) = as

evalC :: Num a => PolyC a b -> (a -> b)
evalC (PolyC as) = evalL as

instance Category PolyC where
  type Ok PolyC = Num
  id = PolyC idP
  PolyC bs . PolyC as = PolyC (bs @. as)

_t1 :: PolyC Int Int
_t1 = PolyC [1,2] . PolyC [3,4] 
