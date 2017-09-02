module Math
    ( module Math.Polynomial
    , (//)
    , fac
    ) where


import Data.Decimal

import Math.Polynomial


fac :: Decimal -> Decimal
fac n = if n < 2 then 1 else n * fac (n-1)


(//) :: Decimal -> Decimal -> Decimal
n // d = Decimal 0 $ floor $ n / d
infixl 7 //
