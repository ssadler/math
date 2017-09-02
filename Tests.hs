{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Math


testDivide :: Test
testDivide = TestLabel "Test Divide" $ TestList [
    check "1" ("-1x3 -2x", "y3 -1y") (divide "-x4 +y3 -2x2 -y" "x"),
    check "2" ("2x+3", "-8x+10") (divide "2x3-4x+7x2+7" "x2+2x-1"),
    check "3" ("3x2-5x+6","") (divide "12x3-11x2+9x+18" "4x+3"),
    check "4" ("-3y 2xy x","") (divide "x2+2x2y-2xy+2xy2-3y2" "x+y")
    -- Never completes
    -- check "5" ("","") (divide "x16+y20+2" "x10+y10-5")
    ]
  where check s a b = TestLabel s $ TestCase $ assertEqual "" a b

-- http://www.mesacc.edu/~scotz47781/mat120/notes/divide_poly/long_division/long_division_practice.html
-- http://math.stackexchange.com/questions/1496716/avoiding-infinite-loop-in-multi-variable-polynomial-division#comment3049027_1496725


testMul :: Assertion
testMul = assertEqual "1 1"
    ("x2 -x -2" ^* "3") "3x2 -3x -6"


main :: IO ()
main = defaultMain $ hUnitTestToTests tests
  where tests = TestList [testDivide, TestCase testMul]

