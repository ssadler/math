{-# LANGUAGE OverloadedStrings #-}
module Math where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Char (isDigit, isSpace)
import Data.List (nub, sort)
import Data.Monoid
import Data.Text as T
-- import Lens.Micro.Platform


data Sign = Positive | Negative
    deriving (Show)

data Variable n = Constant n | Variable Char Int
    deriving (Show)

data Monomial n = Monomial Sign [Variable n]
    deriving (Show)

data Polynomial n = Polynomial [Monomial n]
    deriving (Show)


parseMonomial :: Integral n => Parser (Monomial n)
parseMonomial = do
    sign <- takeSign
    skipSpace
    parts <- many1 (constant <|> variable)
    endOfInput
    let vars = sort $ [n | Variable n _ <- parts]
        mon = Monomial sign parts
    if nub vars /= vars then fail "Repeated vars" else pure mon
  where
    takeSign = (char '-' >> pure Negative) <|> pure Positive
    constant = Constant <$> decimal
    variable = Variable <$> satisfy (inClass "a-z")
                        <*> (("^" >> decimal) <|> pure 1)


monomial :: Integral n => Text -> Monomial n
monomial = either error id . parseOnly parseMonomial
