{-# LANGUAGE OverloadedStrings #-}
module Math.Terms where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.List (groupBy, sort)
import Data.Text (Text)
-- import Lens.Micro.Platform


data Sign = Positive | Negative
    deriving (Eq)

data Class = Class String
    deriving (Eq, Ord, Show)

data Variable = Constant Int | Variable Char Int
    deriving (Eq, Ord)

data Term = Term Sign [Variable]

data Polynomial = Polynomial [Term]

---

showExp :: Int -> String
showExp e = let lst = mod e 10
                hed = quot e 10
                pre = if hed > 0 then showExp hed else ""
            in pre ++ ("⁰¹²³⁴⁵⁶⁷⁸⁹" !! lst : "")

instance Show Variable where
    show (Constant n) = show n
    show (Variable x e) = x : showExp e

instance Show Term where
    show (Term sign vars) = s ++ concatMap show vars
        where s = if sign == Negative then "-" else ""

instance Show Polynomial where
    show (Polynomial mons) = concatMap show mons

---


combineVars :: [Variable] -> [Variable]
combineVars = map combine . groupBy isSame . sort
  where
    combine vars@(Constant _:_)   = Constant $ foldr (*) 1 [n | Constant n <- vars]
    combine vars@(Variable x _:_) = Variable x $ foldr (+) 0 [e | Variable _ e <- vars]
    combine []                    = undefined
    isSame (Constant _) (Constant _)     = True
    isSame (Variable x _) (Variable y _) = x == y
    isSame _ _                           = False


parseTerm :: Parser Term
parseTerm = do
    sign <- takeSign
    skipSpace
    parts <- many1 (constant <|> variable)
    endOfInput
    return $ Term sign $ combineVars parts
  where
    takeSign = (char '-' >> pure Negative) <|> pure Positive
    constant = Constant <$> decimal
    variable = Variable <$> satisfy (inClass "a-z")
                        <*> (("^" >> decimal) <|> pure 1)


term :: Text -> Term
term = either error id . parseOnly parseTerm


termClass :: Term -> Class
termClass (Term _ vars) = Class $ concat [show v | v@(Variable _ _) <- vars]

