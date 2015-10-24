{-# LANGUAGE OverloadedStrings #-}
module Math.Terms where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.List (groupBy, sort)
import Data.Ord (comparing)
import Data.Text (Text)

-------------------------------------------------------------------------------
-- Variable
-------------------------------------------------------------------------------

data Variable = Variable { name :: Char, power :: Int }
    deriving (Eq, Ord)

instance Show Variable where
    show (Variable x e) = x : if e == 1 then "" else showExp e


-------------------------------------------------------------------------------
-- Term (aka Monomial)
-------------------------------------------------------------------------------

data Term = Term { coefficient :: Int, variables :: [Variable] }
    deriving (Eq)


-- | Showing
--
showExp :: Int -> String
showExp e = let lst = mod e 10
                hed = quot e 10
                pre = if hed > 0 then showExp hed else ""
            in pre ++ ("⁰¹²³⁴⁵⁶⁷⁸⁹" !! lst : "")


instance Show Term where
    show (Term co vars) = sco ++ concatMap show vars
      where sco = if co /= 1 then show co else ""


-- | Sorting
--
instance Ord Term where
    compare = comparing maxExp
      where 
        maxExp (Term _ vars) = foldr max 1 [power v | v <- vars]


-- | Constructing
--
term :: Int -> [Variable] -> Term
term co = Term co . map combine . groupBy isSame . sort
  where
    combine vars@(v0:_) = Variable (name v0) $ foldr (+) 0 $ map power vars
    combine []          = undefined
    isSame (Variable x _) (Variable y _) = x == y


textToTerm :: Text -> Term
textToTerm = either error id . parseOnly parseTerm


parseTerm :: Parser Term
parseTerm = term <$> (signed decimal <|> pure 1)
                 <*> many1 variable
                  <* endOfInput
  where
    variable = Variable <$> satisfy (inClass "a-z")
                        <*> (("^" >> decimal) <|> pure 1)


data TermClass = TermClass String
    deriving (Eq, Ord, Show)


termClass :: Term -> TermClass
termClass (Term _ vars) = TermClass $ concat [show v | v@(Variable _ _) <- vars]


termDegree :: Term -> Int
termDegree (Term co vars) = undefined


-------------------------------------------------------------------------------
-- Polynomial
---------------------------------------------------------------------------------

data Polynomial = Polynomial [Term]


instance Show Polynomial where
    show (Polynomial terms) = concatMap show terms


polynomial :: [Term] -> Polynomial
polynomial = Polynomial . reverse . sort


dividePolynomial :: Polynomial -> Polynomial -> Polynomial
dividePolynomial (Polynomial num) (Polynomial den) =
    divide' (reverse $ sort num) (reverse $ sort den)


divide' :: [Term] -> [Term] -> [Term]
divide numerator denominator =
    let numHighest = head $ groupBy termDegree numerator
        denHighest = head $ groupBy termDegree demoninator
        cartProd <- [(x,y) | x <- , y <- ys]
        
    
    
-- On each iteration
-- Take term from denominator with higest degree (or one of them)
-- find a term wtih equal or higher degree to divide
-- do division, distribute result
-- subtract from original polynomial, repeat
