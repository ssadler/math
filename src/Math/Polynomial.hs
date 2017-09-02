{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Math.Polynomial where


import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Char (ord)
import Data.Decimal
import Data.Either (partitionEithers)
import Data.List (intercalate, sortBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, fromJust)
import Data.Monoid
import Data.Ord (comparing)
import Data.String
import Data.Text (Text, pack)


-------------------------------------------------------------------------------
-- Term (aka Monomial)
-------------------------------------------------------------------------------

type TermVars = Map.Map Char Int

data Term = Term { coefficient :: Decimal, variables :: TermVars }
    deriving (Eq)


-- | Showing
--
instance Show Term where
    show (Term co vars) = sco ++ Map.foldMapWithKey ss vars
      where sco = if co /= 1 || vars == Map.empty then show co else ""
            ss k a = k : showExp a

showExp :: Int -> String
showExp e = if e == 1 then "" else show e -- map ("⁰¹²³⁴⁵⁶⁷⁸⁹"!!) digits
  where digits = [ord c - 48 | c <- show e]


-- | Constructing
--
term :: Decimal -> [(Char, Int)] -> Term
term co = Term co . Map.fromListWith (+)


textToTerm :: Text -> Term
textToTerm = either error id . parseOnly (parseTerm <* endOfInput)


parseTerm :: Parser Term
parseTerm = do
    f <- skipSpace >> sign <* skipSpace
    (took, co) <- (,) True <$> coeff <|> pure (False, 1)
    vars <- many' variable <* skipSpace
    let t = term (f co) vars
    if took || vars /= [] then pure t else fail "No term"
  where
    sign = ("-" >> pure negate) <|> ("+" >> pure id) <|> pure id
    coeff = fromInteger <$> decimal
    variable = (,) <$> satisfy (inClass "a-z") <*> (decimal <|> pure 1)


instance IsString Term where
    fromString = textToTerm . pack


data TermClass = TermClass String
    deriving (Eq, Ord, Show)


-- | Divide a term by another term, returning Nothing if they are
-- not divisible into another (monomial) term
termDivide :: Term -> Term -> Maybe Term
termDivide (Term co1 dee) (Term co2 der) =
    let divideExp e1 e2 = if e1 > e2 then Just (e1 - e2) else Nothing
        vars = Map.differenceWith divideExp dee der
        sumDegree = Map.foldr (+) 0
        co = co1 / co2
        worked = sumDegree dee - sumDegree der == sumDegree vars && co /= 0
    in if worked then Just (Term co vars) else Nothing


-- | Multiply two terms
termMultiply :: Term -> Term -> Term
termMultiply (Term co1 vars1) (Term co2 vars2) = Term (co1*co2) vars3
  where vars3 = Map.unionWith (+) vars1 vars2


-- | Get degree of term
termDegree :: Term -> Int
termDegree (Term _ vars) = Map.foldr (+) 0 vars


-- | Solve a term
termSolve :: [(Char, Decimal)] -> Term -> Term
termSolve [] t = t
termSolve ((k,v):xs) (Term co vars) = termSolve xs $ Term nco nvars
  where nco = co * maybe 1 (v^) (Map.lookup k vars)
        nvars = Map.delete k vars


-- | Utility type to compare vars
-- aka Lexicographic ordering
newtype TermSig = TermSig TermVars deriving (Eq)
instance Ord TermSig where
    compare = comparing (\(TermSig vars) -> Map.toAscList vars)


-------------------------------------------------------------------------------
-- Polynomial
---------------------------------------------------------------------------------

newtype Polynomial = P [Term] deriving (Eq)


textToPolynomial :: Text -> Polynomial
textToPolynomial = either error reduce . parseOnly parsePoly
  where parsePoly = P <$> many' parseTerm <* endOfInput


instance Monoid Polynomial
  where
    mappend (P a) (P b) = reduce $ P (a ++ b)
    mempty = P [] 


instance Show Polynomial
    where show (P terms) = intercalate " " $ map show terms


instance IsString Polynomial
    where fromString = textToPolynomial . pack


(^*) :: Polynomial -> Polynomial -> Polynomial
(P terms1) ^* (P terms2) = reduce $ distribution
  where distribution = P [termMultiply t1 t2 | t1 <- terms1, t2 <- terms2]
infixl 7 ^*


-- | Simplify a polynomial
-- This is done by finding the terms with equal signatures and multiplying
-- their coefficients.
fix :: Polynomial -> Polynomial
fix (P terms) = P $ sortBy (comparing cmp) reduced
  where idx = Map.fromListWith (+) [(TermSig vars, co) | Term co vars <- terms]
        reduced = [Term co vars | (TermSig vars, co) <- Map.toList idx, co /= 0]
        cmp (Term _ vars) = (- Map.foldr (+) 0 vars, - Map.foldr max 0 vars)


-- | Reduce a polynomial. Returns in lexicographic order.
reduce :: Polynomial -> Polynomial
reduce (P terms) = P reduced
  where idx = Map.fromListWith (+) [(TermSig vars, co) | Term co vars <- terms]
        reduced = [Term co vars | (TermSig vars, co) <- Map.toDescList idx, co /= 0]


-- | Divide polynomials
-- Returns (quotient, remainder)
divide :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
divide num den = (reduce $ P lefts, reduce $ P rights)
  where
    (lefts, rights) = partitionEithers $ divide' (reduce num) (fix den)


divide' :: Polynomial -> Polynomial -> [Either Term Term]
divide' (P []) _                   = []
divide' (P (t:ts)) (P denominator) =
        -- Only take the largest denominators.
    let maxDegree = foldr (max . termDegree) 0 denominator
        dens = filter ((== maxDegree) . termDegree) denominator
        -- Try to divide by any of the larger denominators
        mres = listToMaybe $ catMaybes $ map (termDivide t) dens
        -- Distribute the result
        dist = P denominator ^* P [fromJust mres]
        -- Subtract to get the remainder
        remainder = P (t:ts) <> "-1" ^* dist
    in case mres of
        Nothing -> Right t : divide' (P ts) (P denominator)
        Just res -> Left res : divide' remainder (P denominator)


solve :: Char -> Decimal -> Polynomial -> Polynomial
solve x y (P terms) = reduce $ P $ map (termSolve [(x,y)]) terms
