{-# LANGUAGE OverloadedStrings #-}
module Math.Terms where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Char (ord)
import Data.Either (partitionEithers)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import Data.String
import Data.Text (Text, pack)

-------------------------------------------------------------------------------
-- Term (aka Monomial)
-------------------------------------------------------------------------------

type TermVars = Map.Map Char Int

data Term = Term { coefficient :: Int, variables :: TermVars }
    deriving (Eq)


-- | Showing
--
instance Show Term where
    show (Term co vars) = sco ++ Map.foldMapWithKey ss vars
      where sco = if co /= 1 || vars == Map.empty then show co else ""
            ss k a = k : showExp a

showExp :: Int -> String
showExp e = if e == 1 then "" else map ("⁰¹²³⁴⁵⁶⁷⁸⁹"!!) digits
  where digits = [ord c - 48 | c <- show e]


-- | Constructing
--
term :: Int -> [(Char, Int)] -> Term
term co = Term co . Map.fromListWith (+)


textToTerm :: Text -> Term
textToTerm = either error id . parseOnly parseTerm


instance IsString Term where
    fromString = textToTerm . pack


parseTerm :: Parser Term
parseTerm = term <$> (signed decimal <|> pure 1)
                 <*> many variable
                  <* endOfInput
  where
    variable = (,) <$> satisfy (inClass "a-z")
                   <*> (decimal <|> pure 1)


data TermClass = TermClass String
    deriving (Eq, Ord, Show)


-- | Divide a term by another term, returning Nothing if they are
-- not divisible into another (monomial) term
termDiv :: Term -> Term -> Maybe Term
termDiv (Term co1 dee) (Term co2 der) =
    let divideExp e1 e2 = if e1 > e2 then Just (e1 - e2) else Nothing
        vars = Map.differenceWith divideExp dee der
        sumDegree = Map.foldr (+) 0
        co = if mod co1 co2 == 0 then quot co1 co2 else 0
        worked = sumDegree dee - sumDegree der == sumDegree vars && co /= 0
    in if worked then Just (Term co vars) else Nothing


-- | Multiply two terms
termMul :: Term -> Term -> Term
termMul (Term co1 vars1) (Term co2 vars2) = Term (co1*co2) vars3
  where vars3 = Map.unionWith (+) vars1 vars2


-- | Get degree of term
termDegree :: Term -> Int
termDegree (Term _ vars) = Map.foldr (+) 0 vars


-- | Utility type to compare vars
newtype TermSig = TermSig TermVars deriving (Eq)
instance Ord TermSig where
    compare = comparing (\(TermSig vars) -> Map.toAscList vars)


-------------------------------------------------------------------------------
-- Polynomial
---------------------------------------------------------------------------------

type Polynomial = [Term]


polynomialMul :: Polynomial -> Polynomial -> Polynomial
polynomialMul terms1 terms2 = polynomialReduce $ distribution
  where distribution = [termMul t1 t2 | t1 <- terms1, t2 <- terms2]


-- | Simplify a polynomial
-- This is done by finding the terms with equal signatures and multiplying
-- their coefficients.
polynomialReduce :: Polynomial -> Polynomial
polynomialReduce dist = polynomialSort reduced
  where idx = Map.fromListWith (+) [(TermSig vars, co) | Term co vars <- dist]
        reduced = [Term co vars | (TermSig vars, co) <- Map.toList idx, co /= 0]


polynomialSort :: Polynomial -> Polynomial
polynomialSort = sortBy (comparing cmp)
  where cmp (Term _ vars) = (- Map.foldr (+) 0 vars, - Map.foldr max 0 vars)

-- | Divide polynomials
-- Returns (quotient, remainder)
dividePolynomial :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
dividePolynomial num den = (polynomialReduce lefts, polynomialReduce rights)
  where
    (lefts, rights) = partitionEithers $ divide' (polynomialSort num) den


divide' :: [Term] -> [Term] -> [Either Term Term]
divide' [] _ = []
divide' (t:ts) denominator =
        -- Only take the largest denominators.
    let maxDegree = foldr (max . termDegree) 0 denominator
        dens = filter ((== maxDegree) . termDegree) denominator
        -- Try to divide by any of the larger denominators
        mres = listToMaybe $ catMaybes $ map (termDiv t) dens
        -- Distribute the result
        dist = case mres of
            Just res -> polynomialMul (denominator) [res]
            Nothing -> []
        -- Subtract to get the remainder
        remainder = if dist == [] then ts
                                  else polynomialReduce $ (t:ts) ++ polynomialMul ["-1"] dist
        rest = divide' remainder denominator
    in case mres of Nothing  -> Right t : rest
                    Just res -> Left res : rest
