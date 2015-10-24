{-# LANGUAGE OverloadedStrings #-}
module Math.Terms where

import Control.Applicative
import Data.Attoparsec.Text as A
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import Data.String
import Data.Text (Text, pack)
import Debug.Trace

showExp :: Int -> String
showExp e = let lst = mod e 10
                hed = quot e 10
                pre = if hed > 0 then showExp hed else ""
            in pre ++ ("⁰¹²³⁴⁵⁶⁷⁸⁹" !! lst : "")


-------------------------------------------------------------------------------
-- Term (aka Monomial)
-------------------------------------------------------------------------------

data Term = Term { coefficient :: Int, variables :: Map.Map Char Int }
    deriving (Eq)


-- | Showing
--
instance Show Term where
    show (Term co vars) = sco ++ Map.foldMapWithKey ss vars
      where sco = if co /= 1 then show co else ""
            ss k a = k : showExp a


-- | Sorting
--
instance Ord Term where
    compare = comparing maxExp
      where 
        maxExp (Term _ vars) = Map.foldr max 1 vars


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
                 <*> many1 variable
                  <* endOfInput
  where
    variable = (,) <$> satisfy (inClass "a-z")
                   <*> (("^" >> decimal) <|> pure 1)


data TermClass = TermClass String
    deriving (Eq, Ord, Show)


termDivide :: Term -> Term -> Maybe Term
termDivide (Term co1 dee) (Term co2 der) =
    let divideVar e1 e2 = if e1 > e2 then Just (e1 - e2) else Nothing
        vars = Map.differenceWith divideVar dee der
        sumDegree = Map.foldr (+) 0
        co = if mod co1 co2 == 0 then quot co1 co2 else 0
        worked = sumDegree dee - sumDegree der == sumDegree vars && co /= 0
    in if worked then Just (Term co vars) else Nothing

-------------------------------------------------------------------------------
-- Polynomial
---------------------------------------------------------------------------------

type Polynomial = [Term]


-- dividePolynomial :: Polynomial -> Polynomial -> Polynomial
-- dividePolynomial (Polynomial num) (Polynomial den) =
--     divide' (reverse $ sort num) (reverse $ sort den)
-- 
-- 
-- divide' :: [Term] -> [Term] -> [Term]
-- divide (t:ts) denominator =
--     let
--         mdiv = listToMaybe $ catMaybes $ map (termDivide t) denominator
--     in
--         case mdiv of Nothing -> undefined
--                      _ -> undefined
        
        

-- On each iteration
-- Take term from denominator with higest degree (or one of them)
-- find a term wtih equal or higher degree to divide
-- do division, distribute result
-- subtract from original polynomial, repeat
