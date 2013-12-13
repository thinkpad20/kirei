{-# LANGUAGE NoMonomorphismRestriction #-}
module Common ((~>), (!), (<$>), (<!>),
               pure, (<*>), (<$), (*>),
               (<*), intercalate, Name,
               foldl', foldr, isInt, unionAll,
               Render(..), mconcat, (<>), (>>==)) where

import Prelude hiding (foldl', foldr)
import Data.Foldable (foldl', foldr)
import Data.List (intercalate)
import Control.Applicative
import Data.Monoid
import qualified Data.Set as S

(~>) = flip (.)
infixr 9 ~>

(!) = flip ($)
infixl 0 !

(<!>) = flip (<$>)
infixr 4 <!>

type Name = String

class Render a where
  render :: Int -> a -> String

instance (Render a, Render b) => Render (a, b) where
  render n (a, b) = "(" ++ render n a ++ ", " ++ render n b ++ ")"

instance Render a => Render (Maybe a) where
  render _ Nothing = "Nothing"
  render n (Just a) = render n a

--Returns if x is an int to n decimal places
isIntTo :: (Integral a, RealFrac b) => b -> a -> Bool
isIntTo x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

isInt x = isIntTo x 10

unionAll = foldl' S.union S.empty

monad >>== function = monad >>= \a -> function >> return a
