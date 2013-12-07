{-# LANGUAGE NoMonomorphismRestriction #-}
module Common ((~>), (!), (<$>), (<!>),
               pure, (<*>), (<$), (*>),
               (<*), intercalate, Name,
               foldl', foldr,
               Render(..)) where

import Prelude hiding (foldl', foldr)
import Data.Foldable (foldl', foldr)
import Data.List (intercalate)
import Control.Applicative
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
