module Common ((~>),
               (!),
               (<$>),
               pure,
               (<*>),
               (<$),
               (*>),
               (<*),
               intercalate,
               Name,
               foldl',
               Render(..)) where

import Data.List (intercalate)
import Control.Applicative
import Data.List (foldl')

(~>) = flip (.)
infixr 9 ~>

(!) = flip ($)
infixl 0 !

type Name = String

class Render a where
  render :: Int -> a -> String
