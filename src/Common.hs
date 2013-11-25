module Common ((~>),
               (!),
               (<$>),
               pure,
               (<*>),
               (<$),
               (*>),
               (<*),
               intercalate,
               Name) where

import Data.List (intercalate)
import Control.Applicative

(~>) = flip (.)
infixr 9 ~>

(!) = flip ($)
infixl 0 !

type Name = String
