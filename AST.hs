module AST where

data Sig = Sig String Bool TypeSig deriving (Show)

data TypeSig = TypeName TypeName
             | MapFrom TypeSig TypeSig
             deriving (Show)

type TypeName = String