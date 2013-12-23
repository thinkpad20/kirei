-- Module for the default/initial versions of type checking data
module Defaults where

import Common
import AST
import Types
import qualified Data.Map as M
import qualified Data.Set as S

defaultTypeMap = M.fromList
  [
    ("+", witha $ numa :=> numa :=> numa)
  , ("-", witha $ numa :=> numa :=> numa)
  , ("*", witha $ numa :=> numa :=> numa)
  , ("/", witha $ numa :=> numa :=> numa)
  , ("<", witha $ compa :=> compa :=> bool)
  , (">", witha $ compa :=> compa :=> bool)
  , ("<=", witha $ compa :=> compa :=> bool)
  , (">=", witha $ compa :=> compa :=> bool)
  , ("==", witha $ eqa :=> eqa :=> bool)
  , ("!=", witha $ eqa :=> eqa :=> bool)
  , ("(if)", witha $ bool :=> a :=> a :=> a)
  , ("[]", witha $ listT a)
  , ("::", witha $ a :=> listT a :=> listT a)
  , ("(fail)", witha a)
  , ("(error)", witha a)
  , ("(or)", witha $ a :=> a :=> a)
  , ("(range)", witha $ a :=> a :=> listT a)
  , ("show", witha $ a' ["Show"] :=> str)
  , ("map", withab $ (a :=> b) :=> f a :=> f b)
  ]
  where witha = Polytype ["a"]
        withab = Polytype ["a", "b"]
        a = TVar [] "a"
        b = TVar [] "b"
        a' classes = TVar classes "a"
        f = TApply (TVar ["Functor"] "f")
        listT = TApply (TConst "[]")
        maybeT = TApply (TConst "Maybe")
        numa = TVar ["Num"] "a"
        compa = TVar ["Comp"] "a"
        eqa = TVar ["Eq"] "a"

defaultInstances = M.fromList
  [
    ("Num",         S.fromList [num])
  , ("Applicative", S.fromList [list])
  , ("Eq",          S.fromList [ num
                               , str
                               , bool
                               , listOf (a ["Eq"])
                               ])
  , ("Functor",     S.fromList [list])
  , ("Show",        S.fromList [ num
                               , str
                               , bool
                               , listOf (a ["Show"])
                               ])
  , ("Monoid",      S.fromList [ num
                               , listOf (a [])])
  , ("Comp",       S.fromList [ num
                               , str
                               , listOf (a ["Comp"])])
  ]
  where list = TConst "[]"
        listOf = TApply list
        a classes = TVar classes "a"

defaultTypeClasses :: M.Map Name TypeClass
defaultTypeClasses = M.fromList
  [
    ("Applicative", TC (type_ :=> type_) "g" [pure_, apply_] ["Functor"])
  , ("Functor", TC (type_ :=> type_) "f" [map_] [])
  , ("Monad", TC (type_ :=> type_) "m" [return_, bind_] ["Applicative"])
  , ("Show", TC type_ "a" [show_] [])
  , ("Eq", TC type_ "a" [eq_] [])
  , ("Ord", TC type_ "a" [succ_] ["Eq"])
  ]
  where a = TVar [] "a"
        a' name = TVar [name] "a"
        b = TVar [] "b"
        f = TApply (TVar ["Functor"] "f")
        g = TApply (TVar ["Applicative"] "g")
        m = TApply (TVar ["Monad"] "m")
        pure_ = TSig "pure" (a :=> g a)
        apply_ = TSig "<*>" (g (a :=> b) :=> g a :=> g b)
        map_ = TSig "map" ((a :=> b) :=> f a :=> f b)
        eq_ = TSig "==" (a' "Eq" :=> a' "Eq" :=> bool)
        succ_ = TSig "succ" (a' "Ord" :=> a' "Ord")
        return_ = TSig "return" (a :=> m a)
        bind_ = TSig ">>=" (m a :=> (a :=> m b) :=> m b)
        show_ = TSig "show" (a' "Show" :=> str)
