module Lisp where

import Data.Either

type LSym = String

data LExpr
  = LAtom LAtom
  | LFunc ([LExpr] -> LExpr)
  | LCons LExpr LExpr

instance Eq LExpr where
  (LAtom x) == (LAtom y) = x == y
  (LCons x y) == (LCons x' y') = x == x' && y == y'
  _ == _ = False

instance Show LExpr where
  show (LAtom x) = "LAtom " ++ show x
  show (LCons x y) = "LCons " ++ show x ++ " . " ++ show y
  show (LFunc _) = "LFunc ..."

data LAtom
  = LInt Int
  | LFlo Float
  | LSym LSym
  | LTrue
  | LFalse
  | LStr String
  | LNil
  deriving (Eq, Show)

type LEnv = [(LSym, LExpr)]

evaluate :: LExpr -> LEnv -> Either String LExpr
evaluate exp env =
  case exp of
    LAtom atm ->
      case atm of
        LSym key -> lookupSym key
        _ -> Right (LAtom atm)
    LCons _ _ ->
      if isList exp then Right (LAtom (LInt 3)) else Right exp


    -- LCons x y ->
    --   case getRights (map (`evaluate` env) lst) of
    --     Left x -> Left x
    --     Right (LFunc f : args) -> Right (f args)



    _ ->
      Right exp


  where
    getRights :: [Either String LExpr] -> Either String [LExpr]
    getRights xs =
      case partitionEithers xs of
        ((s:_), _) -> Left s
        ([], xs) -> Right xs

    lookupSym key =
      case lookup key env of
        Just val' -> Right val'
        _ -> Left ("cannot find " ++ key)



isList :: LExpr -> Bool
isList expr =
  case expr of
    LAtom LNil -> True
    LCons _ expr' -> isList expr'
    _ -> False

mkLList :: [LExpr] -> LExpr
mkLList [] = LAtom LNil
mkLList (x:xs) = LCons x (mkLList xs)

mkHList :: LExpr -> [LExpr]
mkHList (LAtom LNil) = []
mkHList (LCons x y) = x : mkHList y