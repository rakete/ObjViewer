{-# OPTIONS_GHC -fglasgow-exts -fth #-}
module Utility.Tuple
( fst3, snd3, thd3
, fst4
, tupleN
, toTuple
, fromTuple
) where

import Language.Haskell.TH
import Control.Monad

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a

fst4 (a,_,_,_) = a

tupleN n m = do
    let n' = if n>m then m else n
    x <- newName "x"
    return $ LamE [TupP ( (replicate (n-1) WildP) ++ [VarP x] ++ (replicate (m-n) WildP) )] (VarE x)

toTuple :: Name -> ExpQ
toTuple d = do
     TyConI (DataD _ _ _ cstrs _) <- reify d
     if length cstrs == 1
      then do
        (cname,n) <- case head cstrs of
                         (NormalC cname' ts') -> return (cname',length ts')
                         (RecC cname' ts') -> return (cname',length ts')
                         otherwise -> fail "toTuple: no suitable constructor found"
        (pats,vars) <- genPE n
        lamE [conP cname pats] (tupE vars)
      else fail "toTuple: given data structure has not exactly 1 constructor"

fromTuple :: Name -> ExpQ
fromTuple d = do
     TyConI (DataD _ _ _ cstrs _) <- reify d
     if length cstrs == 1
      then do
        (cname,n) <- case head cstrs of
                         (NormalC cname' ts') -> return (cname',length ts')
                         (RecC cname' ts') -> return (cname',length ts')
                         otherwise -> fail "toTuple: no suitable constructor found"
        (pats,vars) <- genPE n
        lamE [tupP pats] $ genAppE (conE cname) vars
      else fail "toTuple: given data structure has not exactly 1 constructor"

genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)

genAppE :: ExpQ -> [ExpQ] -> ExpQ
genAppE expr vars = foldl (\expr' v -> appE expr' v) (appE expr $ head vars) $ tail vars


