{-# OPTIONS_GHC -fth #-}
module Utility.Apply 
( apply
, applyN
) where

import Language.Haskell.TH
import Control.Monad

applyN n t f = do
    c <- newName "c"
    TyConI (DataD _ _ _ cstrs _) <- reify t
    (pats,vars) <- genPE $ n-1
    matches <- mapM (genMatch f vars) cstrs
    lamE (pats ++ [varP c]) (caseE (varE c) matches)

apply t f = do
    c <- newName "c"
    TyConI (DataD _ _ _ cstrs _) <- reify t
    matches <- mapM (genMatch f []) cstrs
    lamE [varP c] (caseE (varE c) matches)

genMatch f vars' (NormalC cname ts) = do
    (cpats,cvars) <- genPE (length ts)
    let vars = vars' ++ cvars
    return $ if (length ts < 1 || length vars < 1)
              then match (conP cname []) (normalB (varE f)) []
              else do
                --let gE = foldl (\g' v -> appE g' v) (appE (varE g) $ head cvars) $ tail cvars
                match (conP cname [x | x <- cpats]) (normalB $ foldl (\f' v -> appE f' v) (appE (varE f) $ head vars) $ tail vars) []

genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)
