module Utility.List
( allEqual
, single
, zip3x
, split
, maybeHead
, fillGaps
, groupEvery
, takeWhileEscaped
) where

import Data.List

allEqual :: Eq a => [a] -> Bool
allEqual (x:xs) = not $ any (not.(== x)) xs
allEqual [] = True

single :: [a] -> Bool
single xs = length xs == 1

zip3x :: [a] -> ([b],b) -> ([c],c) -> [(a,b,c)]
zip3x (a:as) ((b:bs),b') ((c:cs),c') = [(a,b,c)] ++ zip3x as (bs,b') (cs,c')
zip3x (a:as) ((b:bs),b') ([],c') = [(a,b,c')] ++ zip3x as (bs,b') ([],c')
zip3x (a:as) ([],b') ((c:cs),c') = [(a,b',c)] ++ zip3x as ([],b') (cs,c')
zip3x (a:as) ([],b') ([],c') = [(a,b',c')] ++ zip3x as ([],b') ([],c')
zip3x [] _ _ = []

flipflop x | x== 0 = [0] ++ (flipflop 1)
           | x== 1 = [1] ++ (flipflop 0)
           | otherwise = []

split :: Eq a=> a -> [a] -> [[a]]
split c s = foldl (\a b -> if b== c then (a ++ [[]]) else (init a) ++ [(last a) ++ [b]]) [[]] s

maybeHead :: [a] -> Maybe a
maybeHead xs =
    if null xs
     then Nothing
     else Just $ head xs


fillGaps :: (Ord a,Enum a) => a -> [a] -> [a]
fillGaps l xs =
    let xs' = sort $ nub xs
    in gaps l xs'

    where

    gaps z [] = [z]
    gaps z [a] = tail [a..z]
    gaps z (a:b:s) = (init.tail $ [a..b]) ++ (gaps z (b:s))


groupEvery :: Int -> [a] -> [[a]]
groupEvery n s =
    (\(xs,ys) -> xs ++ [take 3 (ys ++ (repeat undefined))]) $ foldl (\(xs,ys) b ->
        if length ys == n
         then (xs ++ [ys],[b])
         else (xs,ys ++ [b])
    ) ([],[]) s

takeWhileEscaped :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeWhileEscaped _ _ [] = []
takeWhileEscaped f _ [x] = if f x then [x] else []
takeWhileEscaped f e (x:xs) =
    if e x
     then (take 1 xs) ++ (takeWhileEscaped f e $ drop 1 xs)
     else if f x
           then x : (takeWhileEscaped f e xs)
           else []
