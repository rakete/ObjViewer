{-# OPTIONS_GHC -fglasgow-exts #-}
module Utility.Misc
( xor
) where

import Maybe

class Show' a where
    show' :: a -> String

instance Show' (Maybe String) where
    show' ms = maybe "" id ms

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a
