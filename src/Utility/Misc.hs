{-# LANGUAGE FlexibleInstances #-}
module Utility.Misc
( xor
) where

class Show' a where
    show' :: a -> String

instance Show' (Maybe String) where
    show' ms = maybe "" id ms

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a
