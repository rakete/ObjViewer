{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Engine.Factory
( Factory(..)
) where

class Factory a b | a -> b where
    construct :: a -> IO b
