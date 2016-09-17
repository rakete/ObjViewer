{-# OPTIONS_GHC -fglasgow-exts #-}
module Utility.Monad
( liftTa
, liftTb
, (>>-)
) where

liftTa :: (Monad m) => (m a,b) -> m (a,b)
liftTa (ma,b) = do
    a <- ma
    return (a,b)

liftTb :: (Monad m) => (a,m b) -> m (a,b)
liftTb (a,mb) = do
    b <- mb
    return (a,b)

(>>-) :: Monad m => m a -> (a -> b) -> m b
(>>-) m f = m >>= return . f




