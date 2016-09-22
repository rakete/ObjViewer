module Math.Metric
( Metric(..)
, lerp
) where

class Metric a where
    distance :: Floating b => a -> a -> b

instance Metric Int where
    distance a b = fromIntegral $ a - b

instance Metric Float where
    distance a b = realToFrac $ a - b

lerp :: (Metric a,Floating b) => (a -> b) -> a -> a -> a -> b
lerp f x0 x1 x = (f x0)+(f x1 - f x0)/(distance x1 x0)*(distance x x0)


