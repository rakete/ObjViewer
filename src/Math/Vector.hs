{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Math.Vector
( Vector(..)
, vertex2Tuple
, vector2Tuple
, vertex3Tuple
, vector3Tuple
, vertex4Tuple
, color4Tuple
, insideRect
, insideTriangle
, intersect
, sameSide
, slope
, angleBetween
) where

import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.CoordTrans

class (Num c,Floating c,Ord c) => Vector v c | v -> c where
    newVector :: [c] -> v
    fromVector :: Vector v1 c => v -> v1
    vecToList :: v -> [c]
    vecSetC :: Vector v1 c => v -> Int -> c -> v1
    vecGetC :: v -> Int -> c
    vecDim :: v -> Int
    vecMap :: (c -> c) -> v -> v
    vecFold :: (c -> c -> c) -> v -> c
    vecSub :: v -> v -> v
    vecAdd :: v -> v -> v
    dot :: v -> v -> c
    vecMinC :: v -> c
    vecMaxC :: v -> c
    vecLength :: v -> c
    norm :: v -> v
    vecNegate :: v -> v
    crossProduct :: v -> v -> v

    -- these defaults are still untested, i have just written them out after i discovered i could
    -- give default implementations for typeclass functions
    vecSetC v i c = newVector $ snd $
        foldl (\(k,xs) b ->
                  if k==i
                   then (k+1,xs ++ [c])
                   else (k+1,xs ++ [b])
              ) (0,[]) $ vecToList v
    vecGetC v i = ((vecToList v) ++ [0,1,2,3,4,5,6,7,8,9,10]) !! i
    vecDim v = length $ vecToList v
    vecMap f v = newVector $ map f $ vecToList v
    vecFold f v = foldl1 f $ vecToList v
    vecSub v1 v2 = newVector $ map (\(c1,c2) -> c1 - c2) $ zip (vecToList v1) (vecToList v2)
    vecAdd v1 v2 = newVector $ map (\(c1,c2) -> c1 + c2) $ zip (vecToList v1) (vecToList v2)
    dot v1 v2 = sum $ map (\(c1,c2) -> c1 * c2) $ zip (vecToList v1) (vecToList v2)
    vecMinC v = minimum $ vecToList v
    vecMaxC v = maximum $ vecToList v
    vecLength v = sqrt $ dot v v
    norm v = vecMap (/(vecLength v)) v
    vecNegate v = vecMap negate v
    crossProduct v w =
        let (Vector3 x1 y1 z1) = fromVector v
            (Vector3 x2 y2 z2) = fromVector w
        in newVector [(y1*z2 - z1*y2),(z1*x2 - x1*z2),(x1*y2 - y1*x2)]

vertex2Tuple (Vertex2 x y) = (x,y)
vector2Tuple (Vector2 x y) = (x,y)
vertex3Tuple (Vertex3 x y z) = (x,y,z)
vector3Tuple (Vector3 x y z) = (x,y,z)
vertex4Tuple (Vertex4 x y z w) = (x,y,z,w)
color4Tuple (Color4 r g b a) = (r,g,b,a)

instance (Ord c,Floating c) => Vector (Vertex2 c) c where
    newVector (x:y:_) = Vertex2 x y
    vecToList (Vertex2 x y) = [x,y]
    fromVector (Vertex2 x y) = newVector [x,y,0.0,1.0]

    vecMap f (Vertex2 x y) = Vertex2 (f x) (f y)
    vecFold f (Vertex2 x y) = f x y
    vecSub (Vertex2 x1 y1) (Vertex2 x2 y2) = Vertex2 (x1-x2) (y1-y2)
    vecAdd (Vertex2 x1 y1) (Vertex2 x2 y2) = Vertex2 (x1+x2) (y1+y2)
    dot (Vertex2 x1 y1) (Vertex2 x2 y2) = (x1*x2)+(y1*y2)
    vecMinC (Vertex2 x y) = minimum [x,y]
    vecMaxC (Vertex2 x y) = maximum [x,y]
    vecLength (Vertex2 x y) = sqrt (x*x + y*y)
    norm v@(Vertex2 x y) =
        let l = vecLength v
        in Vertex2 (x/l) (y/l)
    vecNegate (Vertex2 x y) = Vertex2 (negate x) (negate y)

instance (Ord c,Floating c) => Vector (Vector2 c) c where
    newVector (x:y:_) = Vector2 x y
    vecToList (Vector2 x y) = [x,y]
    fromVector (Vector2 x y) = newVector [x,y,0.0,1.0]
    vecMap f (Vector2 x y) = Vector2 (f x) (f y)
    vecFold f (Vector2 x y) = (f x y)
    vecSub (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1-x2) (y1-y2)
    vecAdd (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1+x2) (y1+y2)
    dot (Vector2 x1 y1) (Vector2 x2 y2) = (x1*x2)+(y1*y2)
    vecMinC (Vector2 x y) = minimum [x,y]
    vecMaxC (Vector2 x y) = maximum [x,y]
    vecLength (Vector2 x y) = sqrt (x*x + y*y)
    norm v@(Vector2 x y) =
        let l = vecLength v
        in Vector2 (x/l) (y/l)
    vecNegate (Vector2 x y) = Vector2 (negate x) (negate y)

instance (Ord c,Floating c) => Vector (Vertex3 c) c where
    newVector (x:y:z:_) = Vertex3 x y z
    vecToList (Vertex3 x y z) = [x,y,z]
    fromVector (Vertex3 x y z) = newVector [x,y,z,1.0]
    vecMap f (Vertex3 x y z) = Vertex3 (f x) (f y) (f z)
    vecFold f (Vertex3 x y z) = f (f x y) z
    vecSub (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vertex3 (x1-x2) (y1-y2) (z1-z2)
    vecAdd (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vertex3 (x1+x2) (y1+y2) (z1+z2)
    dot (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)
    vecMinC (Vertex3 x y z) = minimum [x,y,z]
    vecMaxC (Vertex3 x y z) = maximum [x,y,z]
    vecLength (Vertex3 x y z) = sqrt (x*x + y*y + z*z)
    norm v@(Vertex3 x y z) =
        let l = vecLength v
        in Vertex3 (x/l) (y/l) (z/l)
    vecNegate (Vertex3 x y z) = Vertex3 (negate x) (negate y) (negate z)

instance (Ord c,Floating c) => Vector (Vector3 c) c where
    newVector (x:y:z:_) = Vector3 x y z
    vecToList (Vector3 x y z) = [x,y,z]
    fromVector (Vector3 x y z) = newVector [x,y,z,1.0]
    vecMap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
    vecFold f (Vector3 x y z) = f (f x y) z
    vecSub (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)
    vecAdd (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1+x2) (y1+y2) (z1+z2)
    dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)
    vecMinC (Vector3 x y z) = minimum [x,y,z]
    vecMaxC (Vector3 x y z) = maximum [x,y,z]
    vecLength (Vector3 x y z) = sqrt (x*x + y*y + z*z)
    norm v@(Vector3 x y z) =
        let l = vecLength v
        in Vector3 (x/l) (y/l) (z/l)
    vecNegate (Vector3 x y z) = Vector3 (negate x) (negate y) (negate z)

instance (Ord c,Floating c) => Vector (Vertex4 c) c where
    newVector (x:y:z:w:_) = Vertex4 x y z w
    vecToList (Vertex4 x y z w) = [x,y,z,w]
    fromVector (Vertex4 x y z w) = newVector [x,y,z,w]
    vecMap f (Vertex4 x y z w) = Vertex4 (f x) (f y) (f z) (f w)
    vecFold f (Vertex4 x y z w) = f (f (f x y) z) w
    vecSub (Vertex4 x1 y1 z1 w1) (Vertex4 x2 y2 z2 w2) = Vertex4 (x1-x2) (y1-y2) (z1-z2) (w1-w2)
    vecAdd (Vertex4 x1 y1 z1 w1) (Vertex4 x2 y2 z2 w2) = Vertex4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
    dot (Vertex4 x1 y1 z1 w1) (Vertex4 x2 y2 z2 w2) = (x1*x2)+(y1*y2)+(z1*z2)+(w1*w2)
    vecMinC (Vertex4 x y z w) = minimum [x,y,z,w]
    vecMaxC (Vertex4 x y z w) = maximum [x,y,z,w]
    vecLength (Vertex4 x y z _) = sqrt (x*x + y*y + z*z)
    norm v@(Vertex4 x y z w) =
        let l = vecLength v
        in Vertex4 (x/l) (y/l) (z/l) w
    vecNegate (Vertex4 x y z w) = Vertex4 (negate x) (negate y) (negate z) w

instance (Ord c,Floating c) => Vector (Color4 c) c where
    newVector (x:y:z:w:_) = Color4 x y z w
    vecToList (Color4 x y z w) = [x,y,z,w]
    fromVector (Color4 x y z w) = newVector [x,y,z,w]
    vecMap f (Color4 x y z w) = Color4 (f x) (f y) (f z) (f w)
    vecFold f (Color4 x y z w) = f (f (f x y) z) w
    vecSub (Color4 x1 y1 z1 w1) (Color4 x2 y2 z2 w2) = Color4 (x1-x2) (y1-y2) (z1-z2) (w1-w2)
    vecAdd (Color4 x1 y1 z1 w1) (Color4 x2 y2 z2 w2) = Color4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
    dot (Color4 x1 y1 z1 w1) (Color4 x2 y2 z2 w2) = (x1*x2)+(y1*y2)+(z1*z2)+(w1*w2)
    vecMinC (Color4 x y z w) = minimum [x,y,z,w]
    vecMaxC (Color4 x y z w) = maximum [x,y,z,w]
    vecLength (Color4 x y z _) = sqrt (x*x + y*y + z*z)
    norm v@(Color4 x y z w) =
        let l = vecLength v
        in Color4 (x/l) (y/l) (z/l) w
    vecNegate (Color4 x y z w) = Color4 (negate x) (negate y) (negate z) w

instance (Ord c,Floating c) => Vector (c,c,c) c where
    newVector (x:y:z:_) = (x,y,z)
    vecToList (x,y,z) = [x,y,z]
    fromVector (x,y,z) = newVector [x,y,z,0]

--
--

insideRect :: (Floating c,Ord c) => Vertex2 c -> Vertex2 c -> Vertex2 c -> Bool
insideRect p q v =
    let t = (xp > xq, yp > yq)
        (Vertex2 xp yp) = p
        (Vertex2 xq yq) = q
        (Vertex2 x y) = v
    in case t of
        (False,False) -> (x >= xp && x <= xq && y >= yp && y <= yq)
        (False,True) -> (x >= xp && x <= xq && y <= yp && y >= yq)
        (True,False) -> (x <= xp && x >= xq && y >= yp && y <= yq)
        (True,True) -> (x <= xp && x >= xq && y <= yp && y >= yq)

insideTriangle :: (Num c,VertexComponent c, Vector v c) => v -> v -> v -> v -> Bool
insideTriangle a b c v = (sameSide a b c v) && (sameSide b c a v) && (sameSide c a b v)

intersect :: (RealFloat c,Floating c,Ord c,VertexComponent c) => Vertex2 c -> Vertex2 c -> Vertex2 c -> Vertex2 c -> Maybe (Vertex2 c)
intersect a p@(Vertex2 xp yp) b q@(Vertex2 xq yq) =
    let mp = slope a p
        mq = slope b q
        x = ((-1)*mq*xq + yq + mp*xp - yp)/(mp - mq)
    in if isNaN x
        then Nothing
        else Just $ Vertex2 x (mp*(x-xp)+yp)

sameSide :: (Num c, Vector v c) => v -> v -> v -> v -> Bool
sameSide p' q' r' v' =
    let p = p'
        q = fromVector q'
        r = fromVector r'
        v = fromVector v'
        cp1 = (q `vecSub` p) `crossProduct` (v `vecSub` p)
        cp2 = (q `vecSub` p) `crossProduct` (r `vecSub` p)
    in (cp1 `dot` cp2) >= 0.0

slope :: (Floating c,Ord c, VertexComponent c) => Vertex2 c -> Vertex2 c -> c
slope = (\v2a v2b -> (\(Vertex2 x y) -> y/x) $ (v2a `vecSub` v2b))

angleBetween :: (RealFloat c, Fractional c, Vector v c) => v -> v -> c
angleBetween a b =
    case vecDim a of
     2 -> atan2 ((vecGetC a 0) * (vecGetC b 1) - (vecGetC a 1) * (vecGetC b 0)) (a `dot` b)
     3 -> acos ((a `dot` b) / (vecLength a * vecLength b))
     otherwise -> error "angleBetween only works for 2 and 3 dimensional vectors"

testor1 :: [Float]
testor1 =
    [ angleBetween (Vertex3 0 1 0) (Vertex3 0 1 0)
    , angleBetween (Vertex3 0 1 0) (Vertex3 1 1 0)
    , angleBetween (Vertex3 0 1 0) (Vertex3 1 0 0)
    , angleBetween (Vertex3 0 1 0) (Vertex3 1 (-1) 0)
    , angleBetween (Vertex3 0 1 0) (Vertex3 0 (-1) 0)
    , angleBetween (Vertex3 0 1 0) (Vertex3 (-1) (-1) 0)
    , angleBetween (Vertex3 0 1 0) (Vertex3 (-1) 0 0)
    , angleBetween (Vertex3 0 1 0) (Vertex3 (-1) 1 0) ]

testor2 :: [Float]
testor2 =
    [ angleBetween (Vertex2 0 1) (Vertex2 0 1)
    , angleBetween (Vertex2 0 1) (Vertex2 1 1)
    , angleBetween (Vertex2 0 1) (Vertex2 1 0)
    , angleBetween (Vertex2 0 1) (Vertex2 1 (-1))
    , angleBetween (Vertex2 0 1) (Vertex2 0 (-1))
    , angleBetween (Vertex2 0 1) (Vertex2 (-1) (-1))
    , angleBetween (Vertex2 0 1) (Vertex2 (-1) 0)
    , angleBetween (Vertex2 0 1) (Vertex2 (-1) 1) ]
