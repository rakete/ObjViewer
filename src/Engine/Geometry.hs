module Engine.Geometry
( newVBO
, Indices
, FaceGroup(..)
, Mesh(..)
, renderMeshWith
, create2DGrid
, triangulatePolygon
) where

import Graphics.Rendering.OpenGL.GL hiding (get,newMatrix)

import Data.Maybe
import Foreign
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Math.Vector
import Engine.Texture

-- thanks quicksilver
newVBO :: (Storable a) => BufferTarget -> [a] -> BufferUsage -> IO BufferObject
newVBO buf xs mode = do
    [bo] <- genObjectNames 1
    bindBuffer buf $= Just bo
    withArray xs $ \p ->
        bufferData buf $= (fromIntegral (length xs * sizeOf (head xs)),p,mode)
    return bo

type Indices a = [a]

data FaceGroup = FaceGroup
    { face_offset :: Int
    , face_numIndices :: Int
    , face_material :: Material GLfloat
    }
    deriving Show

data Mesh = Mesh
    { mesh_vertices :: BufferObject
    , mesh_normals :: Maybe BufferObject
    , mesh_texcoords :: Maybe BufferObject
    , mesh_vertexcolors :: Maybe BufferObject
    , mesh_indices :: BufferObject
    , mesh_opaqueFaces :: [FaceGroup]
    , mesh_translucentFaces :: [FaceGroup]
    , mesh_poses :: [(String,BufferObject)]
    , mesh_numIndices :: Int
    , mesh_color :: Maybe (Color4 GLfloat)
    }
    deriving Show

renderMeshWith :: Mesh -> IO () -> IO ()
renderMeshWith mesh fio = do
    clientState VertexArray $= Enabled
    clientState IndexArray $= Enabled

    let n = fromIntegral $ mesh_numIndices mesh

    bindBuffer ArrayBuffer $= (Just $ mesh_vertices mesh)
    arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 nullPtr

    _ <- case mesh_normals mesh of
            (Just nid) -> do
                clientState NormalArray $= Enabled
                bindBuffer ArrayBuffer $= Just nid
                arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 nullPtr
            otherwise -> return ()

    _ <- case mesh_texcoords mesh of
            (Just tid) -> do
                clientState TextureCoordArray $= Enabled
                bindBuffer ArrayBuffer $= Just tid
                arrayPointer TextureCoordArray $= VertexArrayDescriptor 3 Float 0 nullPtr
            otherwise -> return ()

    bindBuffer ElementArrayBuffer $= (Just $ mesh_indices mesh)
    arrayPointer IndexArray $= VertexArrayDescriptor 1 UnsignedInt 0 nullPtr

    fio

    clientState IndexArray $= Disabled
    clientState TextureCoordArray $= Disabled
    clientState NormalArray $= Disabled
    clientState VertexArray $= Disabled


create2DGrid :: (Enum a, Vector v1 a, Vector v2 a, Integral i,VertexComponent a) =>
    (Bool, Bool, Bool) -> a -> a -> a -> v1 -> ([v2], [i])
create2DGrid axis a_size b_size spacing pos = create where
    Vertex3 x_pos y_pos z_pos = fromVector $ pos

    cs c_pos c_size = [(c_pos - (c_size/2)),((c_pos - (c_size/2))+spacing)..(c_pos + (c_size/2))]

    createIndices as bs (i:is) (x:xs) | i == (length as) * (length bs) = []
                                      | mod i (length as) == 0 || mod i (length as) == (length as)-1 = [x] ++ createIndices as bs is xs
                                      | otherwise = [x,x] ++ (createIndices as bs is xs)

    indicesB as bs (i:n:is) = (take (length bs) $ iterate (+(length as)) i) ++ (indicesB as bs (n:is))

    createIndex as bs = (createIndices as bs [0..] [0..]) ++ (createIndices as bs [0..] (indicesB as bs [0..]))

    create = case axis of
            (False,_,_) ->
                let as = cs y_pos a_size
                    bs = cs z_pos b_size
                in ([newVector (x:y:z:[0.0]) | x <- [x_pos], y <- as, z <- bs],
                    map fromIntegral $ createIndex as bs)
            (_,False,_) ->
                let as = cs x_pos a_size
                    bs = cs z_pos b_size
                in ([newVector (x:y:z:[0.0]) | x <- as, y <- [y_pos], z <- bs],
                    map fromIntegral $ createIndex as bs)
            (_,_,False) ->
                let as = cs x_pos a_size
                    bs = cs y_pos b_size
                in ([newVector (x:y:z:[0.0]) | x <- as, y <- bs, z <- [z_pos]],
                    map fromIntegral $ createIndex as bs)
            otherwise -> ([],[])

{-triangulatePolygon :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex3 c) -> Indices a -> (Bool,[Indices a])
triangulatePolygon _ t@(TriangleIndices _) = (True,[t])
triangulatePolygon _ (QuadIndices (i1,i2,i3,i4)) = (True,[TriangleIndices (i1,i2,i3),TriangleIndices (i1,i3,i4)])
triangulatePolygon verticesm (PolygonIndices allindices) =
    if n > 3
     then recur allindices Nothing
     else let t = (\(a:b:c:[]) -> (a,b,c)) allindices
          in (True,[TriangleIndices t])

    where

    vertices = M.map fromVector $ verticesm --M.filterWithKey (\k _ -> any (==(fromIntegral k)) indices) verticesm
    n = fromIntegral(M.size verticesm)
    discard_component = -- (Bool,Bool,Bool)
        (\v -> (\(x,y,z) -> let b = vecMinC v in (x==b,y==b,z==b)) $ vertex3Tuple v) $
            foldl (\v1 v2 -> vecMap (/n) $ v1 `vecAdd` v2) (Vertex3 0.0 0.0 0.0) $
                scanl1 (\v1 v2 -> vecMap abs $ (v1 `vecSub` v2)) $ M.elems vertices
    flat_vertices = case discard_component of
                        (True,_,_) -> M.map ((\(Vertex3 _ y z) -> Vertex2 y z)) vertices
                        (_,True,_) -> M.map ((\(Vertex3 x _ z) -> Vertex2 x z)) vertices
                        (_,_,True) -> M.map ((\(Vertex3 x y _) -> Vertex2 x y)) vertices

    recur indices@(i1:i2:i3:is) mi =
         let v1 = flat_vertices M.! (fromIntegral i1)
             v2 = flat_vertices M.! (fromIntegral i2)
             v3 = flat_vertices M.! (fromIntegral i3)
             ts = filter (\x -> not (x==i1 || x==i2 || x==i3)) allindices
             vs = map (\i -> flat_vertices M.! (fromIntegral i)) ts
         in let b' = not $ any (==True) $ map (\v -> insideTriangle v1 v2 v3 v) vs
            in if null is
                then (b',[TriangleIndices (i1,i2,i3)])
                else if b'
                      then let (b,tris) = recur (i1:i3:is) (Just i1) -- wrong! this was not what i thought of, but it works...
                           in if b
                               then (b,[TriangleIndices (i1,i2,i3)] ++ tris)
                               else if isJust mi
                                     then if fromJust mi == i1
                                           then (False,(takeachance indices) ++ tris)
                                           else recur ((i2:i3:is) ++ [i1]) mi
                                     else recur ((i2:i3:is) ++ [i1]) (Just i1)
                      else if isJust mi
                            then if fromJust mi == i1
                                  then (False,takeachance indices)
                                  else recur ((i2:i3:is) ++ [i1]) mi
                            else recur ((i2:i3:is) ++ [i1]) (Just i1)

    takeachance indices@(i1:i2:i3:is) =
        if null is
         then [TriangleIndices (i1,i2,i3)]
         else [TriangleIndices (i1,i2,i3)] ++ takeachance (i1:i3:is)-}

--
--

foldCyclicLinked :: (a -> b -> b -> b -> a) -> a -> [b] -> a
foldCyclicLinked f a xs = recur (last xs) xs a where
    recur x [] a = a
    recur x [y] a = f a x y (head xs)
    recur x (y:z:rs) a = recur y (z:rs) $ f a x y z

foldCyclicLinkedWithCont :: (a -> b -> b -> b -> [b] -> a) -> a -> [b] -> a
foldCyclicLinkedWithCont f a xs = recur (last xs) xs a where
    recur x [] a = a
    recur x [y] a = f a x y (head xs) []
    recur x (y:z:rs) a = recur y (z:rs) $ f a x y z rs

mapCyclicLinked :: (a -> a -> a -> b) -> [a] -> [b]
mapCyclicLinked f xs = recur (last xs) xs where
    recur _ [] = []
    recur x [y] = [f x y (head xs)]
    recur x (y:z:rs) = f x y z : recur y (z:rs)

submap :: Ord k => M.Map k a -> [k] -> M.Map k a
submap m ks = foldl (\m k -> M.insert k (m M.! k) m) M.empty ks

polygonVertexAngle :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex2 c) -> a -> a -> a -> (c,c)
polygonVertexAngle vertices prev current next =
    let --is = indicesAsList indices
        --(prev:current:next:[]) = (take 1 $ drop 1 $ dropWhile (/=i) $ reverse is) ++ (take 2 $ dropWhile (/=i) is)
        v1 = (vertices M.! prev) `vecSub` (vertices M.! current)
        v2 = (vertices M.! next) `vecSub` (vertices M.! current)
        a = (angleBetween v1 v2) / (pi/180)
        a' = if a < 0 then 180 + (180 + a) else a
    in (a',360-a')

polygonAngles :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex2 c) -> [a] -> M.Map a (c,c)
polygonAngles vertices is = foldCyclicLinked accumAngles M.empty is where
    accumAngles m prev current next = M.insert current (polygonVertexAngle vertices prev current next) m

polygonInnerAngles :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex2 c) -> [a] -> M.Map a c
polygonInnerAngles vertices is =
    let angles = polygonAngles vertices is
        (alpha,beta,am,bm) =
            M.foldWithKey (\k (a,b) (alpha,beta,am,bm) ->
                            (alpha+a, beta+b, M.insert k a am, M.insert k b bm)
                          ) (0.0,0.0,M.empty,M.empty) angles
    in if alpha < beta
        then am
        else bm

polygonOuterAngles :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex2 c) -> [a] -> M.Map a c
polygonOuterAngles vertices is =
    let angles = polygonAngles vertices is
        (alpha,beta,am,bm) =
            M.foldWithKey (\k (a,b) (alpha,beta,am,bm) ->
                            (alpha+a, beta+b, M.insert k a am, M.insert k b bm)
                          ) (0.0,0.0,M.empty,M.empty) angles
    in if alpha > beta
        then am
        else bm

flatten :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex3 c) -> [a] -> M.Map a (Vertex2 c)
flatten vertices is =
    let poly = catMaybes $ map (\i -> M.lookup i vertices) is
        n = length poly
        (xg:yg:zg:xs:ys:zs:[]) =
            catMaybes $ foldl (\(xg:yg:zg:xs:ys:zs:[]) (Vertex3 x y z) ->
                            map Just $ concat $
                                [ [ case maybe_old of; (Just old) -> if new > old then new else old; otherwise -> new | (maybe_old,new) <- [(xg,x),(yg,y),(zg,z)]]
                                , [ case maybe_old of; (Just old) -> if new < old then new else old; otherwise -> new | (maybe_old,new) <- [(xs,x),(ys,y),(zs,z)]] ]
                              ) (take 6 $ repeat Nothing) poly
        (xb:yb:zb:[]) =
           let vs = [xg - xs, yg - ys, zg -zs]
               minv = minimum vs
           in map (== minv) vs
    in case (xb,yb,zb) of
        (True,_,_) -> M.map ((\(Vertex3 _ y z) -> Vertex2 y z)) vertices
        (_,True,_) -> M.map ((\(Vertex3 x _ z) -> Vertex2 x z)) vertices
        (_,_,True) -> M.map ((\(Vertex3 x y _) -> Vertex2 x y)) vertices

convexVertices :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex2 c) -> [a] -> [a]
convexVertices vertices is = M.keys $ M.filter (<180) $ polygonInnerAngles vertices is

reflexVertices :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex2 c) -> [a] -> [a]
reflexVertices vertices is = M.keys $ M.filter (>180) $ polygonInnerAngles vertices is

earVertices :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex2 c) -> [a] -> [a]
earVertices vertices is = foldCyclicLinked accumEars [] is where
    accumEars acc prev current next =
        let v1 = vertices M.! prev
            v2 = vertices M.! current
            v3 = vertices M.! next
            convexs = convexVertices vertices is
            reflexs = filter (\i -> i /= prev && i /= current && i /= next) $ reflexVertices vertices is
            triangleTest = not $ any id $ map (\i -> insideTriangle v1 v2 v3 $ vertices M.! i) reflexs
            earTest = any (==current) convexs
        in if earTest
            then if triangleTest
                  then acc ++ [current]
                  else acc
            else acc

cutEar :: (Num a,Integral a) => [a] -> a -> [a]
cutEar [] _ = []
cutEar [_] _ = []
cutEar (_:_:[]) _ = []
cutEar (a:b:c:[]) _ = [a,b,c]
cutEar is@(a:b:c:d:xs) i =
    let es = foldCyclicLinkedWithCont (\es prev current next cont ->
                if current == i
                 then [prev,current,next]
                 else es
             ) [] is
    in if length es == 0
        then error "Nothing to cut"
        else es

triangulatePolygon :: (Num a,Integral a,RealFloat c,Fractional c,Ord c,Enum c,VertexComponent c) => M.Map a (Vertex3 c) -> Indices a -> (Bool,Indices a)
triangulatePolygon _ t@(a:b:c:[]) = (True,t)
triangulatePolygon _ (i1:i2:i3:i4:[]) = (True,[i1,i2,i3,i1,i3,i4])
triangulatePolygon vertices is =
    let es = earVertices vertices2d is
    in (True, concat $ triangulate es is) where

    vertices2d = flatten vertices is
    triangulate _ [] = []
    triangulate _ [_] = []
    triangulate _ (_:_:[]) = []
    triangulate _ (a:b:c:[]) = [[a,b,c]]
    triangulate _ (a:b:c:d:[]) = [[a,b,c],[a,c,d]]
    triangulate (e:_) is@(_:_:_:_:_) =
        let ret = cutEar is e
            is' = filter (/=e) is
            es' = earVertices vertices2d is'
        in ret : triangulate es' is'
    triangulate [] is = takeachance is

    takeachance indices@(i1:i2:i3:is) =
        if null is
         then [[i1,i2,i3]]
         else [i1,i2,i3] : takeachance (i1:i3:is)
