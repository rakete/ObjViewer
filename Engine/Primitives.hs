{-# OPTIONS_GHC -fglasgow-exts #-}
module Engine.Primitives
( GridPrimitive(..)
, PlanePrimitive(..)
, BoxPrimitive(..)
--, SpherePrimitive(..)
) where

import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.BufferObjects
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL

import qualified Data.Map as M
import Foreign.Storable

import Engine.Factory
import Engine.Geometry
import Parser.ObjParser
import Math.Vector
import Utility.List

data GridPrimitive = GridPrimitive (Bool, Bool, Bool) GLfloat GLfloat GLfloat

instance Factory GridPrimitive Mesh where
    construct (GridPrimitive axis a_size b_size spacing) = do
        let (vs,is) = (create2DGrid axis a_size b_size spacing (Vertex3 0 0 0)) :: ([Vertex3 GLfloat],[GLuint])
        vid <- newVBO ArrayBuffer vs StaticDraw
        iid <- newVBO ElementArrayBuffer is StaticDraw
        return $ Mesh vid Nothing Nothing Nothing iid [] [] [] (length is) Nothing

--
--

data PlanePrimitive = PlanePrimitive GLfloat GLfloat GLfloat

instance Factory PlanePrimitive Mesh where
    construct (PlanePrimitive asize bsize spacing) = do
        let (a1,a2) = (0 - asize/2, 0 + asize/2)
            (b1,b2) = (0 - bsize/2, 0 + bsize/2)
            xs = [a1,a1+spacing..a2]
            zs = [b1,b1+spacing..b2]
            vs = [ Vertex3 x 0.0 z | x <- xs, z <- zs ]
            ns = [ Vertex3 x 1.0 z | (Vertex3 x _ z) <- vs ]
            u = fromIntegral $ length xs
            v = fromIntegral $ length zs
            is = genIndices u v 0 1 1

        vid <- newVBO ArrayBuffer vs StaticDraw
        nid <- newVBO ArrayBuffer ns StaticDraw
        iid <- newVBO ElementArrayBuffer is StaticDraw

        return $ Mesh vid (Just nid) Nothing Nothing iid [] [] [] (length is) Nothing

        where

        genIndices :: GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> [GLuint]
        genIndices u v i x y | x == u = []
                             | y == v = genIndices u v (i+1) (x+1) 1
                             | otherwise = [i, i+v, i+v+1, i+1] ++ genIndices u v (i+1) x (y+1)


--
--

data BoxPrimitive = BoxPrimitive GLfloat GLfloat GLfloat (Color4 GLfloat)

instance Factory BoxPrimitive Mesh where
    construct (BoxPrimitive h w d color) = do
        let (h1,h2) = (0 - h/2, 0 + h/2)
            (w1,w2) = (0 - w/2, 0 + w/2)
            (d1,d2) = (0 - d/2, 0 + d/2)
            vs =
                [ Vertex3 x y z | x <- [w1], y <- [h1,h2], z <- [d1,d2] ] ++
                [ Vertex3 x y z | x <- [w2], y <- [h1,h2], z <- [d1,d2] ] ++
                [ Vertex3 x y z | x <- [w1,w2], y <- [h1], z <- [d1,d2] ] ++
                [ Vertex3 x y z | x <- [w1,w2], y <- [h2], z <- [d1,d2] ] ++
                [ Vertex3 x y z | x <- [w1,w2], y <- [h1,h2], z <- [d1] ] ++
                [ Vertex3 x y z | x <- [w1,w2], y <- [h1,h2], z <- [d2] ]
            is = ([0,1,2
                  ,3,2,1
                  ,4,6,5
                  ,7,5,6
                  ,8,10,9
                  ,11,9,10
                  ,12,13,14
                  ,15,14,13
                  ,20,22,21
                  ,23,21,22
                  ,18,16,19
                  ,17,19,16] :: [GLuint])
            ns =
                [ Vertex3 x y z | x <- [w1 - 1.0], y <- [h1,h2], z <- [d1,d2] ] ++
                [ Vertex3 x y z | x <- [w2 + 1.0], y <- [h1,h2], z <- [d1,d2] ] ++
                [ Vertex3 x y z | x <- [w1,w2], y <- [h1 - 1.0], z <- [d1,d2] ] ++
                [ Vertex3 x y z | x <- [w1,w2], y <- [h2 + 1.0], z <- [d1,d2] ] ++
                [ Vertex3 x y z | x <- [w1,w2], y <- [h1,h2], z <- [d1 - 1.0] ] ++
                [ Vertex3 x y z | x <- [w1,w2], y <- [h1,h2], z <- [d2 + 1.0] ]

        vid <- newVBO ArrayBuffer vs StaticDraw
        nid <- newVBO ArrayBuffer ns StaticDraw
        iid <- newVBO ElementArrayBuffer is StaticDraw

        return $ Mesh vid (Just nid) Nothing Nothing iid [] [] [] (length is) (Just color)

--
--

{-data SpherePrimitive = SpherePrimitive GLfloat GLuint GLuint (Color4 GLfloat)

instance Factory SpherePrimitive Mesh where
    construct (SpherePrimitive r u v color) = do
        let dtheta = fromIntegral $ 180 `div` v
            dphi = fromIntegral $ 360 `div` u
            vs = (do
                    theta <- [((-90.0)+dtheta)..(90.0-dtheta)]
                    phi <- [0..(360-dphi)]
                    return $ Vertex3 ((cos theta)*(cos phi)*(sqrt r)) ((cos theta)*(sin phi)*(sqrt r)) ((sin theta)*(sqrt r))
                 )
                 --[ Vertex3 ((cos (-90.0))*(sqrt r)) ((cos (-90.0))*(sqrt r)) ((sin (-90.0))*(sqrt r))
                 --, Vertex3 ((cos 90.0)*(sqrt r)) ((cos 90.0)*(sqrt r)) ((sin 90.0)*(sqrt r)) ]
            is = genIndices 1 1 1

        vid <- newVBO ArrayBuffer vs StaticDraw
        iid <- newVBO ElementArrayBuffer is StaticDraw

        return $ Mesh vid Nothing Nothing Nothing iid [] [] [] (length is) (Just color)

        where

        genIndices :: GLuint -> GLuint -> GLuint -> [GLuint]
        genIndices i x y | x == u = []
                         | y == v = genIndices (i+1) (x+1) 1
                         | x == u-1 = [i, i+1, y+1, y] ++ genIndices (i+1) x (y+1)
                         | otherwise = [i, i+1, i+1+v, i+v] ++ genIndices (i+1) x (y+1)-}
