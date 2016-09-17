{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL (get)
import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Foreign
import Control.Monad.Trans
import Control.DeepSeq
import Data.List
import System.Environment

import Data.Word
import Data.Int

import Math.Vector
import Math.Quaternion

import Parser.ObjParser
import Engine.Texture
import Engine.Geometry
import Engine.Factory
import Engine.Primitives

data VBOData = VBOData
        { geometryId :: BufferObject
        , normalsId :: BufferObject
        , texcoordsId :: BufferObject
        , indicesId :: BufferObject
        , vboFaces :: [FaceGroup]
        }

instance Factory (Maybe (ObjScene GLfloat GLuint)) (M.Map String VBOData,M.Map String VBOData) where
    construct mobjscene = do
        if isJust mobjscene
            then do
                let objects_map = objects $ fromJust mobjscene
                let maybe_mtllib = mtllib $ fromJust mobjscene
                (newvbos,newtransvbos) <- M.foldWithKey (vboUploadObject maybe_mtllib) (return (M.empty,M.empty)) objects_map
                return (newvbos,newtransvbos)
            else return (M.empty,M.empty)

        where

        vboUploadObject mmtllib _ mesh io = do
            putStrLn $ "Uploading " ++ (objmesh_name mesh) ++ "..."

            let (vertexlist,normallist,texcoordlist,indiceslist) = (objmesh_data mesh) :: ([Vertex3 GLfloat],[Normal3 GLfloat],[TexCoord2 GLfloat],Indices GLuint)
                --triangulated_indiceslist :: [Indices GLuint] = concat $ map (triangulatePolygon (M.fromList $ zip [0..] vertexlist)) indiceslist

            putStrLn $ show $ last vertexlist
            putStrLn $ show $ last normallist
            putStrLn $ show $ last texcoordlist
            putStrLn $ show $ last indiceslist

            vid <- newVBO ArrayBuffer vertexlist StaticDraw
            nid <- newVBO ArrayBuffer normallist StaticDraw
            tid <- newVBO ArrayBuffer texcoordlist StaticDraw
            iid <- newVBO ElementArrayBuffer indiceslist StaticDraw

            let gs = (M.elems $ groups mesh)
            ts <- sequence $ map (\g -> do
                --let is = indicesAsList $ vertex_indices g
                let offset = group_offset g
                let ni = group_numIndices g
                let mat = if isJust mmtllib
                           then if isJust (group_mat g)
                                 then M.findWithDefault defaultMaterial (fromJust $ group_mat g) (fromJust mmtllib)
                                 else defaultMaterial
                           else defaultMaterial
                let trans = (let (_,d) = material_dissolve mat in d < 1.0)
                --iid <- newVBO ElementArrayBuffer is StaticDraw
                --print offset
                --print ni
                if trans
                 then return $ (Nothing,Just $ FaceGroup offset ni mat)
                 else return $ (Just $ FaceGroup offset ni mat,Nothing) ) gs

            putStrLn "Bar"

            let faces = catMaybes $ fst $ unzip ts
            let trans_faces = catMaybes $ snd $ unzip ts

            (m,mt) <- io
            let newvbo = VBOData vid nid tid iid faces
            let trans_newvbo = VBOData vid nid tid iid trans_faces
            return $ case (null faces, null trans_faces) of
                (False,False) -> (M.insert (objmesh_name mesh) newvbo m,M.insert (objmesh_name mesh) trans_newvbo mt)
                (False,True) -> (M.insert (objmesh_name mesh) newvbo m,mt)
                (True,False) -> (m,M.insert (objmesh_name mesh) trans_newvbo mt)
                (True,True) -> (m,mt)

createVBOs :: StateT ViewerState IO ()
createVBOs = do
    mobjscene <- gets currentObjScene
    (newvbos,newtransvbos) <- liftIO $ construct mobjscene
    updateVBOs newvbos newtransvbos

createGridVBO :: StateT ViewerState IO ()
createGridVBO = do
    grid <- liftIO $ construct $ GridPrimitive (True,False,True) 20.0 20.0 1.0
    modify (\s -> s{ currentGrid = Just $ grid })

data ViewerState =
    ViewerState
        { currentGrid :: Maybe Mesh
        , currentPath :: Maybe FilePath
        , currentObjScene :: Maybe (ObjScene GLfloat GLuint)
        , currentVBOs :: (M.Map String VBOData)
        , transluentVBOs :: (M.Map String VBOData)
        , isDirty :: IORef Bool
        , quitNow :: IORef Bool
        , cameraMode :: IORef Bool
        , orbitX :: IORef (GLint,GLfloat)
        , orbitY :: IORef (GLint,GLfloat)
        , zoom :: IORef (Int,GLdouble)
        , showWireframe :: IORef Bool
        }

initStateIO :: IO ViewerState
initStateIO = do
    isdirty <- newIORef True
    quitnow <- newIORef False
    cameramode <- newIORef False
    orbitx <- newIORef ((-1),0.0)
    orbity <- newIORef ((-1),0.0)
    z <- newIORef (0,20.0)
    showwire <- newIORef False
    return $ ViewerState Nothing Nothing Nothing M.empty M.empty isdirty quitnow cameramode orbitx orbity z showwire

updateFileState :: Maybe FilePath -> Maybe (ObjScene GLfloat GLuint) -> StateT ViewerState IO ()
updateFileState mpath mobj = do
    modify (\v -> v{ currentPath = mpath, currentObjScene = mobj })

updateVBOs :: M.Map String VBOData -> M.Map String VBOData -> StateT ViewerState IO ()
updateVBOs mvbo mtransvbo = do
    modify (\v -> v{ currentVBOs = mvbo, transluentVBOs = mtransvbo })

loadObjFile :: FilePath -> StateT ViewerState IO ()
loadObjFile path = do
    parseResult <- liftIO $ parseObjFile path
    case parseResult of
        Left err -> do
            liftIO $ print err
            updateFileState (Just path) Nothing
            return ()
        Right obj -> do
            liftIO $ putStrLn $ "Success loading : " ++ path
            updateFileState (Just path) (Just obj)
            return ()

terminateGL :: IO ()
terminateGL = do
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

initGL :: StateT ViewerState IO ()
initGL = do
    mpath <- gets currentPath
    quit <- gets quitNow
    orbitx <- gets orbitX
    orbity <- gets orbitY
    z <- gets zoom
    liftIO $ do
        putStr "Initializing GL context..."
        GLFW.initialize
        -- open window
        GLFW.openWindow (Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
        GLFW.windowTitle $= "ObjViewer : " ++ (show mpath)
        shadeModel $= Smooth
        -- enable antialiasing
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        clearColor $= Color4 0.9 0.9 0.9 1.0
        depthFunc $= Just Lequal
        frontFace $= CCW
        cullFace $= Just Back

        colorMaterial $= Nothing
        normalize $= Enabled
        lighting $= Enabled

        ambient (Light 0) $= Color4 0.2 0.2 0.2 1.0
        diffuse (Light 0) $= Color4 0.3 0.3 0.3 1.0
        specular (Light 0) $= Color4 0.4 0.4 0.4 1.0
        position (Light 0) $= Vertex4 4.0 1.0 0.0 1.0
        spotDirection (Light 0) $= Normal3 (-1.0) 0.0 0.0
        spotExponent (Light 0) $= 1.0
        light (Light 0) $= Enabled

        GLFW.windowSizeCallback $= (\size@(GL.Size w h) -> do
            let aspect = (1.0*(fromIntegral w)/(fromIntegral h))
            matrixMode $= Projection
            loadIdentity
            viewport $= (Position 0 0, size)
            perspective 45 aspect 0.01 1000

            matrixMode $= GL.Modelview 0
            loadIdentity

            (_,z') <- readIORef z
            let eye = Vertex3 0.0 0.0 z'
            let center = Vertex3 0.0 0.0 0.0
            let up = Vector3 0.0 1.0 0.0
            lookAt eye center up

            (_,ox) <- readIORef orbitx
            (_,oy) <- readIORef orbity
            let q1 = rotationQuat (ox) (Vertex3 0.0 1.0 0.0) :: Vertex4 GLfloat
            let q2 = rotationQuat (oy) (Vertex3 1.0 0.0 0.0) :: Vertex4 GLfloat
            (mr :: (GLmatrix GLfloat)) <- newMatrix RowMajor $ quatMat $ q2 `quatProduct` q1
            multMatrix mr
            )
        GLFW.windowCloseCallback $= (modifyIORef quit (\_ -> True) >> readIORef quit)

        putStrLn "finished"

showCurrentMatrix :: IO ()
showCurrentMatrix = do
    (m :: GLmatrix GLfloat) <- GL.get (matrix $ Just $ Modelview 0)
    putStrLn ""
    putStrLn $ show m
    components <- getMatrixComponents ColumnMajor m
    putStrLn $ show components

viewerLoop :: StateT ViewerState IO ()
viewerLoop = do
    quit <- gets quitNow
    dirty <- gets isDirty
    cameramode <- gets cameraMode
    orbitx <- gets orbitX
    orbity <- gets orbitY
    z <- gets zoom
    showwire <- gets showWireframe
    liftIO $ do
        putStrLn "Entering viewerLoop..."
        -- disable auto polling in swapBuffers
        GLFW.disableSpecial GLFW.AutoPollEvent
        -- mark screen dirty in refresh callback which is often called
        -- when screen or part of screen comes into visibility.
        GLFW.windowRefreshCallback $= (modifyIORef dirty (\_ -> True))
        -- use key callback to track whether ESC is pressed
        GLFW.keyCallback $= (\_ _ -> do
                esc <- GLFW.getKey GLFW.ESC
                if esc == GLFW.Press
                 then modifyIORef quit (\_ -> True)
                 else return ()
                w <- GLFW.getKey $ GLFW.CharKey 'W'
                if w == GLFW.Press
                 then do
                    modifyIORef showwire (\b -> not b)
                    modifyIORef dirty (\_ -> True)
                 else return ())
        GLFW.mousePosCallback $= (\(Position x y) -> do
            c <- readIORef cameramode
            (lastx',_) <- readIORef orbitx
            (lasty',_) <- readIORef orbity
            if c
             then if not (lastx' < 0 || lasty' < 0)
                   then do
                    modifyIORef orbitx (\(lastx,ox) ->
                        let new_ox = ox + ((fromIntegral $ (x - lastx))/2)
                        in if new_ox > 360.0
                            then (x, new_ox - 360.0)
                            else if new_ox < 0.0
                                  then (x, 360.0 + new_ox)
                                  else (x, new_ox))
                    modifyIORef orbity (\(lasty,oy) ->
                        let new_oy = oy + ((fromIntegral $ (y - lasty))/2)
                        in if new_oy > 360.0
                            then (y, new_oy - 360.0)
                            else if new_oy < 0.0
                                  then (y, 360.0 + new_oy)
                                  else (y, new_oy))
                    modifyIORef dirty (\_ -> True)
                   else do
                    modifyIORef orbitx (\(_,ox) -> (x, ox))
                    modifyIORef orbity (\(_,oy) -> (y, oy))
             else return ())
        GLFW.mouseButtonCallback $= (\b s ->
            if s == GLFW.Press && b == GLFW.ButtonMiddle
                then do
                    c <- readIORef cameramode
                    if c
                     then do
                        --putStrLn "CameraMode disabled"
                        modifyIORef cameramode (\_ -> False)
                        modifyIORef orbitx (\(_,ox) -> ((-1), ox))
                        modifyIORef orbity (\(_,oy) -> ((-1), oy))
                     else do
                        --putStrLn "CameraMode enabled"
                        modifyIORef cameramode (\_ -> True)
                else return ())
        GLFW.mouseWheelCallback $= (\i -> do
            modifyIORef z (\(lastz,z') ->
                if (abs $ lastz - i) < 10
                 then
                    let new_z = z' + ((fromIntegral (lastz - i))*(z'/10))
                    in (i,new_z)
                 else (i,z'))
            modifyIORef dirty (\_ -> True))
    loop

    where

    loop :: StateT ViewerState IO ()
    loop = do
        dirty <- gets isDirty
        quit <- gets quitNow
        viewerstate <- get
        liftIO $ do
            GLFW.waitEvents
            d <- readIORef dirty
            if d
             then (render viewerstate >> GLFW.swapBuffers)
             else return ()
            modifyIORef dirty (\_ -> False)
        q <- liftIO $ readIORef quit
        if q then return () else loop



deleteVBOs :: StateT ViewerState IO ()
deleteVBOs = do
    liftIO $ return ()

data RenderMode = Opaque | Transparent

render :: ViewerState -> IO ()
render viewerstate = do
    clear [ColorBuffer,DepthBuffer]
    renderGrid
    foldl (\io (name,vbo) -> renderVBO Opaque name vbo io) (return ()) $ M.assocs $ currentVBOs viewerstate
    foldl (\io (name,vbo) -> renderVBO Transparent name vbo io) (return ()) $ M.assocs $ transluentVBOs viewerstate

    matrixMode $= GL.Modelview 0
    loadIdentity

    (_,z') <- readIORef $ zoom viewerstate
    let eye = Vertex3 0.0 0.0 z'
    let center = Vertex3 0.0 0.0 0.0
    let up = Vector3 0.0 1.0 0.0
    lookAt eye center up

    (_,ox) <- readIORef $ orbitX viewerstate
    (_,oy) <- readIORef $ orbitY viewerstate
    let q1 = rotationQuat (ox) (Vertex3 0.0 1.0 0.0) :: Vertex4 GLfloat
    let q2 = rotationQuat (oy) (Vertex3 1.0 0.0 0.0) :: Vertex4 GLfloat
    (mr :: (GLmatrix GLfloat)) <- newMatrix ColumnMajor $ quatMat $ q2 `quatProduct` q1
    multMatrix mr

    where

    renderVBO mode _ vbo io = do
        io
        --putStrLn $ "Rendering " ++ name ++ "..."

        clientState VertexArray $= Enabled
        clientState IndexArray $= Enabled
        clientState NormalArray $= Enabled
        clientState TextureCoordArray $= Enabled

        bindBuffer ArrayBuffer $= (Just $ geometryId vbo)
        arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 nullPtr

        bindBuffer ArrayBuffer $= (Just $ normalsId vbo)
        arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 nullPtr

        bindBuffer ArrayBuffer $= (Just $ texcoordsId vbo)
        arrayPointer TextureCoordArray $= VertexArrayDescriptor 2 Float 0 nullPtr

        bindBuffer ElementArrayBuffer $= (Just $ indicesId vbo)
        arrayPointer IndexArray $= VertexArrayDescriptor 1 UnsignedInt 0 nullPtr

        foldl (renderFace mode) (return ()) $ vboFaces vbo

        clientState TextureCoordArray $= Disabled
        clientState NormalArray $= Disabled
        clientState IndexArray $= Disabled
        clientState VertexArray $= Disabled

    renderFace mode io face = do
        io
        let mat = face_material face
        let (_,alpha) = material_dissolve mat
        showwire <- readIORef $ showWireframe viewerstate

        polygonOffsetFill $= Enabled
        polygonMode $= (Fill,Fill)
        polygonOffset $= (1.0,1.0)

        let offset = face_offset face
        let numindices = fromIntegral $ face_numIndices face

        --print offset
        --print numindices

        case mode of
            Opaque -> do
                materialAmbient Front $= material_ambient mat
                materialDiffuse Front $= material_diffuse mat
                materialSpecular Front $= material_specular mat
                materialEmission Front $= (fromMaybe (Color4 0.0 0.0 0.0 0.0) $ material_emission mat)
                materialShininess Front $= (realToFrac $ material_exponent mat)
                drawElements Triangles numindices UnsignedInt (plusPtr nullPtr (offset*(sizeOf (undefined :: Word32))))
                --drawRangeElements Triangles (offset_n,offset_m) numindices UnsignedInt nullPtr
            Transparent -> do
                --blendFunc $= (One,One)
                materialAmbient Front $= material_ambient mat
                let (Color4 diffuse_r diffuse_g diffuse_b _) = material_diffuse mat
                materialDiffuse Front $= Color4 diffuse_r diffuse_g diffuse_b (realToFrac alpha)
                materialSpecular Front $= material_specular mat
                materialEmission Front $= (fromMaybe (Color4 0.0 0.0 0.0 0.0) $ material_emission mat)
                materialShininess Front $= (realToFrac $ material_exponent mat)
                drawElements Triangles numindices UnsignedInt (plusPtr nullPtr (offset*(sizeOf (undefined :: Word32))))
                --blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

        if showwire
         then do
            lighting $= Disabled
            currentColor $= Color4 0.0 0.0 0.0 0.5
            polygonOffset $= ((-1.0),(-1.0))
            lineWidth $= 0.5
            lineSmooth $= Disabled
            polygonMode $= (Line,Line)
            drawElements Triangles numindices UnsignedInt (plusPtr nullPtr (offset*(sizeOf (undefined :: Word32))))
            lighting $= Enabled
         else return ()

        polygonOffsetFill $= Disabled

    --bufferPointer bo buf arr numc ctype = do
    --    bindBuffer buf $= Just bo
    --    arrayPointer arr $= VertexArrayDescriptor numc ctype 0 nullPtr

    renderGrid = do
        let mgrid = currentGrid viewerstate
        case mgrid of
            Just grid -> do
                clientState VertexArray $= Enabled
                clientState IndexArray $= Enabled

                let vid = mesh_vertices grid
                let iid = mesh_indices grid
                let n = fromIntegral $ mesh_numIndices grid

                bindBuffer ArrayBuffer $= Just vid
                bindBuffer ElementArrayBuffer $= Just iid

                arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 nullPtr
                arrayPointer IndexArray $= VertexArrayDescriptor 1 UnsignedInt 0 nullPtr

                lighting $= Disabled
                currentColor $= Color4 0.6 0.6 0.6 1.0
                lineWidth $= 0.5
                lineSmooth $= Enabled
                polygonMode $= (Fill,Fill)
                drawElements Lines n UnsignedInt nullPtr
                lighting $= Enabled

                clientState IndexArray $= Disabled
                clientState VertexArray $= Disabled
            Nothing -> return ()

runObjViewer :: StateT ViewerState IO ()
runObjViewer = do
  args <- liftIO $ getArgs
  case args of
    [] -> do
        liftIO $ print "usage: objviewer <filename>"
    (path:_) -> do
        loadObjFile path
        initGL
        mobjscene <- gets currentObjScene
        case mobjscene of
            Just _ -> createVBOs
            Nothing -> return ()
        createGridVBO
        viewerLoop

main :: IO ((),ViewerState)
main = do
    initState <- initStateIO
    runStateT runObjViewer initState
