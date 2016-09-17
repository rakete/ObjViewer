{-# OPTIONS_GHC -fglasgow-exts #-}
module Engine.Texture
( Material(..)
, defaultMaterial
, uploadTexture
, uploadTextureFromFile
, uploadMipmappedTexture
, uploadMipmappedTextureFromFile
, GlyphTexture(..)
, createGlyphTexture
, renderText
) where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GLU.Mipmapping
import Graphics.Imlib
import Foreign
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Data.List

import Math.Vector
import Math.Matrix
import Math.Quaternion
import Engine.Factory

import Utility.Map
import Utility.List


data Material a = Material
    { material_name :: String
    , material_ambient :: Color4 a
    , material_diffuse :: Color4 a
    , material_specular :: Color4 a
    , material_filter :: Maybe (Color4 a)
    , material_emission :: Maybe (Color4 a)
    , material_exponent :: Float
    , material_dissolve :: (Bool,Float)
    , material_illum :: Int
    , material_sharpness :: Maybe Int
    , material_refraction :: Maybe Float
    , material_ambientTexture :: Maybe (String,Maybe TextureObject)
    , material_diffuseTexture :: Maybe (String,Maybe TextureObject)
    , material_specularTexture :: Maybe (String,Maybe TextureObject)
    }
    deriving Show

defaultMaterial :: Material GLfloat
defaultMaterial = Material
    { material_name = "default"
    , material_ambient = Color4 1.0 1.0 1.0 1.0
    , material_diffuse = Color4 1.0 1.0 1.0 1.0
    , material_specular = Color4 1.0 1.0 1.0 1.0
    , material_filter = Nothing
    , material_emission = Nothing
    , material_exponent = 1.0
    , material_dissolve = (False,1.0)
    , material_illum = 0
    , material_sharpness = Nothing
    , material_refraction = Nothing
    , material_ambientTexture = Nothing
    , material_diffuseTexture = Nothing
    , material_specularTexture = Nothing
    }

instance Factory (M.Map String (Material GLfloat)) (M.Map String (Material GLfloat)) where
    construct mtllib = sequenceMap $ M.map uploadMaterial mtllib where
        uploadMaterial m = do
            let atex = material_ambientTexture m
            let dtex = material_diffuseTexture m
            let stex = material_specularTexture m
            (atex':dtex':stex':[]) <- sequence $ map (\mtex ->
                                        if isJust mtex
                                         then let (path,mbuf) = fromJust mtex
                                              in if isNothing mbuf
                                                  then do
                                                    tbuf <- uploadTextureFromFile path
                                                    return $ Just (path,Just tbuf)
                                                  else return mtex
                                         else return mtex
                                     ) [atex,dtex,stex]
            let m'' = (\m' -> m'{ material_ambientTexture = atex', material_diffuseTexture = dtex', material_specularTexture = stex' }) m
            return m''

uploadTexture :: ImlibImage -> IO TextureObject
uploadTexture image = do
    contextSetImage image
    w <- liftM fromIntegral imageGetWidth
    h <- liftM fromIntegral imageGetHeight
    iptr <- liftM castPtr imageGetDataForReadingOnly
    let pdata = PixelData BGRA UnsignedByte (iptr :: Ptr Word8)
    [texid] <- genObjectNames 1

    textureBinding Texture2D $= Just texid
    textureWrapMode Texture2D S $= (Repeated,Repeat)
    textureWrapMode Texture2D T $= (Repeated,Repeat)
    textureFilter Texture2D $= ((Linear',Nothing),Linear')
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D w h) 0 pdata

    return texid

uploadTextureFromFile :: String -> IO TextureObject
uploadTextureFromFile path = do
    loadImage path >>= uploadTexture

uploadMipmappedTexture :: ImlibImage -> IO TextureObject
uploadMipmappedTexture image = do
    contextSetImage image
    w <- liftM fromIntegral imageGetWidth
    h <- liftM fromIntegral imageGetHeight
    iptr <- liftM castPtr imageGetDataForReadingOnly
    let pdata = PixelData BGRA UnsignedByte (iptr :: Ptr Word8)
    [texid] <- genObjectNames 1

    textureBinding Texture2D $= Just texid
    textureWrapMode Texture2D S $= (Repeated,Repeat)
    textureWrapMode Texture2D T $= (Repeated,Repeat)
    textureFilter Texture2D $= ((Linear',Just Linear'),Linear')
    build2DMipmaps Texture2D RGBA' w h pdata

    return texid

uploadMipmappedTextureFromFile :: String -> IO TextureObject
uploadMipmappedTextureFromFile path = do
    loadImage path >>= uploadMipmappedTexture

--
--

data GlyphTexture =
    GlyphTexture
        { glyphTextureId :: TextureObject
        , textureDimensions :: (Int,Int)
        , rowsAndLines :: (Int,Int)
        , glyphString :: String
        , glyphDimensions :: (Int,Int)
        , glyphSpacing :: (Int,Int)
        , glyphAdvance :: M.Map Char (Int,Int)
        , glyphTexCoords :: M.Map Char (GLfloat,GLfloat,GLfloat,GLfloat)
        }

createGlyphTexture :: String -> String -> String -> IO (Maybe GlyphTexture)
createGlyphTexture fontpaths fontstring text = do
    sequence $ map addPathToFontPath $ split ':' fontpaths

    let fontname = takeWhile (not . (=='/')) fontstring

    fontlist <- listFonts
    if elem fontname fontlist
     then do
        font <- loadFont fontstring
        contextSetFont font

        dims <- sequence $ map (\c -> do { (x,y) <- getTextSize [c]; i <- getTextInset [c]; a <- getTextAdvance [c]; return (x,y,i,(c,a))}) text

        let (xs,ys,is,as) = unzip4 dims

        let advancemap = M.fromList as

        let (glyph_w,glyph_h) = (maximum xs, maximum ys)

        let numrows = foldl1 (\nr' nr ->
                        let x1 = nr*glyph_w
                            y1 = let (l,i) = (length text `divMod` nr)
                                 in (l + (if i > 0 then 1 else 0))*glyph_h
                            a = abs(x1 - y1)
                            x2 = nr'*glyph_w
                            y2 = let (l,i) = (length text `divMod` nr')
                                 in (l + (if i > 0 then 1 else 0))*glyph_h
                            b = abs(x2 - y2)
                        in if a < b
                            then nr
                            else nr') $ [1..(length text)]

        let numlines = let (l,i) = (length text `divMod` numrows)
                       in l + (if i > 0 then 1 else 0)

        let (texture_w,texture_h) = (numrows * glyph_w, numlines * glyph_h)

        let (space_w,space_h) = case (texture_w < texture_h, texture_h < texture_w) of
                                    (True,False) ->
                                        let sw = (texture_h-texture_w) `div` numrows
                                            sh = if numrows * (glyph_w+sw) == texture_h
                                                  then 0
                                                  else if (numrows * (glyph_w+sw)) > texture_h
                                                        then (numrows*(glyph_w+sw)-texture_h) `div` numlines
                                                        else 0
                                        in (sw,sh)
                                    (False,True) ->
                                        let sh = (texture_w-texture_h) `div` numlines
                                            sw = if numlines * (glyph_h+sh) == texture_w
                                                  then 0
                                                  else if (numlines * (glyph_h+sh)) > texture_w
                                                        then (numlines*(glyph_h+sh)-texture_w) `div` numrows
                                                        else 0
                                        in (sw,sh)
                                    otherwise -> (0,0)

        let (texture_w',texture_h') = (numrows * (glyph_w+space_w), numlines * (glyph_h+space_h))

        let texcoordmap = let w = fromIntegral texture_w' -- texture size
                              h = fromIntegral texture_h'
                              pw = 1.0/w -- pixelsize
                              ph = 1.0/h
                              gw = fromIntegral glyph_w -- glyphsize
                              gh = fromIntegral glyph_h
                              aw = fromIntegral $ glyph_w+space_w --advance per row/line
                              ah = fromIntegral $ glyph_h+space_h
                          in M.fromList [(c,( pw*x, ph*y, pw*x+pw*gw, ph*y+ph*gh) ) | ((x,y),c) <- zip [(x,y) | y <- [0.0,ah..h], x <- [0.0,aw..(w-aw)]] text]

        image <- createImage texture_w' texture_h'
        contextSetImage image
        imageSetHasAlpha True
        imageSetFormat "png"
        imageSetBorder $ ImlibBorder 0 0 0 0
        sequence $ [textDraw x y [c] | ((x,y),c) <- zip [(x,y) | y <- [0,(glyph_h+space_h)..texture_h'], x <- [0,(glyph_w+space_w)..(texture_w'-(glyph_w+space_w))]] text]
        texid <- uploadMipmappedTexture image
        return $ Just $ GlyphTexture texid (texture_w',texture_h') (numrows,numlines) text (glyph_w,glyph_h) (space_w,space_h) advancemap texcoordmap
     else do
        return Nothing

renderText :: (Vector v GLfloat) => v -> v -> v -> GLfloat -> GlyphTexture -> String -> IO ()
renderText pos_v right_v up_v size glyphtex text = preservingMatrix $ do
    textureBinding Texture2D $= Just (glyphTextureId glyphtex)
    let angle1 = (Vector3 0.0 1.0 0.0) `angleBetween` (fromVector up_v)
    let angle2 = (Vector3 1.0 0.0 0.0) `angleBetween` (fromVector right_v)
    let q1 = rotationQuat angle1 (Vector3 1.0 0.0 0.0)
    let q2 = rotationQuat angle2 (Vector3 0.0 1.0 0.0)
    translate $ fromVector pos_v
    rotateQuatGL $ q1 `quatProduct` q2
    foldl (renderChar size glyphtex) (return (0,0,0.0,0.0,' ')) text
    return ()

    where

    renderChar glyphsize_w g io c = do
        (r,l,advance_x',advance_y',c') <- io

        let glyphpixels_w = fst $ glyphDimensions g
        let glyphpixels_h = snd $ glyphDimensions g
        let pixel_size = glyphsize_w / fromIntegral glyphpixels_w
        let glyphsize_h = pixel_size * fromIntegral glyphpixels_h
        let advance_x = advance_x' + pixel_size * (fromIntegral $ fst $ fromMaybe (glyphpixels_w,glyphpixels_h) $ M.lookup c' $ glyphAdvance g)
        let advance_y = advance_y' + pixel_size * (fromIntegral $ snd $ fromMaybe (glyphpixels_w,glyphpixels_h) $ M.lookup c' $ glyphAdvance g)

        case M.lookup c $ glyphTexCoords g of
            Just (u1,v1,u2,v2) -> do

                --let x1 = (pos_x+(r*advance_x))
                --let y1 = (pos_y-(l*advance_y))
                --let x2 = (pos_x+(r*advance_x))+glyphsize_w
                --let y2 = (pos_y-(l*advance_y))-glyphsize_h

                let x1 = advance_x
                let y1 = 0.0-advance_y
                let x2 = advance_x+glyphsize_w
                let y2 = 0.0-advance_y+glyphsize_h

                renderPrimitive Polygon $ do
                    texCoord ((TexCoord2 u1 v1 ) :: TexCoord2 GLfloat)
                    vertex ((Vertex3 x1 y2 0.0) :: Vertex3 GLfloat)
                    texCoord ((TexCoord2 u2 v1 ) :: TexCoord2 GLfloat)
                    vertex ((Vertex3 x2 y2 0.0) :: Vertex3 GLfloat)
                    texCoord ((TexCoord2 u2 v2 ) :: TexCoord2 GLfloat)
                    vertex ((Vertex3 x2 y1 0.0) :: Vertex3 GLfloat)
                    texCoord ((TexCoord2 u1 v2 ) :: TexCoord2 GLfloat)
                    vertex ((Vertex3 x1 y1 0.0) :: Vertex3 GLfloat)

                return (r+1,l,advance_x,advance_y',c)
            Nothing -> do
                case c of
                    '\n' ->
                        return (0,l+1,0.0,advance_y,' ')
                    '\t' ->
                        return (r+4,l,advance_x+3*glyphsize_w,advance_y',' ')
                    otherwise ->
                        return (r+1,l,advance_x,advance_y',' ')
