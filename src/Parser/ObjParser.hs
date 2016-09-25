module Parser.ObjParser
( ObjMaterial
, ObjVertexGroup(..)
, ObjMesh(..)
, ObjScene(..)
, parseObjFile
) where

import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.Texturing
import Graphics.Rendering.OpenGL.GL.Tensor

import Text.Parsec
import Text.Parsec.Perm
import qualified Text.Parsec.Token as P
--import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String

import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M
import System.Directory

import Data.List
import Data.Array

import Math.Vector
import Engine.Texture
import Engine.Geometry

import Utility.Map
import Utility.List
import Utility.Tuple

type ObjMaterial a = Material a

data ObjVertexGroup = ObjVertexGroup
    { group_material :: Maybe String
    , group_offset :: Int
    , group_size :: Int
    }
    deriving Show

data ObjMesh a b = ObjMesh
    { objmesh_name :: String
    , objmesh_data :: ([Vertex3 a], [Normal3 a], [TexCoord2 a], Indices b)
    , objmesh_groups :: M.Map (Maybe String, b) ObjVertexGroup
    }
    deriving Show

data ObjScene a b = ObjScene
    { mtllib :: Maybe (M.Map String (ObjMaterial a))
    , objects :: M.Map b (ObjMesh a b)
    }

-- this definition is used by Parsec to create a custom lexer for our language
objStyle :: P.LanguageDef st
objStyle = P.LanguageDef
    { P.commentStart   = ""
    , P.commentEnd     = ""
    , P.commentLine    = "#"
    , P.nestedComments = True
    , P.identStart     = alphaNum <|> oneOf "_/"
    , P.identLetter    = alphaNum <|> oneOf "_./-"
    , P.opStart        = P.opLetter objStyle
    , P.opLetter       = parserZero
    , P.reservedOpNames= []
    , P.reservedNames  = ["mtllib","o","v","vp","vt","vn","g","s","usemtl","f"
                       ,"newmtl","Ka","Kd","Ks","Ke","Ns","Ni","Tf","d","-halo","illum","sharpness","Ni"
                       ,"map_Ka","map_Kd","map_Ks"]
    , P.caseSensitive  = True
    }

-- create the lexer
lexer = P.makeTokenParser objStyle

-- these are for simplicity, we use our custom lexer to create parsers
-- for different types of tokens
naturalOrFloat = P.naturalOrFloat lexer
natural = P.natural lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer

parseComponent :: (Fractional c) => GenParser Char st c
parseComponent = do
    sign <- option 1 $ do
        s <- oneOf "+-"
        return $ case s of
            '+' -> (1.0)
            '-' -> (-1.0)
    x <- naturalOrFloat
    return $ case x of
        Left y -> fromRational((fromInteger y) * sign)
        Right y -> fromRational((fromRational . toRational $ y) * sign)

data ParserState = ParserState
    { parserstate_groupname :: Maybe String,
      parserstate_smoothgroup :: Maybe String,
      parserstate_material :: Maybe String }

initParserState :: ParserState
initParserState = ParserState
    { parserstate_groupname = Nothing,
      parserstate_smoothgroup = Nothing,
      parserstate_material = Nothing }

parseSmoothGroup :: GenParser Char ParserState ()
parseSmoothGroup = do
  (maybe_groupname, maybe_smoothgroup) <- permute $
      (,)
      <$?> (Nothing,
           do reserved "g"
              groupname <- option "off" identifier
              return $ Just groupname)
      <|?> (Nothing,
           do reserved "s"
              smoothgroup <- option "off" identifier
              return $ Just smoothgroup)

  if isJust maybe_groupname
    then modifyState (\p -> p{ parserstate_groupname = if maybe_groupname == (Just "off") then Nothing else maybe_groupname })
    else return ()
  if isJust maybe_smoothgroup
    then modifyState (\p -> p{ parserstate_smoothgroup = if maybe_smoothgroup == (Just "off") then Nothing else maybe_smoothgroup })
    else return ()

parseMaterialGroup :: GenParser Char ParserState ()
parseMaterialGroup = do
  (maybe_material, maybe_groupname, maybe_smoothgroup) <- permute $
      (,,)
      <$?> (Nothing,
            do reserved "usemtl"
               mat <- identifier
               return $ Just mat)
      <|?> (Nothing,
           do reserved "g"
              groupname <- option "off" identifier
              return $ Just groupname)
      <|?> (Nothing,
           do reserved "s"
              smoothgroup <- option "off" identifier
              return $ Just smoothgroup)

  if isJust maybe_material
    then modifyState (\p -> p{ parserstate_material = maybe_material })
    else return ()
  if isJust maybe_groupname
    then modifyState (\p -> p{ parserstate_groupname = if maybe_groupname == (Just "off") then Nothing else maybe_groupname })
    else return ()
  if isJust maybe_smoothgroup
    then modifyState (\p -> p{ parserstate_smoothgroup = if maybe_smoothgroup == (Just "off") then Nothing else maybe_smoothgroup })
    else return ()

parseVertices :: (VertexComponent c, Fractional c) => GenParser Char ParserState ([Vertex3 c], [Normal3 c], [TexCoord2 c])
parseVertices = do
    parseMaterialGroup
    (vertex_list,normal_list,texcoord_list) <- permute $
        (,,)
        <$$> (many1 $ do
            reserved "v"
            x <- parseComponent
            y <- parseComponent
            z <- parseComponent
            return $ Vertex3 x y z)
        <|?> ([],many1 $ do
            reserved "vn"
            nx <- parseComponent
            ny <- parseComponent
            nz <- parseComponent
            return $ Normal3 nx ny nz)
        <|?> ([],many1 $ do
            reserved "vt"
            u <- parseComponent
            v <- parseComponent
            return $ TexCoord2 u v)

    return $ (vertex_list, normal_list, texcoord_list)

parseVertexGroup :: (Enum c, RealFloat c, VertexComponent c, Integral i) => [Vertex3 c] -> GenParser Char ParserState (Maybe String, [(Maybe String, [(i, Maybe i, Maybe i)])])
parseVertexGroup vertices = do
    parseMaterialGroup
    maybe_material <- getState >>= return . parserstate_material

    vertexgroup <- many1Till (do
        parseSmoothGroup
        maybe_smoothgroup <- getState >>= return . parserstate_smoothgroup
        reserved "f"
        face <- many3 $ try (do
                                v <- do
                                  v' <- natural
                                  return $ fromIntegral v' --(v'-1-v_offset)
                                symbol "/"
                                t <- option Nothing $ do
                                  t' <- natural
                                  return $ Just $ fromIntegral t' --(t'-1-t_offset)
                                symbol "/"
                                n <- option Nothing $ do
                                  n' <- natural
                                  return $ Just $ fromIntegral n' --(n'-1-n_offset)
                                return (v, n, t))
                <|> (do
                        v' <- natural
                        return (fromIntegral v',Nothing,Nothing))
        case face of
          (a:b:c:[]) -> return (maybe_smoothgroup, [a,b,c])
          _ -> let array_vertices = array (0,length vertices) $ zip [0..] vertices
                   polygon_indices = fst3 $ unzip3 face
                   polygon_vertices = M.fromList $ [(i,v) | i <- polygon_indices, v <- map (\i -> array_vertices ! (fromIntegral i)) polygon_indices]
                   polygon = M.fromList $ zip polygon_indices face
                   (_, triangle_indices) = triangulatePolygon polygon_vertices polygon_indices
                   triangles = map (\i -> fromJust $ M.lookup i polygon) triangle_indices
               in return (maybe_smoothgroup, triangles))
        (try $ lookAhead $ permute $ (,,)
          <$$> ((reserved "usemtl" >> identifier) <|> (eof >> return ""))
          <|?> (Nothing, reserved "g" >> option "off" identifier >>= return . Just)
          <|?> (Nothing, reserved "s" >> option "off" identifier >>= return . Just))
    return (maybe_material, vertexgroup)

assembleObjMeshData :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Num i, Integral i) => ([Vertex3 c], [Normal3 c], [TexCoord2 c]) -> [(Maybe String, [(i, Maybe i, Maybe i)])] -> i -> ([Vertex3 c], [Normal3 c], [TexCoord2 c], Indices i)
assembleObjMeshData (orig_vertices, orig_normals, orig_texcoords) polygons offset =
  let array_vertices = array (0,length orig_vertices) $ zip [0..] orig_vertices
      array_normals = array (0,length orig_normals) $ zip [0..] orig_normals
      array_texcoords = array (0,length orig_texcoords) $ zip [0..] orig_texcoords
      (more_vertices, maybe_meshnormals, maybe_meshtexcoords) = unzip3 $ do
          meshindex <- concatMap snd polygons
          let (vertex_index, maybe_normal_index, maybe_texcoord_index) = meshindex
          return (array_vertices ! fromIntegral (vertex_index - 1),
                  maybe Nothing (\i -> Just $ array_normals ! fromIntegral (i - 1)) maybe_normal_index,
                  maybe Nothing (\i -> Just $ array_texcoords ! fromIntegral (i - 1)) maybe_texcoord_index)
      unzipped_polygon_indices = unzip3 $ concatMap snd polygons
      orig_vertex_indices = map (\i -> i - 1) $ fst3 $ unzipped_polygon_indices
      orig_normal_indices = map (\i -> i - 1) $ catMaybes $ snd3 $ unzipped_polygon_indices
      orig_texcoord_indices = map (\i -> i - 1) $ catMaybes $ thd3 $ unzipped_polygon_indices
      more_vertices_size = fromIntegral $ length more_vertices
      vertex_eq_normal = orig_vertex_indices == orig_normal_indices
      vertex_eq_texcoord = orig_vertex_indices == orig_texcoord_indices
      use_orig = (null orig_normals && null orig_texcoords) ||
                 (vertex_eq_normal && null orig_texcoords) ||
                 (vertex_eq_texcoord && null orig_normals) ||
                 (vertex_eq_normal && vertex_eq_texcoord)
  in if use_orig
     then (orig_vertices, orig_normals, orig_texcoords, orig_vertex_indices)
     else (more_vertices, catMaybes maybe_meshnormals, catMaybes maybe_meshtexcoords, [offset .. offset+more_vertices_size-1])

assembleObjMeshGroups :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Num i, Integral i) => ([Vertex3 c], [Normal3 c], [TexCoord2 c]) -> [(Maybe String, [(Maybe String, [(i, Maybe i, Maybe i)])])] -> (([Vertex3 c], [Normal3 c], [TexCoord2 c], Indices i), M.Map (Maybe String, i) ObjVertexGroup)
assembleObjMeshGroups sparsedata groupstuples =
  snd $ foldl (\(offset, ((accum_vertices, accum_normals, accum_texcoords, accum_indices), accum_groupmap)) (maybe_material, polygons) ->
                 let (vertices, normals, texcoords, indices) = assembleObjMeshData sparsedata polygons offset
                     indices_size = fromIntegral $ length indices
                     result_meshdata = (accum_vertices ++ vertices, accum_normals ++ normals, accum_texcoords ++ texcoords, accum_indices ++ indices)
                     result_groupmap = M.insert (maybe_material, offset) (ObjVertexGroup maybe_material (fromIntegral offset) (fromIntegral indices_size)) accum_groupmap
                 in (offset + indices_size, (result_meshdata, result_groupmap)))
               (0, (([], [], [], []), M.empty)) groupstuples

parseObject :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Integral i) => String -> GenParser Char ParserState (ObjMesh c i)
parseObject fallbackname = do
    name <- option fallbackname $ do
                reserved "o"
                identifier >>= return
    sparsedata <- parseVertices
    groupstuples <- many1 $ parseVertexGroup $ fst3 sparsedata
    let (meshdata, meshgroups) = assembleObjMeshGroups sparsedata groupstuples
    return $ ObjMesh name meshdata meshgroups

parseObjScene :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Integral i) => String -> GenParser Char ParserState (Maybe FilePath,ObjScene c i)
parseObjScene fallbackname = do
    whiteSpace
    mtlfile <- option Nothing $ do
        reserved "mtllib"
        identifier >>= return . Just
    objects <- many1 $ parseObject fallbackname
    eof
    return $ (mtlfile, ObjScene Nothing (listIntMap objects))

parseObjFile :: (ColorComponent c,VertexComponent c, Fractional c, RealFloat c, Enum c, Integral i) => FilePath -> IO (Either ParseError (ObjScene c i))
parseObjFile objpath = do
    objinput <- readFile objpath
    let fallbackname = takeWhileEscaped (/='.') (=='\\') $ reverse $ takeWhile (/='/') $ reverse objpath
        objparse = (runParser (parseObjScene fallbackname) initParserState objpath objinput)
    case objparse of
        Left err -> return $ Left err
        Right (maybe_mtlpath, os@(ObjScene _ o)) -> do
            case maybe_mtlpath of
                Just mtlpath -> do
                    b <- doesFileExist mtlpath
                    if b
                     then do
                        mtlinput <- readFile mtlpath
                        let mtlparse = (runParser parseObjMaterialLib initParserState mtlpath mtlinput)
                        case mtlparse of
                           Left err -> return $ Left err
                           Right m -> return $ Right $ ObjScene (Just m) o
                     else return $ Right os
                otherwise -> return $ Right os

--
--

parseObjMaterial :: (ColorComponent c, Fractional c) => GenParser Char ParserState (ObjMaterial c)
parseObjMaterial = do
    reserved "newmtl"
    name <- identifier
    permute $
        (createMaterial name)
        <$?> (material_ambient defaultMaterial, do -- a
            reserved "Ka"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Color4 cr cg cb 1.0)
        <|?> (material_diffuse defaultMaterial, do -- b
            reserved "Kd"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Color4 cr cg cb 1.0)
        <|?> (material_specular defaultMaterial, do -- c
            reserved "Ks"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Color4 cr cg cb 1.0)
        <|?> (material_filter defaultMaterial, do -- d
            reserved "Tf"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Color4 cr cg cb 1.0)
        <|?> (material_emission defaultMaterial, do -- e
            reserved "Ke"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Color4 cr cg cb 1.0)
        <|?> (material_exponent defaultMaterial, do -- f
            reserved "Ns"
            x <- parseComponent
            return x)
        <|?> (material_dissolve defaultMaterial, do -- g
            reserved "d"
            b <- option False $ do
                reserved "-halo"
                return True
            x <- parseComponent
            return (b,x))
        <|?> (material_illum defaultMaterial, do -- h
            reserved "illum"
            x <- natural
            return $ fromInteger x)
        <|?> (material_sharpness defaultMaterial, do -- i
            reserved "sharpness"
            x <- natural
            return $ fromInteger x)
        <|?> (material_refraction defaultMaterial, do -- j
            reserved "Ni"
            x <- parseComponent
            return x)
        <|?> (Nothing, do -- k
            reserved "map_Ka"
            x <- identifier
            return $ Just (x,Nothing))
        <|?> (Nothing, do -- l
            reserved "map_Kd"
            x <- identifier
            return $ Just (x,Nothing))
        <|?> (Nothing, do -- m
            reserved "map_Ks"
            x <- identifier
            return $ Just (x,Nothing))

    where

    createMaterial name a b c d e f g h i j k l m = Material
        { material_name = name
        , material_ambient = a
        , material_diffuse = b
        , material_specular = c
        , material_filter = d
        , material_emission = e
        , material_exponent = f
        , material_dissolve = g
        , material_illum = h
        , material_sharpness = i
        , material_refraction = j
        , material_ambientTexture = k
        , material_diffuseTexture = l
        , material_specularTexture = m
        }

parseObjMaterialLib :: (ColorComponent c, Fractional c) => GenParser Char ParserState (M.Map String (ObjMaterial c))
parseObjMaterialLib = do
    whiteSpace
    material_list <- many1 parseObjMaterial
    eof
    return $ listStringMap material_name material_list

parseMtlFile :: (ColorComponent c, Fractional c) => FilePath -> IO (Either ParseError (M.Map String (ObjMaterial c)))
parseMtlFile path = do
    input <- readFile path
    return (runParser parseObjMaterialLib initParserState path input)

--
--

many3 :: GenParser tok st a -> GenParser tok st [a]
many3 p = do{ x <- p; y <- p; z <- p; xs <- many p; return (x:y:z:xs) }

many1Till :: (Stream s m t, Show end) =>
              ParsecT s u m a ->
              ParsecT s u m end ->
              ParsecT s u m [a]
many1Till p end = do
  notFollowedBy end
  first <- p
  rest <- manyTill p end
  return (first:rest)
