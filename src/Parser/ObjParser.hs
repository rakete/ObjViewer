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
    , groups :: M.Map String ObjVertexGroup
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
    { num_vertices :: (Integer,Integer)
    , num_texcoords :: (Integer,Integer)
    , num_normals :: (Integer,Integer)
    }

initParserState :: ParserState
initParserState = ParserState
    { num_vertices = (0,0)
    , num_texcoords = (0,0)
    , num_normals = (0,0)
    }

parseVertices :: (VertexComponent c, Fractional c) => GenParser Char ParserState ([Vertex3 c], [Normal3 c], [TexCoord2 c])
parseVertices = do
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

    updateState (\p -> p{ num_vertices = (\(a,b) -> (fromIntegral $ length vertex_list, a+b)) $ num_vertices p} )
    updateState (\p -> p{ num_normals = (\(a,b) -> (fromIntegral $ length normal_list, a+b)) $ num_normals p} )
    updateState (\p -> p{ num_texcoords = (\(a,b) -> (fromIntegral $ length texcoord_list, a+b)) $ num_texcoords p} )

    return $ (vertex_list,normal_list,texcoord_list)

parseVertexGroup :: Integral i => GenParser Char ParserState (Maybe String, [[(i, Maybe i, Maybe i)]])
parseVertexGroup = do
    (ParserState (_,v_offset) (_,t_offset) (_,n_offset)) <- getState
    (material,groupname,smoothgroup) <- permute $
      (,,)
      <$?> (Nothing,
            do reserved "usemtl"
               mat <- identifier
               return $ Just mat)
      <|?> (Nothing,
           do reserved "g"
              groupname <- option "" identifier
              return $ Just groupname)
      <|?> (Nothing,
           do reserved "s"
              smoothgroup <- option "" identifier
              return $ Just smoothgroup)
    indices <- many1 $ do
        reserved "f"
        many3 $ try (do
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
                        return (v,n,t))
          <|> (do
                  v' <- natural
                  return (fromIntegral (v'-1-v_offset),Nothing,Nothing))
    return (material, indices)

assembleObjMeshData :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Num i, Integral i) => ([Vertex3 c], [Normal3 c], [TexCoord2 c]) -> [[(i, Maybe i, Maybe i)]] -> ([Vertex3 c], [Normal3 c], [TexCoord2 c])
assembleObjMeshData (vertices, sparse_normals, sparse_texcoords) polygons =
  let (meshvertices, maybe_meshnormals, maybe_meshtexcoords) = unzip3 $ do
          meshindex <- concat polygons
          let (vertex_index, maybe_normal_index, maybe_texcoord_index) = meshindex
          return (vertices !! fromIntegral (vertex_index - 1),
                  maybe Nothing (\i -> Just $ sparse_normals !! fromIntegral (i - 1)) maybe_normal_index,
                  maybe Nothing (\i -> Just $ sparse_texcoords !! fromIntegral (i - 1)) maybe_texcoord_index
                 )
      meshnormals = catMaybes maybe_meshnormals
      meshtexcoords = catMaybes maybe_meshtexcoords
  in (meshvertices, meshnormals, meshtexcoords)

assembleObjMeshGroups :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Num i, Integral i) => ([Vertex3 c], [Normal3 c], [TexCoord2 c]) -> [(Maybe String, [[(i, Maybe i, Maybe i)]])] -> (([Vertex3 c], [Normal3 c], [TexCoord2 c], Indices i), M.Map String ObjVertexGroup)
assembleObjMeshGroups sparsedata groupstuples =
  snd $ foldl (\(offset, ((accum_vertices, accum_normals, accum_texcoords, accum_indices), accum_groupmap)) (maybe_material, polygons) ->
                 let (vertices, normals, texcoords) = assembleObjMeshData sparsedata polygons
                     material_name = (maybe "" id maybe_material)
                     vertices_size = fromIntegral $ length vertices
                     result_meshdata = (accum_vertices ++ vertices, accum_normals ++ normals, accum_texcoords ++ texcoords, accum_indices ++ [offset .. offset+vertices_size])
                     result_groupmap = M.insert material_name (ObjVertexGroup maybe_material (fromIntegral offset) (fromIntegral vertices_size)) accum_groupmap
                 in (offset + vertices_size, (result_meshdata, result_groupmap)))
               (0, (([], [], [], []), M.empty)) groupstuples

parseObject :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Integral i) => String -> GenParser Char ParserState (ObjMesh c i)
parseObject fallbackname = do
    name <- option fallbackname $ do
                reserved "o"
                identifier >>= return
    sparsedata <- parseVertices
    groupstuples <- many1 parseVertexGroup
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
        <$$> (do -- a
            reserved "Ka"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Color4 cr cg cb 1.0)
        <||> (do -- b
            reserved "Kd"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Color4 cr cg cb 1.0)
        <||> (do -- c
            reserved "Ks"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Color4 cr cg cb 1.0)
        <|?> (Nothing,do -- d
            reserved "Tf"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Just $ Color4 cr cg cb 1.0)
        <|?> (Nothing,do -- e
            reserved "Ke"
            cr <- parseComponent
            cg <- parseComponent
            cb <- parseComponent
            return $ Just $ Color4 cr cg cb 1.0)
        <||> (do -- f
            reserved "Ns"
            x <- parseComponent
            return x)
        <||> (do -- g
            reserved "d"
            b <- option False $ do
                reserved "-halo"
                return True
            x <- parseComponent
            return (b,x))
        <||> (do -- h
            reserved "illum"
            x <- natural
            return $ fromInteger x)
        <|?> (Nothing,do -- i
            reserved "sharpness"
            x <- natural
            return $ Just $ fromInteger x)
        <|?> (Nothing,do -- j
            reserved "Ni"
            x <- parseComponent
            return $ Just x)
        <|?> (Nothing,do -- k
            reserved "map_Ka"
            x <- identifier
            return $ Just (x,Nothing))
        <|?> (Nothing,do -- l
            reserved "map_Kd"
            x <- identifier
            return $ Just (x,Nothing))
        <|?> (Nothing,do -- m
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
