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

-- a vertex group is what we can fit into a single drawcall, we use glDrawElements with with indexed
-- vbos to draw, so the offset is an offset into an element array buffer and the size is the number of
-- indices we want to render
--
-- in the obj file, each usemtl that specifies a material will result in a new vertex group like this,
-- so that we can set the appropriate parameters for the material before issuing the drawcall, the
-- implementation does not recognize when the same material is repeatedly used, it assumes each usemtl
-- directive uses a new material
data ObjVertexGroup = ObjVertexGroup
    { group_material :: Maybe String
    , group_offset :: Int
    , group_size :: Int
    }
    deriving Show

-- a mesh has a name, a list of vertices, optional normals and texcoords and a list of indices that
-- describe the faces of the mesh, all faces will be triangles in this implementation
--
-- the list of vertex groups associates ranges of the data with materials, using the offset and size
-- of the vertex groups we can render the data using a material with a glDrawElements call
data ObjMesh a b = ObjMesh
    { objmesh_name :: String
    , objmesh_data :: ([Vertex3 a], [Normal3 a], [TexCoord2 a], Indices b)
    , objmesh_groups :: [ObjVertexGroup]
    }
    deriving Show

-- the main data structure holding the whole scene, it contains a material library associating
-- material string names with actual material data structures, and a map of objects with their names
-- used as keys
data ObjScene a b = ObjScene
    { objscene_mtllib ::  M.Map String (ObjMaterial a)
    , objscene_objects :: M.Map b (ObjMesh a b)
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

-- a component a vertex, normal or texcoord, also used when parsing materials
-- this exists so that I can parse numbers that look like an int or a float and
-- may even have a plus or minus in front indicating its sign
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

-- sadly this code relies on the mutable parser state, in an obj file their can be "g", "s" and "usemtl"
-- statements more or less everywhere that are stateful, that means setting the group with "g", the smoothgroup
-- with "s" or the material with "usemtl" applies to all statements that come afterwards, so I use this state
-- to set the current groupname, smoothgroup or material whenever I parse those, then query the state when I
-- actually need them
data ParserState = ParserState
    { parserstate_groupname :: Maybe String,
      parserstate_smoothgroup :: Maybe String,
      parserstate_material :: Maybe String }

initParserState :: ParserState
initParserState = ParserState
    { parserstate_groupname = Nothing,
      parserstate_smoothgroup = Nothing,
      parserstate_material = Nothing }

-- both parseSmoothGroup and parseMaterialGroup look and act very similar for a reason which I explain in the
-- comment for parseSmoothGroup
--
-- parseMaterialGroup exists because I noticed a tendency of exporters to sprinkle "g", "s" and "usemtl" blocks
-- everywhere, even at places where I don't expected them
-- so parseMaterialGroup parses these blocks which may consist of any number of randomly ordered occurences of
-- these "g", "s" and "usemtl" keywords using a permuting parser
--
-- notice how parseMaterialGroup and parseSmoothGroup have no return type, these functions actually only have a
-- side effect, they modify the parser state if "g", "s" or "usemtl" were parsed, and only if they were parsed
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

-- the reason why parseSmoothGroup exists is because I needed a way to distinguish when parseMaterialGroup parses only
-- a smoothgroup consisting of "g" or "s", or when parseMaterialGroup parses a full material with "g","s" or "usemtl",
-- while still having each of the "g","s" or "usemtl" options remain optional in the permuting parser, so that I can
-- use the parser even when there is none of the keywords present to be parsed at all
--
-- parseSmoothGroup is used instead of parseMaterialGroup when I parse the indices of the mesh that make up the faces,
-- these are grouped by an initial material definition but then also may have multiple groupnames and smoothgroups,
-- making it neccessary that I parse only "g" or "s" keywords, but not "usemtl"
--
-- just as parseMaterialGroup this has no return type, it only modifies the parser state
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

-- parseVertices should be relativly simple to understand, it just parses the raw vertices, normals and texcoords of
-- the mesh with are represented by lines starting with either "v", "vn" or "vt" in the obj files
--
-- the v, vn or vt lines each come as a block, but the whole blocks may be ordered arbitrarily, so again I am using
-- a permuting parser
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

-- after parsing the vertices with parseVertices, whats left is parsing the indices that make up the faces, together with all smoothgroups
-- and materials that are associated with faces, thats what parseVertexGroups is for
--
-- the face indices come as lines starting with a "f" and then any number (>=3) of triplets like v/n/t, where v,n,t are indices into the
-- vertices, normal and texcoord arrays parsed above, and only v is mandatory, n and t are optional
--
-- this function is crucial to understanding how this programs data is setup and then rendered, the parseVertexGroup is used as argument
-- to a many1 parser to parse all batches of indices seperated by different materials that we can render with glDrawElements
--
-- notice how we first use parseMaterialGroup, then use parseSmoothGroup in the argument to many1Till, notice also how the second argument
-- to many1Till, the end parser, is a parser very similar to parseMaterialGroup but uses try $ lookAhead in front and "usemtl" is non optional
--
-- the end parser ensures that we terminate as soon as we match another material group, but can not use parseMaterialGroup as end parser
-- because that would also match smooth groups because its "usemtl" is optional
--
-- the parser that does all the work of parsing all the indices, that also contains parseSmoothGroup at its beginning is repeatedly parsing
-- the f v/n/t ... lines and the parseSmoothGroup ensures that all smooth groups are also parsed but we still put everything into the same
-- material batch because only when we parse a material group with a "usemtl" we terminate
--
-- maybe things become clearer when looking at the result type: (Maybe String, [(Maybe String, [(i, Maybe i, Maybe i)])])
-- - the first Maybe String is a material, this function only returns one material batch
-- - the second list are all smooth groups belonging to the material batch, each smooth group is identified by a Maybe String, there may
--   be several smooth groups with the same identifying Maybe String
-- - the last list [(i, Maybe i, Maybe i)] represent polygons assembled by the index triplets v/n/t we are mainly interested in
--
-- finally notice in that case statement that if a parsed polygon is larger then a triangle, it will get triangulated, we need the actual
-- vertices for that so thats why we pass them to this functions as first argument
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

-- the data parsed by parseVertices and parseVertexGroups is not suitable to be rendered with opengl yet, we need to first transform it
-- so that it fits opengls idea of how the data should look like
--
-- most importantly we have the following problem: in the obj file format a vertex can have multiple normals associated with it, but to
-- render in opengl using vbos every vertex can only have one normal associated with it
-- solving this problem is easy though, we just have to iterate over all indices, look up the corresponding vertex/normal/texcoord triplet
-- from the input arrays, and append those to the output arrays
-- with this method we get all vertices, normals and texcoord in the correct order, and they are guaranteed to be unique pairings
--
-- the above gives us the vertices in correct order, and to get fitting indices we can just enumerate integers from 0 to (length vertices),
-- and since this function is called repeatedly for different vertex groups which together make up the same mesh, that get uploaded altogether
-- into opengl buffers, we have an offset for the indices, so its [offset .. offset+(length vertices)] below
--
-- when you look at the code below, you see both the vertex, normal and texcoord array generation and the indices generation besides the
-- let bindings for (more_vertices, more_normals, more_texcoords) = unzip3... and more_indices = [offset...]
--
-- the approach described above has a downside: we are throwing away all the information in the indices, and inflate the number of vertices
-- by inserting a new unique vertex for every index encountered, this is slow for larger meshes so I tried to counter this downside by
-- using the original vertices, normals, texcoords and indices as output if either there are no normals or texcoords at all, or the indices
-- for vertices, normals and textures are all equal
assembleObjMeshData :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Integral i) => ([Vertex3 c], [Normal3 c], [TexCoord2 c]) -> [(Maybe String, [(i, Maybe i, Maybe i)])] -> i -> ([Vertex3 c], [Normal3 c], [TexCoord2 c], Indices i)
assembleObjMeshData (orig_vertices, orig_normals, orig_texcoords) polygons offset =
  let array_vertices = array (0,length orig_vertices) $ zip [0..] orig_vertices
      array_normals = array (0,length orig_normals) $ zip [0..] orig_normals
      array_texcoords = array (0,length orig_texcoords) $ zip [0..] orig_texcoords
      (indices_equal_list, inflated_vertices, maybe_meshnormals, maybe_meshtexcoords) = unzip4 $ do
          meshindex <- concatMap snd polygons
          let (vertex_index, maybe_normal_index, maybe_texcoord_index) = meshindex
          return ((isNothing maybe_normal_index) && (isNothing maybe_texcoord_index) ||
                  (isNothing maybe_texcoord_index) && (vertex_index == (fromJust maybe_normal_index)) ||
                  (isNothing maybe_normal_index) && (vertex_index == (fromJust maybe_texcoord_index)) ||
                  (vertex_index == (fromJust maybe_normal_index)) && (vertex_index == (fromJust maybe_texcoord_index)),
                  array_vertices ! fromIntegral (vertex_index - 1),
                  maybe Nothing (\i -> Just $ array_normals ! fromIntegral (i - 1)) maybe_normal_index,
                  maybe Nothing (\i -> Just $ array_texcoords ! fromIntegral (i - 1)) maybe_texcoord_index)
      unzipped_polygon_indices = unzip3 $ concatMap snd polygons
      orig_vertex_indices = map (\i -> i - 1) $ fst3 $ unzipped_polygon_indices
      inflated_indices = [offset .. offset+inflated_vertices_size-1]
      inflated_vertices_size = fromIntegral $ length inflated_vertices
      all_indices_equal = all id indices_equal_list
      use_orig = (null orig_normals && null orig_texcoords) || all_indices_equal
  in if use_orig
     then (orig_vertices, orig_normals, orig_texcoords, orig_vertex_indices)
     else (inflated_vertices, catMaybes maybe_meshnormals, catMaybes maybe_meshtexcoords, inflated_indices)

-- this just applies assembleObjMeshData to all the vertex groups and returns the resulting data as (meshdata, meshgroups) tuple which we
-- can then use to create a ObjMesh, its just a fold which accumulates lists and keeps track of an offset
assembleObjMeshGroups :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Num i, Integral i) => ([Vertex3 c], [Normal3 c], [TexCoord2 c]) -> [(Maybe String, [(Maybe String, [(i, Maybe i, Maybe i)])])] -> (([Vertex3 c], [Normal3 c], [TexCoord2 c], Indices i), [ObjVertexGroup])
assembleObjMeshGroups sparsedata groupstuples =
  snd $ foldl' (\(offset, ((accum_vertices, accum_normals, accum_texcoords, accum_indices), accum_groups)) (maybe_material, polygons) ->
                 let (vertices, normals, texcoords, indices) = assembleObjMeshData sparsedata polygons offset
                     indices_size = fromIntegral $ length indices
                     result_meshdata = (accum_vertices ++ vertices, accum_normals ++ normals, accum_texcoords ++ texcoords, accum_indices ++ indices)
                     result_groups = accum_groups ++ [(ObjVertexGroup maybe_material (fromIntegral offset) (fromIntegral indices_size))]
                 in (offset + indices_size, (result_meshdata, result_groups)))
               (0, (([], [], [], []), [])) groupstuples

-- compared to the stuff above, parseObject, parseObjScene and parseObjFile are uninteresting, these just call the parsers we defined above
-- and stick the results together
-- parseObject parses just one mesh, consisting of first a name, then the section with all the vertices and after that a section with all
-- the face indices, makes an ObjMesh out of the parsed meshdata and meshgroups
parseObject :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Integral i) => String -> GenParser Char ParserState (ObjMesh c i)
parseObject fallbackname = do
    name <- option fallbackname $ do
                reserved "o"
                identifier >>= return
    sparsedata <- parseVertices
    groupstuples <- many1 $ parseVertexGroup $ fst3 sparsedata
    let (meshdata, meshgroups) = assembleObjMeshGroups sparsedata groupstuples
    return $ ObjMesh name meshdata meshgroups

-- parseObjScene parse a scene that consists of an optional mtlfile keyword and then a number of parseObjects, but at least one
parseObjScene :: (VertexComponent c, Fractional c, RealFloat c, Enum c, Integral i) => String -> GenParser Char ParserState (Maybe FilePath,ObjScene c i)
parseObjScene fallbackname = do
    whiteSpace
    mtlfile <- option Nothing $ do
        reserved "mtllib"
        identifier >>= return . Just
    objects <- many1 $ parseObject fallbackname
    eof
    return $ (mtlfile, ObjScene M.empty (listIntMap objects))

-- parseObjFile parses a whole obj file, here the error reporting is done when the parse fails, and we also call the parser for the material lib here,
-- this function produces the final ObjScene that we use in the renderer
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
                           Right m -> return $ Right $ ObjScene m o
                     else return $ Right os
                otherwise -> return $ Right os

--
--

-- parseObjMaterial is just one big permute parser so that a material file can have all their keywords ordered completely arbitrary
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

-- parseObjMaterialLib and parseMtlFile just as their equivalents above parseObjScene and parseObjFile are both just
-- boring wrappers around existing functionality, parseObjMaterialLib parses many1 parseObjMaterial and is used by
-- parseMtlFile to parse a material file
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
  --notFollowedBy end
  first <- p
  rest <- manyTill p end
  return (first:rest)
