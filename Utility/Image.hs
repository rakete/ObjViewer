module Utility.Image
( mapImage
, mapImageBits
, foldImage
, foldImageBits
, bestMatchingColors
, tileImage
) where

import Graphics.Imlib
import qualified Data.Map as M
import qualified Data.Array as A
import Data.Ix
import Foreign
import Data.List
import Control.Monad.Trans

import Math.Metric

--instance Metric ImlibColor where
--    distance (ImlibColor _ r1 g1 b1) (ImlibColor _ r2 g2 b2) =
--        let r = fromIntegral r1 - fromIntegral r2
--            b = fromIntegral b1 - fromIntegral b2
--            g = fromIntegral g1 - fromIntegral g2
--        in sqrt(3*(r*r)+4*(g*g)+2*(b*b))

instance Metric ImlibColor where
    distance (ImlibColor _ r1 g1 b1) (ImlibColor _ r2 g2 b2) =
        let r' = (fromIntegral r1 + fromIntegral r2) / 2
            r = fromIntegral r1 - fromIntegral r2
            b = fromIntegral b1 - fromIntegral b2
            g = fromIntegral g1 - fromIntegral g2
        in sqrt((2+r'/256)*(r*r)+4*(g*g)+(2+(255-r')/256)*(b*b))

instance Ord ImlibColor where
    compare a@(ImlibColor a1 r1 g1 b1) b@(ImlibColor a2 r2 g2 b2) =
        let da = distance a (ImlibColor 255 255 255 255)
            db = distance b (ImlibColor 255 255 255 255)
        in compare da db

mapImage :: (Int -> Int -> ImlibColor -> a) -> IO [a]
mapImage f = do
    w <- imageGetWidth
    h <- imageGetHeight
    p <- imageGetDataForReadingOnly
    arr <- peekArray (w*h) p
    return $ [f x y c | ((x,y),c) <- zip [(x,y) | y <- [0..(h-1)], x <- [0..(w-1)]] $ map colorFromBits arr]

mapImageBits :: (Int -> Int -> Word32 -> a) -> IO [a]
mapImageBits f = do
    w <- imageGetWidth
    h <- imageGetHeight
    p <- imageGetDataForReadingOnly
    arr <- peekArray (w*h) p
    return $ [f x y b | ((x,y),b) <- zip [(x,y) | y <- [0..(h-1)], x <- [0..(w-1)]] arr]

foldImage :: (Int -> Int -> ImlibColor -> a -> a) -> a -> IO a
foldImage f a = do
    w <- imageGetWidth
    h <- imageGetHeight
    p <- imageGetDataForReadingOnly
    arr <- peekArray (w*h) p
    return $ foldl' (\a' ((x,y),c) -> f x y c a') a $ zip [(x,y) | y <- [0..(h-1)], x <- [0..(w-1)]] $ map colorFromBits arr

foldImageBits :: (Int -> Int -> Word32 -> a -> a) -> a -> IO a
foldImageBits f a = do
    w <- imageGetWidth
    h <- imageGetHeight
    p <- imageGetDataForReadingOnly
    arr <- peekArray (w*h) p
    return $ foldl' (\a' ((x,y),b) -> f x y b a') a $ zip [(x,y) | y <- [0..(h-1)], x <- [0..(w-1)]] arr

bestMatchingColors :: [ImlibColor] -> ImlibColor -> [ImlibColor]
bestMatchingColors (x:xs) c =
    if null (x:xs)
      then [ImlibColor 255 0 0 0,ImlibColor 255 255 255 255]
      else recur xs c (distance x c) [x,ImlibColor 255 255 255 255]

    where

    recur [] _ _ xs' = xs'
    recur (y:ys) c min xs' =
        let s = distance y c
        in if s < min
            then recur ys c s (y:xs')
            else recur ys c min xs'

data Neighbour = N | NE | E | SE | S | SW | W | NW
    deriving (Eq,Ord,Ix,Show)

--neighbours :: (a,a) -> (a,a) -> (a,a) -> a -> (M.Map Neighbour ((a,a),a)),[Neighbour])
--neighbours (x,y) (w,h) (tw,th) o =

tileImage :: ImlibImage -> (Int,Int) -> Int -> String -> IO [ImlibImage]
tileImage image (tile_w,tile_h) overlap path = do
    contextSetImage image
    name <- imageGetFilename >>= return . takeWhile (not.(=='.'))
    let path' = reverse $ dropWhile (=='/') $ reverse path

    w <- imageGetWidth
    h <- imageGetHeight

    m <- if overlap == 0
          then do
            let numtiles_w = w `div` tile_w
            let numtiles_h = h `div` tile_h

            foldImageBits (\x y b m ->
                 let k = (x `div` tile_w) + (numtiles_w * (y `div` tile_h))
                     m' = M.insertWith (++) k [b] m
                 in if x >= (numtiles_w * tile_w) || y >= (numtiles_h * tile_h)
                     then m
                     else m') M.empty
          else do
            let numtiles_w = w `div` tile_w
            let numtiles_h = h `div` tile_h
            let max_x = if numtiles_w <= 1
                         then numtiles_w*tile_w-1
                         else if even numtiles_w
                               then (numtiles_w-2)*(tile_w-overlap)-(snd $ numtiles_w `divMod` 2)-1+(2*tile_w)-1
                               else (numtiles_w-2)*(tile_w-overlap)-(snd $ numtiles_w `divMod` 2)-1+(2*tile_w)
            let max_y = if numtiles_h <= 1
                         then numtiles_h*tile_h-1
                         else if even numtiles_h
                               then (numtiles_h-2)*(tile_h-overlap)-(snd $ numtiles_h `divMod` 2)-1+(2*tile_h)-1
                               else (numtiles_h-2)*(tile_h-overlap)-(snd $ numtiles_h `divMod` 2)-1+(2*tile_h)

            print numtiles_w
            print numtiles_h
            print max_x
            print max_y

            foldImageBits (\x y b m ->
                let tws = takeWhile (<=x) [0,(tile_w-overlap)..]
                    ths = takeWhile (<=y) [0,(tile_h-overlap)..]
                    k = (length tws - 1) + (numtiles_w * (length ths - 1))
                    kw = k - 1
                    kh = k - numtiles_w
                    -- the following mess is responsible for creating the approriate neighboring
                    -- indices for the fold at the end of the let clause (an image is like a grid, think
                    -- of that and you'll get the idea what i am doing here)
                    -- i already thought of an replacement, which can been seen above,
                    -- the 'neighbours' function (which is commented out)
                    ks = case (last tws == x, last ths == y) of
                            (True,True) ->
                                case (x == 0, y == 0) of
                                    (True,True) -> [k]
                                    (True,False) ->
                                        case (x == max_x, y == max_y) of
                                            (False,False) -> [kh,k]
                                            otherwise -> [kh]
                                    (False,True) ->
                                        case (x == max_x, y == max_y) of
                                            (False,False) -> [kw,k]
                                            otherwise -> [kw]
                                    (False,False) ->
                                        case (x == max_x, y == max_y) of
                                            (False,False) -> [k,kw,kh,kh-1]
                                            (True,False) -> [kh-1,kw]
                                            (False,True) -> [kh-1,kh]
                                            (True,True) -> [kh-1]
                            (False,True) ->
                                case (x == 0, y == 0) of
                                    (True,True) -> [k]
                                    (True,False) -> [kh,k]
                                    (False,True) -> [k]
                                    (False,False) ->
                                        case (x == max_x, y == max_y) of
                                            (False,False) -> [kh,k]
                                            (False,True) -> [kh]
                            (True,False) ->
                                case (x == 0, y == 0) of
                                    (True,True) -> [k]
                                    (True,False) -> [k]
                                    (False,True) -> [kw,k]
                                    (False,False) ->
                                        case (x == max_x, y == max_y) of
                                            (False,False) -> [kw,k]
                                            (True,False) -> [kw]
                            (False,False) -> [k]
                    -- this fold creates... whatever, i forgot, but it looks important
                    m' = foldl' (\m'' k -> M.insertWith (++) k [b] m'') m ks
                in if x > max_x || y > max_y
                    then m
                    else m') M.empty --[(x,y) | y <- [0..(h-1)], x <- [0..(w-1)]]

    sequence $ M.elems $ M.mapWithKey (\k xs -> withArray (reverse xs) (\p -> do
        tile <- createImageUsingData tile_w tile_h p
        contextSetImage tile
        imageSetFormat "png"
        saveImage $ path' ++ "/" ++ name ++ (show k) ++ ".png"
        tile' <- loadImage $ path' ++ "/" ++ name ++ (show k) ++ ".png"
        return tile')) m

--testfoo = do
--    image <- loadImage "mars_heightmap.png"
--    tileImage image (64,64) 1 "./tiles/"

