{-# OPTIONS_GHC -fglasgow-exts #-}
module Math.Matrix
( Matrix(..)
) where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.Maybe

import Math.Vector

-- the defaults deal with quadratic, row-major matrices _only_
-- don't try to use them with non-quadratic ones!
class (Fractional c, Show c, Eq c) => Matrix m c | m -> c where
    newMatrix :: [c] -> m
    fromMatrix :: (Matrix m1 c) => m -> m1
    matToList :: m -> [c]
    getRow :: (Vector v c) => Int -> m -> v
    getColumn :: (Vector v c) => Int -> m -> v
    matGetC :: m -> (Int,Int) -> c
    matPrint :: m -> IO ()
    matDim :: m -> Int
    matMul :: m -> m -> m
    matMulVec :: (Vector v c) => m -> v -> v
    matMap :: (c -> c) -> m -> m
    transpose :: m -> m
    luDecomposition :: m -> (m,m)
    forwardSubstitution :: m -> [c] -> [c] -- this uses lists instead of vectors because the type system was being a bitch
    invert :: m -> Maybe m
    det :: m -> c
    translationMat :: (Vector v c) => v -> m
    scaleMat :: (Vector v c) => v -> m
    identity :: m

    getRow i a' =
        let n = matDim a'
            a = matGetC a' --(\(i,j) -> (matToList a') !! (i*n+j))
        in newVector $ [a(i,j) | j <- [0..(n-1)]]
    getColumn j a' = getRow j (transpose a')
    matGetC a' =
        let y = (\(i,j) -> identity !! (i*4+j))
            n = matDim a'
        in if n > 4
            then error "matGetC: matrix dimension too large"
            else (\(i,j) -> if i<n && j<n then (matToList a') !! (i*n+j) else y(i,j))
    matPrint a' =
        let n = (matDim a')
            a = (\(i,j) -> (matToList a') !! (i*n+j))
        in sequence_ $ do
            i <- [0..(n-1)]
            j <- [0..(n-1)]
            let c = a(i,j)
            let printfunc (i',j') | i'==0,j'==0         = putStr $ "[" ++ show c ++ ","
                                  | i'==(n-1),j'==(n-1) = putStrLn $ "" ++ show c ++ "]"
                                  | j'==(n-1)           = putStrLn $ "" ++ show c ++ ","
                                  | otherwise           = putStr $ "" ++ show c ++ ","
            return $ printfunc (i,j)
    matDim a =
        case length $ matToList a of
            4 -> 2
            9 -> 3
            16 -> 4
            otherwise -> error "matDim: this is not a quadratic 4x4, 3x3 or 2x2 matrix!"
    matMul a' b' =
        let n = if matDim a' == matDim b' then matDim a' else error "matMul: matrices are not of the same dimension"
            a = (\(i,j) -> (matToList a') !! (i*n+j))
            b = (\(i,j) -> (matToList b') !! (i*n+j))
            c = (\(i,j) -> sum [a(i,k)*b(k,j) | k <- [0..(n-1)]])
        in newMatrix [c(i,j) | i <- [0..(n-1)], j <- [0..(n-1)]]
    matMulVec a' v' =
        let n = matDim a'
            a = (\(i,j) -> (matToList a') !! (i*n+j))
            v = (\j -> (vecToList v') !! j)
            w = (\i -> sum [a(i,k)*v(k) | k <- [0..(n-1)]])
        in newVector [w(i) | i <- [0..(n-1)]]
    matMap f m = newMatrix $ map f $ matToList m
    transpose a' =
        let n = matDim a'
            a = (\(i,j) -> (matToList a') !! (i*n+j))
        in newMatrix [a(j,i) | i <- [0..(n-1)], j <- [0..(n-1)]]
    luDecomposition a' =
        -- facts i remember:
        -- + using crouts algorithm
        -- + decomposing into _u_pper and _l_ower triangle matrices
        -- + L has always 1.0 on its diagonal
        -- + so we can compute the determinant easily by taking the product of the diagonal components of U
        -- + i have no idea what pivoting is and didn't implement it
        let n = matDim a'
            a (i,j) = (matToList a') !! (i*n+j)
            u (i,j) = a(i,j) - sum [l(i,k)*u(k,j) | k <- [0..(i-1)]]
            l (i,j) = if u(j,j) == 0.0
                       then 0.0
                       else (a(i,j) - sum [l(i,k)*u(k,j) | k <- [0..(j-1)]]) / u(j,j)
        in ( newMatrix [l(i,j) | i <- [0..(n-1)], j <- [0..(n-1)]]
           , newMatrix [u(i,j) | i <- [0..(n-1)], j <- [0..(n-1)]] )
    forwardSubstitution a' e' =
        let n = matDim a'
            a = (\(i,j) -> (matToList a') !! (i*n+j))
            e = (\i -> e' !! i)
            x = (\i -> [(e(k) - sum [ a(k,l)*x(l) | l <- [0..(k-1)]]) / a(k,k) | k <- [0..(n-1)]] !! i)
        in [x(k) | k <- [0..(n-1)]]
    invert a' =
        -- inversion is somewhat easy, with the help of the luDecomposition;
        -- we do a 'forwardSubstitution' to solve a linear system of equations so
        -- that LI*L = I and UI*U = I, then the inverse of A is UI * LI (see wikipedia),
        -- if the determinat of A is 0 however, A is not invertible
        let (l',u') = luDecomposition a'
            n = matDim a'
            u = (\(i,j) -> (matToList u') !! (i*n+j))
            d = product $ [ u(k,k) | k <- [0..(n-1)]]
            -- these e's are the 'base vectors' of the identity matrix for dimension n
            e = (\i -> [if k== i then 1.0 else 0.0 | k <- [0..(n-1)]])
            -- we transpose U so it is a lower triangle matrix and we can use the forwardSubstitution
            ui = newMatrix $ concat $ [forwardSubstitution (transpose u') (e k) | k <- [0..(n-1)]]
            li = transpose $ newMatrix $ concat $ [forwardSubstitution l' (e k) | k <- [0..(n-1)]]
        in if d==0
            then Nothing
            else Just $ matMul ui li
    det a =
        let n = matDim a
            (l',u') = luDecomposition a
            u = (\(i,j) -> (matToList u') !! (i*n+j))
        in product $ [u(k,k) | k <- [0..(n-1)]]
    translationMat v =
        let (GL.Vector3 x y z) = fromVector v
        in newMatrix $
            [1.0,0.0,0.0,x
            ,0.0,1.0,0.0,y
            ,0.0,0.0,1.0,z
            ,0.0,0.0,0.0,1.0]
    scaleMat v =
        let (GL.Vector3 x y z) = fromVector v
        in newMatrix $
            [x  ,0.0,0.0,0.0
            ,0.0,y  ,0.0,0.0
            ,0.0,0.0,z  ,0.0
            ,0.0,0.0,0.0,1.0]
    identity = newMatrix $
        [1.0,0.0,0.0,0.0
        ,0.0,1.0,0.0,0.0
        ,0.0,0.0,1.0,0.0
        ,0.0,0.0,0.0,1.0]

instance (Fractional c, Show c, Eq c) => Matrix [c] c where
    newMatrix xs | null xs = error "newMatrix: empty list"
                 | otherwise = xs
    fromMatrix a' =
        -- we can only convert from NxN to 4x4, but its ok, we'll just represent everything as 4x4 matrix
        -- ^ that causes problems when trying to use it with say a 3x3 and a 2x2 matrix, we can't use fromMatrix on both of them
        let y = (\(i,j) -> identity !! (i*4+j))
            n = matDim a'
            a = (\(i,j) -> if i<n && j<n then (matToList a') !! (i*n+j) else y(i,j))
        in newMatrix [a(i,j) | i <- [0..3], j <- [0..3]]
    matToList xs = xs

testfoo =
    let a = [1,2,3,
             4,5,6,
             7,8,9]
        b = [10,11,12,0,
             13,14,15,0,
             16,17,18,0,
             0,0,0,1]
    in matPrint $ matMul (fromMatrix a) b

testfoo2 =
    let a = [4,3,
             6,3]
        (l,u) = luDecomposition a
    in do
        matPrint l
        matPrint u

testfoo3 =
    let a = [-1,5,-1,
             -2,11,7,
             1,-5,2]
        mi = fromJust $ invert a
    in do
        matPrint mi
        matPrint $ matMul mi a
        print $ det a
