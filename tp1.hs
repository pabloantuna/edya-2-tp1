data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
    deriving (Eq, Ord, Show)

class Punto p where
    dimension :: p -> Int
    coord :: Int -> p -> Double
    dist :: p -> p -> Double
    dist p1 p2 = sum [(coord i p2 - coord i p1)^ 2 | i <- [0..(dimension p1) - 1]] -- robada a castro porque yo me olvide por completo de la existencia de sum
    -- dist p1 p2 = 
    --     let f 1 v1 v2 = ((coord 1 v2) - (coord 1 v1))^2
    --         f n v1 v2 = f (n-1) v1 v2 + ((coord n v2) - (coord n v1))^2
    --     in
    --         f m p1 p2
    --     where
    --         m = dimension p1 - 1

newtype Punto2d = P2d (Double, Double) deriving Show
newtype Punto3d = P3d (Double, Double, Double) deriving Show

instance Punto Punto2d where
    dimension _ = 2
    coord 0 (P2d (x, y)) = x
    coord 1 (P2d (x, y)) = y
    
instance Punto Punto3d where
    dimension _ = 3
    coord 0 (P3d (x, y, z)) = x
    coord 1 (P3d (x, y, z)) = y
    coord 2 (P3d (x, y, z)) = z

--2
{-fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList (p:ps) = -}

splitPunto :: Punto p => [p] -> ([p], [p])
splitPunto [] = ([], [])
splitPunto [x] = ([x], [])
splitPunto (x:y:zs) = let (xs, ys) = splitPunto zs 
                 in (x:xs, y:ys)

mergePunto :: Punto p => ([p], [p]) -> Int -> [p]
mergePunto ([], ys) eje = ys
mergePunto (xs, []) eje = xs
mergePunto (x:xs, y:ys) eje = if coord eje x <= coord eje y then x:mergePunto (xs, y:ys) eje else y:mergePunto (x:xs, ys) eje

msortPunto :: Punto p => [p] -> Int -> [p]
msortPunto [] eje = []
msortPunto [x] eje = [x]
msortPunto xs eje = let (ls, rs) = splitPunto xs
               (ls', rs') = (msortPunto ls eje, msortPunto rs eje)
            in mergePunto (ls', rs') eje

{-mediana :: Punto p => [p] -> Int -> p
mediana xs eje = -}
