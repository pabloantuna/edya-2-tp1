data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
    deriving (Eq, Ord, Show)

class Punto p where
    dimension :: p -> Int
    coord :: Int -> p -> Double
    dist :: p -> p -> Double
    dist p1 p2 = 
        let f 1 v1 v2 = ((coord 1 v2) - (coord 1 v1))^2
            f n v1 v2 = f (n-1) v1 v2 + ((coord n v2) - (coord n v1))^2
        in
            sqrt (f m p1 p2)
        where
            m = dimension p1

newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)

instance Punto Punto2d where
    dimension _ = 2
    coord 1 (P2d (x, y)) = x
    coord 2 (P2d (x, y)) = y
    
instance Punto Punto3d where
    dimension _ = 3
    coord 1 (P3d (x, y, z)) = x
    coord 2 (P3d (x, y, z)) = y
    coord 3 (P3d (x, y, z)) = z
