import Data.List
import Data.Maybe

-- Ejercicio 1

data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
    deriving (Eq, Ord, Show)

class Punto p where
    dimension :: p -> Int
    coord :: Int -> p -> Double
    dist :: p -> p -> Double
    dist p1 p2 = sum [(coord i p2 - coord i p1)^ 2 | i <- [0..dimension p1 - 1]]

newtype Punto2d = P2d (Double, Double) deriving Show
newtype Punto3d = P3d (Double, Double, Double) deriving Show

instance Punto Punto2d where
    dimension _ = 2
    coord 0 (P2d p) = fst p
    coord 1 (P2d p) = snd p

instance Punto Punto3d where
    dimension _ = 3
    coord 0 (P3d (x, _, _)) = x
    coord 1 (P3d (_, y, _)) = y
    coord 2 (P3d (_, _, z)) = z

-- EJERCICIO 2

ordenar :: Punto p => Int -> [p] -> [p]
ordenar eje = sortBy (\a b -> compare (coord eje a) (coord eje b))

mayorMediana :: Punto p => [p] -> Int -> Int
mayorMediana lista eje = fromMaybe 0 (findIndex (\p -> coord eje p > mediana) lista) - 1
    where mediana = coord eje (lista!!(length lista `div` 2))

generarArbol :: Punto p => Int -> [p] -> NdTree p
generarArbol _ [] = Empty
generarArbol nivel (x:xs) = Node (generarArbol (nivel + 1) menores) mediana (generarArbol (nivel + 1) mayores) (nivel `mod` dimension x)
    where
        lista = ordenar (nivel `mod` dimension x) (x:xs)
        (menores, mediana:mayores) = splitAt (mayorMediana lista (nivel `mod` dimension x)) lista

fromList :: Punto p => [p] -> NdTree p
fromList = generarArbol  0

-- >>> fromList [P2d(2,3), P2d(5,4), P2d(9,6), P2d(4,7), P2d(8,1), P2d(7,2)]
-- >>> fromList [P2d(1,2), P2d(1,3), P2d(2,3), P2d(3,3), P2d(1,4)]
-- >>> generarArbol 1 [P2d(1,2), P2d(1,3), P2d(2,3), P2d(3,3), P2d(1,4)]
-- Node (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 0) 1) (P2d (7.0,2.0)) (Node Empty (P2d (8.0,1.0)) (Node Empty (P2d (9.0,6.0)) Empty 0) 1) 0
-- Node (Node Empty (P2d (1.0,2.0)) (Node Empty (P2d (1.0,3.0)) Empty 0) 1) (P2d (1.0,4.0)) (Node Empty (P2d (2.0,3.0)) (Node Empty (P2d (3.0,3.0)) Empty 0) 1) 0
-- Node (Node (Node Empty (P2d (1.0,2.0)) Empty 1) (P2d (1.0,3.0)) (Node Empty (P2d (2.0,3.0)) Empty 1) 0) (P2d (3.0,3.0)) (Node Empty (P2d (1.0,4.0)) Empty 0) 1

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p Empty = Node Empty p Empty 0
