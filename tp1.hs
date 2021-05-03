import Data.List ( maximumBy, minimumBy, findIndex, sortBy )
import Data.Maybe ( fromMaybe )

-- Ejercicio 1

data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
    deriving (Eq, Ord, Show)

class Punto p where
    dimension :: p -> Int
    coord :: Int -> p -> Double
    dist :: p -> p -> Double
    dist p1 p2 = sum [(coord i p2 - coord i p1)^ 2 | i <- [0..dimension p1 - 1]]

newtype Punto2d = P2d (Double, Double) deriving (Eq, Show)
newtype Punto3d = P3d (Double, Double, Double) deriving (Eq, Show)

instance Punto Punto2d where
    dimension _ = 2
    coord 0 (P2d p) = fst p
    coord 1 (P2d p) = snd p

instance Punto Punto3d where
    dimension _ = 3
    coord 0 (P3d (x, _, _)) = x
    coord 1 (P3d (_, y, _)) = y
    coord 2 (P3d (_, _, z)) = z

-- Ejercicio 2

comparar :: Punto p => Int -> p -> p -> Ordering
comparar eje a b = compare (coord eje a) (coord eje b)

ordenar :: Punto p => Int -> [p] -> [p]
ordenar eje = sortBy (comparar eje)

mayorMediana :: Punto p => [p] -> Int -> Int
mayorMediana lista eje = fromMaybe (length lista) (findIndex (\p -> coord eje p > mediana) lista) - 1
    where mediana = coord eje (lista!!(length lista `div` 2))

generarArbol :: Punto p => Int -> [p] -> NdTree p
generarArbol _ [] = Empty
generarArbol nivel (x:xs) = Node (generarArbol (nivel + 1) menores) mediana (generarArbol (nivel + 1) mayores) (nivel `mod` dimension x)
    where
        lista = ordenar (nivel `mod` dimension x) (x:xs)
        (menores, mediana:mayores) = splitAt (mayorMediana lista (nivel `mod` dimension x)) lista

fromList :: Punto p => [p] -> NdTree p
fromList = generarArbol  0

-- >>> fromList [P2d (2,3)]
-- Node Empty (P2d (2.0,3.0)) Empty 0

-- >>> fromList [P2d(2,3), P2d(5,4), P2d(9,6), P2d(4,7), P2d(8,1), P2d(7,2)]
-- >>> fromList [P2d(1,2), P2d(1,3), P2d(2,3), P2d(3,3), P2d(1,4)]
-- >>> generarArbol 1 [P2d(1,2), P2d(1,3), P2d(2,3), P2d(3,3), P2d(1,4)]
-- Node (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 0) 1) (P2d (7.0,2.0)) (Node (Node Empty (P2d (8.0,1.0)) Empty 0) (P2d (9.0,6.0)) Empty 1) 0
-- Node (Node (Node Empty (P2d (1.0,2.0)) Empty 0) (P2d (1.0,3.0)) Empty 1) (P2d (1.0,4.0)) (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (3.0,3.0)) Empty 1) 0
-- Node (Node (Node Empty (P2d (1.0,2.0)) Empty 1) (P2d (1.0,3.0)) (Node Empty (P2d (2.0,3.0)) Empty 1) 0) (P2d (3.0,3.0)) (Node Empty (P2d (1.0,4.0)) Empty 0) 1

-- Ejercicio 3

insertar_ :: Punto p => Int -> p -> NdTree p -> NdTree p
insertar_ n p Empty = Node Empty p Empty n
insertar_ n p (Node left x right eje) =
    if comparar eje p x == GT
    then Node left x (insertar_ ((n + 1) `mod` dimension x) p right) eje
    else Node (insertar_ ((n + 1) `mod` dimension x) p left) x right eje

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p Empty = Node Empty p Empty 0
insertar p arbol@(Node l x r ejeInicial) = insertar_ ejeInicial p arbol

-- >>> insertar (P2d (8.0, 9.0)) (Node (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 0) 1) (P2d (7.0,2.0)) (Node Empty (P2d (8.0,1.0)) (Node Empty (P2d (9.0,6.0)) Empty 0) 1) 0)
-- Node (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 0) 1) (P2d (7.0,2.0)) (Node Empty (P2d (8.0,1.0)) (Node (Node Empty (P2d (8.0,9.0)) Empty 1) (P2d (9.0,6.0)) Empty 0) 1) 0

-- Ejercicio 4

buscarReemplazo :: (Eq p, Punto p) => NdTree p -> Int -> Bool -> p
buscarReemplazo (Node left p right _) eje buscarMenor =
    if buscarMenor
    then minimumBy (comparar eje) candidatos
    else maximumBy (comparar eje) candidatos
    where candidatos = [buscarReemplazo left eje buscarMenor | left /= Empty] ++ [p] ++ [buscarReemplazo right eje buscarMenor | right /= Empty]

reemplazar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
reemplazar p (Node Empty x Empty eje) = Empty
reemplazar p (Node left x Empty eje) = Node (eliminar reemplazo left) reemplazo Empty eje
    where reemplazo = buscarReemplazo left eje False
reemplazar p (Node left x right eje) = Node left reemplazo (eliminar reemplazo right) eje
    where reemplazo = buscarReemplazo right eje True

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p (Node left x right eje)
  | p == x = reemplazar p (Node left x right eje)
  | comparar eje p x == GT = Node left x (eliminar p right) eje
  | otherwise = Node (eliminar p left) x right eje

-- >>> eliminar (P2d (8.0, 9.0)) (insertar (P2d (8.0, 9.0)) (Node (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 0) 1) (P2d (7.0,2.0)) (Node Empty (P2d (8.0,1.0)) (Node Empty (P2d (9.0,6.0)) Empty 0) 1) 0))
-- Node (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 0) 1) (P2d (7.0,2.0)) (Node Empty (P2d (8.0,1.0)) (Node Empty (P2d (9.0,6.0)) Empty 0) 1) 0

-- Ejercicio 5

type Rect = (Punto2d, Punto2d)

inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (px, py)) (P2d (r1x, r1y), P2d (r2x, r2y)) =
    px >= min r1x r2x
    && px <= max r1x r2x
    && py >= min r1y r2y
    && py <= max r1y r2y

-- >>> inRegion (P2d (100, 99)) (P2d (-99,-99), P2d (99,99))
-- False

orthogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
orthogonalSearch Empty rect = []
orthogonalSearch (Node Empty p Empty eje) rect = [p | inRegion p rect]
orthogonalSearch (Node left p right eje) (r1, r2) =
    let (low, high) = if comparar eje r1 r2 == GT then (r2, r1) else (r1, r2)
        resultLeft = if comparar eje low p == GT then [] else orthogonalSearch left (r1, r2)
        resultRight = if comparar eje high p == GT then orthogonalSearch right (r1, r2) else []
    in resultLeft ++ [p | inRegion p (r1, r2)] ++ resultRight

-- >>> orthogonalSearch (Node (Node (Node Empty (P2d (2.0,3.0)) Empty 0) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 0) 1) (P2d (7.0,2.0)) (Node Empty (P2d (8.0,1.0)) (Node Empty (P2d (9.0,6.0)) Empty 0) 1) 0) (P2d (2,3),P2d (7,7))
-- [P2d (2.0,3.0),P2d (5.0,4.0),P2d (4.0,7.0)]
