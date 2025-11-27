
module Pred where

import Dibujo

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred f d = foldDib (\x-> if pred x then f x else Basica x) rotar rot45 espejar apilar juntar encimar d

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib f = foldDib f id id id (\_ _ x y -> x || y) (\_ _ x y -> x || y) (||)
-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib f = foldDib f id id id (\_ _ x y -> x && y) (\_ _ x y -> x && y) (&&)


-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 dib = (probar_rotar (lista_DIB_rotar dib) 4 0)

{-
 -Crea una lista de booleanos donde
 -el constructor Rotar es True y el resto False
 -
 - Implementado así para poder trabajar con las funciones
 - de listas que haskell ya provee.
 -}
lista_DIB_rotar :: Dibujo a -> [Bool]
lista_DIB_rotar dib = foldDib (\x -> []) (\x -> True:x) (\x -> False:x) (\x -> False:x) (\_ _ x y -> (False:x) ++ (False:y))(\_ _ x y -> (False:x) ++ (False:y)) (\x y -> (False:x) ++ (False:y)) dib

{-
 -Verifica que haya k True's seguidos
 -
 - El k indica la cantidad de True's que 
 - deben estar seguidos
 - El n indica la posicion de la lista 
 - desde la cual se toman k elementos para analizar
 - 
 - La primera parte de la 1er guarda es para cuando se ingresa una lista con
 -una cantidad menor a k elementos. En el caso de k=4, seria una lista 
 -con 1,2,3 booleanos. 
 -
 -La segunda parte es para saber cuando 'cortar', que es cuando la lista
 -no es exactamente un multiplo de k
 -
 - La Segunda guarda es para saber cuando 'cortar', cuando la
 - lista es un multiplo de k.
 -}
probar_rotar :: [Bool] -> Int -> Int -> Bool
probar_rotar xs k n
  | length xs < k ||  (n > (length xs) - k && ((mod (length xs) k) /= 0)) = False
  | n == ((length xs) - (k-1))  && (mod (length xs) k) == 0 = False
  | otherwise = (all (== True) (take k (drop n xs))) || probar_rotar xs k (n+1)


-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 dib = probar_espejar (lista_DIB_espejar dib) 2 0

lista_DIB_espejar :: Dibujo a -> [Bool]
lista_DIB_espejar dib = foldDib (\x -> []) (\x -> (False:x)) (\x -> (False:x)) (\x -> (True:x)) (\_ _ x y -> (False:x) ++ (False:y)) (\_ _ x y -> (False:x) ++ (False:y)) (\x y -> (False:x) ++ (False:y)) dib

probar_espejar :: [Bool] -> Int -> Int -> Bool
probar_espejar xs k n
    | length xs < k ||  (n > (length xs) - k && ((mod (length xs) k) /= 0)) = False
    | n == ((length xs) - (k-1))  && (mod (length xs) k) == 0 = False
    | otherwise = (all (== True) (take k (drop n xs))) || probar_espejar xs k (n+1)

data Superfluo = RotacionSuperflua | FlipSuperfluo deriving (Eq, Show)

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion dib = if esRot360 dib then [RotacionSuperflua] else []

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip dib = if esFlip2 dib then [FlipSuperfluo] else []


-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo dib = 
    let errores = errorFlip dib ++ errorRotacion dib
    in if null errores
       then Right dib
       else Left errores
