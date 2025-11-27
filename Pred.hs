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
-- cambiar :: Pred a -> a -> Dibujo a -> Dibujo a

-- Alguna básica satisface el predicado.
{-
anyDib :: Pred a -> Dibujo a -> Bool
anyDib f a = f a
anyDib f (rotar a) =  (anyDib f a)
anyDib f (rot45 a) = (anyDib f a)
anyDib f (espejar a) = (anyDib f a)
anyDib f (apilar i j a b) = (anyDib f a) || (anyDib f b)
anyDib f (juntar i j a b) = (anyDib f a) || (anyDib f b)
anyDib f (encimar a b) = (anyDib f a) || (anyDib f b)
-}

anyDib :: Pred a -> Dibujo a -> Bool
anyDib f = foldDib f id id id (\_ _ x y -> x || y) (\_ _ x y -> x || y) (||)
{-
-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib f a = f a
allDib f (rotar a) =  (allDib f a)
allDib f (rot45 a) = (allDib f a)
allDib f (espejar a) = (allDib f a)
allDib f (apilar i j a b) = (allDib f a) && (allDib f b)
allDib f (juntar i j a b) = (allDib f a) && (allDib f b)
allDib f (encimar a b) = (allDib f a) && (allDib f b) -}

allDib :: Pred a -> Dibujo a -> Bool
allDib f = foldDib f id id id (\_ _ x y -> x && y) (\_ _ x y -> x && y) (&&)

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred a
esRot360 = anyDib (\d -> anyDib (\d' -> anyDib (\d'' -> anyDib (\d''' -> d''' == Rotar d'') d'') d') d)

{-

esRot360_r1 :: Pred (Dibujo a)
esRot360_r1 pred_dib 
                   | (rotar (rotar (rotar (rotar pred_dib))) == pred_dib) = True
                   | otherwise = False

Nico sugiere esto:
contar_rot n dib | rotar(dib) == dib = 1 + contar_rot (n-1) dib

Otra forma de definirlo recursivamente es hacer un contador
cont_rot :: Dibujo a -> Int
cont_rot dib | (rotar dib) == dib = 1 + contar_rot (dib)
             | otherwise = contar_rot (dib)

esRot360_r2 :: Pred (Dibujo a)
esRot360_r2 pred_dib 
                   | (cont_rot pred_dib) == 4 = True
                   | otherwise = False


                   
-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)

data Superfluo = RotacionSuperflua | FlipSuperfluo

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)

-}
