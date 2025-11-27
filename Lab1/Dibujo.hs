
module Dibujo (
  Dibujo(..),
  comp,
  (^^^),
  (.-.),
  (///),
  rotar,
  rot45,
  encimar,
  apilar,
  pureDib,
  espejar,
  juntar,
  r90,
  r180,
  r270,
  encimar4,
  cuarteto,
  ciclar,
  mapDib,
  foldDib,
  )
where

-- Definir el lenguaje via constructores de tipo
data TrioRect = Triangulo | Rectangulo deriving(Eq, Show)

data Dibujo a = Basica a
              | Rotar (Dibujo a)
              | Rotar45 (Dibujo a)
              | Espejar (Dibujo a) 
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
                deriving(Eq, Show)
-- a == (x,y)
-- a == [(x,y),(z,w]

-- Asociatividad de ciertos métodos
-- Nota: la definición infixr de un operador supone que el operador asocia a derecha 

infixr 6 ^^^ -- +

infixr 7 .-. -- * 

infixr 8 /// -- ()

-- Funciones constructoras del lenguaje (no venían con el skeleton)
-- Debido a que debe implementarse mapDib y foldDib es necesario implementarlas

-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib value = Basica value

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rotar45

espejar :: Dibujo a -> Dibujo a 
espejar = Espejar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar 

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar 

-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp _ 0  = id
comp f value  = f . comp f (value-1) 


-- Rotaciones de múltiplos de 90.
r90 :: Dibujo a -> Dibujo a
r90 = Rotar 

r180 :: Dibujo a -> Dibujo a
r180 = Rotar . Rotar

r270 :: Dibujo a -> Dibujo a
r270 = Rotar . Rotar . Rotar


-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar 

-- '$' el operador de menor precedencia, tiene precedencia 0 
-- Nota: el operador '$' es el operador de menor precedencia y es tipo infixr

-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = Apilar 1 1 (Juntar 1 1 a b ) (Juntar 1 1 c d)

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dib = Encimar dib $ Encimar (r90 dib) $ Encimar (r180 dib) (r270 dib)

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (r90 d) (r180 d) (r270 d)


-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f = foldDib (pureDib . f) rotar rot45 espejar juntar apilar encimar

-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
foldDib fig rot r45 esp junt api enc dib = case dib of
  Basica a -> fig a
  Rotar d -> rot $ foldDib fig rot r45 esp junt api enc d
  Rotar45 d -> r45 $ foldDib fig rot r45 esp junt api enc d
  Espejar d -> esp $ foldDib fig rot r45 esp junt api enc d
  Juntar x y d e -> junt x y (foldDib fig rot r45 esp junt api enc d) (foldDib fig rot r45 esp junt api enc e)
  Apilar x y d e -> api x y (foldDib fig rot r45 esp junt api enc d) (foldDib fig rot r45 esp junt api enc e)
  Encimar d e -> enc (foldDib fig rot r45 esp junt api enc d) (foldDib fig rot r45 esp junt api enc e)

contar_basica:: Dibujo a -> Int
contar_basica (Basica x) = 1
contar_basica (Rotar d1) = contar_basica d1
contar_basica (Rotar45 d1) = contar_basica d1
contar_basica (Espejar d1) = contar_basica d1
contar_basica (Apilar i j d1 d2) = contar_basica d1 + contar_basica d2
contar_basica (Juntar i j d1 d2) = contar_basica d1 + contar_basica d2
contar_basica (Encimar d1 d2) = contar_basica d1 + contar_basica d2


--Pequeño comentario sobre como funcionan mapDib y foldDib
{-
  mapDib es como bien sabemos las funciones que va buscar mapear ciertas funciones de un dibujo para tratar de 
  rotar, apilar o alguna otra función definida para el dibujo. Argumentos:
    f :: (a->b) = 'es una función que toma un valor de tipo a y devuelve uno de tipo b'
    un dibujo cualquiera
  Retorna: un dibujo al que se le aplico la función f

  foldDib es un poco más compleja de explicar pero en resumidas cuentas actua de la misma forma que el fold común 
  del prelude de Haskell, en este caso reduciendo el Dibujo a un único valor. La implementación es bastante costosa de
  entender pero la mejor explicación hasta ahora es que busca de alguna forma hacer un pasaje de las funciones, un dibujo a
  un valor. 
  Argumentos:
    fig :: (a->b) = 'es la función pureDib'
    rot :: (b->b) = 'función que rota la figura'
    r45 :: (b->b) = 'función que rota 45 grados a la figura'
    esp :: (b->b) = 'función que espeja a la figura'
    junt :: (Float -> Float -> b -> b -> b) = 'función que junta en una coordenada (x,y) a dos figuras'
    api ::  (Float -> Float -> b -> b -> b) = 'función que apila en una coordenada (x,y) a dos figuras'
    enc :: (b -> b -> b) = 'función que encima dos figuras volviendolas una'
    dib :: Dibujo a
  Retorna:
    value :: b
-}
