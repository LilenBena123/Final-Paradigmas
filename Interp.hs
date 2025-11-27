
-- Sacar del esqueleto final!
module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar f d w h = f (d V.+ w) h (V.negate w) 

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar f d w h = f (d V.+ w) (V.negate w) h

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 f d w h = f (d V.+ new_w) new_w (new_h)
 where
  new_w = 0.5 V.*(w V.+ h)
  new_h = 0.5 V.*(h V.- w)

--interpreta el operador de apilar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar m n f g d w h = pictures[f d new_w h, g new_d (new_r V.* w) h]
 where
  new_d = d V.+ new_w
  r = m / (m+n)
  new_w = r V.* w
  new_r = n / (m+n)

--interpreta el operador de juntar
interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar m n f g d w h = pictures[f (d V.+ new_h) w (r V.* h), g d w new_h ]
 where
  r = m / (m + n)
  new_h = new_r V.* h
  new_r = n / (m+n)
--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar f g d w h = pictures[f d w h, g d w h]

--interpreta cualquier expresion del tipo Dibujo a
--Caso recursivo como para entender que hay que hacer
{-
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f (Basica a) = f a
interp f (Rotar d1) = rotate 90 (interp f d1)
interp f (Rotar45 d1) = rotate 45 (interp f d1)
interp f (Encimar d1 d2) = pictures[interp f d1, interp f d2]
interp f (Apilar i j d1 d2) = pictures[interp f i d1, interp f j d2]
interp f (Juntar i j d1 d2) = pictures[interp f i d1, interp f j d2]
interp f (Espejar d1 d2) = pictures[interp f d1, interp f d2]
-}

interpBas :: Float -> ImagenFlotante
interpBas a _ _ _ = color red (circleSolid a)

interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp interpBas dib = foldDib interpBas interp_rotar interp_rotar45 interp_espejar interp_juntar interp_apilar interp_encimar dib
