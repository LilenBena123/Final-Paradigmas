module Basica.Figuras where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo
import Interp

-- el vector nulo
vCero :: Vector
vCero = (0,0)

rectangulo :: ImagenFlotante
rectangulo a b c = polygon [vCero, a, a V.+b, b]  

semicirculo :: ImagenFlotante
semicirculo a b c = arcSolid 0 180 25

circulo :: ImagenFlotante
circulo a b c = circleSolid 5

circulo_negro_borde :: ImagenFlotante
circulo_negro_borde a b c = Color black $ circle 5

rectangulo_negro :: ImagenFlotante
rectangulo_negro a b c = translate 50 50 $ Color black (rectangleSolid 50 5)

semicirculo_rojo :: ImagenFlotante
semicirculo_rojo a b c = translate 50 50 $ Color red $ semicirculo a b c

semicirculo_blanco :: ImagenFlotante
semicirculo_blanco a b c = translate 50 50 $ Pictures[Color white $ arcSolid 180 360 25,
                                    Color black $ arc 180 360 25]

circulo_blanco :: ImagenFlotante
circulo_blanco a b c = translate 50 50 $ Pictures[Color white $ circulo a b c,
                                circulo_negro_borde a b c]

triangulo :: ImagenFlotante
triangulo a b c = polygon $ map (a V.+) [vCero, mitad b V.+ c , b , vCero]

trianguloVioleta :: ImagenFlotante
trianguloVioleta a b c = color violet $ triangulo a b c

trianguloVioleta2 :: ImagenFlotante
trianguloVioleta2 a b c =  translate 12.5 (-19) $ scale 0.75 1.20 $ color violet $ triangulo a b c

poligono :: ImagenFlotante
poligono a b c = polygon $ map (a V.+) [vCero, b, b V.+ c, c, vCero]

poligonoVioletaO :: ImagenFlotante
poligonoVioletaO a b c = translate 12.5 (-19) $ scale 0.75 1.20 $ color violetaOscuro $ poligono a b c
        where violetaOscuro = dim violet


trianguloVerde :: ImagenFlotante
trianguloVerde a b c = translate 12.5 21 $ scale 0.75 0.80 $ color green $ triangulo a b c


lineaVerde :: ImagenFlotante
lineaVerde a b c = translate 50 23 $ color green $ rectangleSolid 15.0 43.0

-- ""color"" nuevo:
transparente :: Color
transparente = makeColorI 1 0 0 0

cuadrado :: ImagenFlotante
cuadrado a b c = polygon $ map (a V.+) [vCero, b, b V.+ c, c, vCero]

trianguloVerde1 :: ImagenFlotante
trianguloVerde1 a b c = color green $ triangulo a b c

cuadradoMarron :: ImagenFlotante
cuadradoMarron a b c = color orange $ cuadrado a b c

cuadradoTransparente :: ImagenFlotante
cuadradoTransparente a b c = color transparente $ cuadrado a b c

trianguloRecto :: ImagenFlotante
trianguloRecto origen ancho alto =
  color green $ polygon $ map (origen V.+) [vCero, ancho, alto, vCero]
