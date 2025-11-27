module Basica.Flor where
import Dibujo
import Interp
import Basica.Figuras

data Basica= TrianguloVioleta2|PoligonoVioletaO|LineaVerde|TrianguloVerde

tallo :: Dibujo Basica
tallo = apilar 1 1 (hojas) (Rotar(Rotar(pureDib LineaVerde)))

hojas :: Dibujo Basica
hojas = juntar 1 1 (Rotar(pureDib TrianguloVerde)) (Rotar(Rotar(Rotar(pureDib TrianguloVerde))))


flor :: Dibujo Basica
flor= apilar 1 1 (pureDib (TrianguloVioleta2))(Rotar45( pureDib (PoligonoVioletaO)))

ejemplo:: Dibujo Basica
ejemplo = apilar 1 1 flor tallo


interpBas :: Basica -> ImagenFlotante
interpBas TrianguloVioleta2 = trianguloVioleta2
interpBas PoligonoVioletaO = poligonoVioletaO
interpBas LineaVerde = lineaVerde
interpBas TrianguloVerde = trianguloVerde
