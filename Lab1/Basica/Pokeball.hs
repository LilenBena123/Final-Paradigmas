module Basica.Pokeball where
import Dibujo
import Interp
import Basica.Figuras

data Basica = SemicirculoBlanco | RectanguloNegro | SemicirculoRojo | CirculoBlanco


ejemplo :: Dibujo Basica 
ejemplo = encimar (base) (pureDib(CirculoBlanco))
 where
    parte_de_arriba = juntar 1 1 (pureDib(SemicirculoRojo)) ((pureDib(RectanguloNegro))) 
    parte_de_abajo = pureDib(SemicirculoBlanco)
    base = encimar parte_de_arriba parte_de_abajo


interpBas :: Basica -> ImagenFlotante
interpBas RectanguloNegro = rectangulo_negro
interpBas SemicirculoBlanco = semicirculo_blanco
interpBas SemicirculoRojo = semicirculo_rojo
interpBas CirculoBlanco = circulo_blanco

