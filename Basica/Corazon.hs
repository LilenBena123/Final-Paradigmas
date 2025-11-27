module Basica.Corazon where

import Dibujo
import Interp
import Basica.Figuras

data Basica = Triangulo | TrianguloVioleta 

ejemplo :: Dibujo Basica

{- ejemplo de corazon-}
ejemplo = apilar 0.5 0.7 (juntar 1 1 (pureDib TrianguloVioleta) (pureDib TrianguloVioleta)) (r180(pureDib TrianguloVioleta))


{-ejemplo de caperucita (no roja) solo con triangulos-}
--ejemplo = apilar 1 0.2 (apilar 1 1 (rotar(juntar 2 2 (r90(pureDib Triangulo)) (espejar(r90(pureDib Triangulo))))) (pureDib Triangulo) ) (juntar 0.5 0.5 (pureDib Triangulo) (pureDib Triangulo))

interpBas :: Basica -> ImagenFlotante
interpBas Triangulo =  triangulo
interpBas TrianguloVioleta = trianguloVioleta

