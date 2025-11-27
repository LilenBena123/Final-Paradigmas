module Basica.Pino where
import Dibujo
import Interp
import Basica.Figuras

data Basica = CuadradoMarron | TrianguloVerde | CuadradoTransparente | TrianguloRecto

------------------- Partes del dibujo a realizar -------------------
transparencia :: Dibujo Basica
transparencia = (pureDib CuadradoTransparente)

tronco :: Dibujo Basica
tronco = ((///) (Juntar 1 0.5 transparencia (pureDib CuadradoMarron)) (Juntar 0.5 1 (pureDib CuadradoMarron) transparencia))

capa1 :: Dibujo Basica
capa1 = ((///) (Juntar 0.75 1 transparencia (espejar (pureDib TrianguloRecto))) (Juntar 1 0.75 (pureDib TrianguloRecto) transparencia))

capa2 :: Dibujo Basica
capa2 = ((///) (Juntar 0.3 1 transparencia (espejar (pureDib TrianguloRecto))) (Juntar 1 0.3 (pureDib TrianguloRecto) transparencia))

capa3 :: Dibujo Basica
capa3 = (pureDib TrianguloVerde)

-------------------------- Dibujo: ÁRBOL --------------------------
ejemplo :: Dibujo Basica
ejemplo = ((.-.) (Apilar 0.7 0.8 capa1 capa2) ((.-.) capa3 tronco))

interpBas :: Basica -> ImagenFlotante
interpBas TrianguloVerde = trianguloVerde1
interpBas CuadradoMarron = cuadradoMarron
interpBas CuadradoTransparente = cuadradoTransparente
interpBas TrianguloRecto = trianguloRecto
