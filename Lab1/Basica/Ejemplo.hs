module Basica.Ejemplo where
import Dibujo
import Interp
import Basica.Comun

type Basica = ()

ejemplo :: Dibujo Basica
ejemplo = Espejar(pureDib())

interpBas :: Basica -> ImagenFlotante
interpBas () = formaF
