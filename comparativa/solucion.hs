import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Int

 --Definimos los estados posibles del semáforo
data Estado = Rojo | Verde | Amarillo | AmarilloIntermitente deriving (Show, Eq)

-- El temporizador calcula el estado según el segundo del ciclo (0 a 224 segundos)
timer :: Int32 -> Estado
timer timestamp =
    let duracionCiclo = 90 + 3 + 120 + 3 + 6 + 3 
        segundoActual = timestamp `mod` duracionCiclo
    in condicional segundoActual
  where
    condicional s
        | s < 90  = Rojo
        | s < 93  = AmarilloIntermitente
        | s < 213 = Verde
        | s < 216 = AmarilloIntermitente
        | s < 222 = Amarillo
        | otherwise = AmarilloIntermitente
