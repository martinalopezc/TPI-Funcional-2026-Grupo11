import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Int


data Estado = Rojo | Verde | Amarillo | AmarilloIntermitente deriving (Show, Eq)

-- =============================================================================
-- FUNCION: timer
-- NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color de estado)
-- ESTRATEGIA DE CONTROL: Funcion Predicado (Evalua rangos numericos usando guardas)
-- IMPACTO EN MEMORIA: No destructiva
-- =============================================================================
timer :: Int64 -> Estado
timer timestamp =
    let duracionCiclo = 90 + 3 + 120 + 3 + 6 + 3
        segundoActual = timestamp mod duracionCiclo
    in condicional segundoActual
  where
    condicional s
        | s < 90  = Rojo
        | s < 93  = AmarilloIntermitente
        | s < 213 = Verde
        | s < 216 = AmarilloIntermitente
        | s < 222 = Amarillo
        | otherwise = AmarilloIntermitente
