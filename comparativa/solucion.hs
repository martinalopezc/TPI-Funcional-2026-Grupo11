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
-- =============================================================================
-- FUNCION: registrarCambio
-- NATURALEZA: Impura (Produce efectos secundarios de salida de datos en la terminal)
-- ESTRATEGIA DE CONTROL: Secuencial (Bloque do para ejecucion de IO)
-- IMPACTO EN MEMORIA: No destructiva
-- =============================================================================
registrarCambio :: Int64 -> Estado -> Estado -> IO ()
registrarCambio epoch colorAnterior colorNuevo = do
    let dt = posixSecondsToUTCTime (fromIntegral epoch)
    putStrLn $ "Tiempo [" ++ iso8601Show dt ++ "]: la luz cambio de " ++ show colorAnterior ++ " a " ++ show colorNuevo

-- =============================================================================
-- FUNCION: main
-- NATURALEZA: Impura (Modulo de ejecucion principal con efectos secundarios de IO)
-- ESTRATEGIA DE CONTROL: Secuencial (Bloque do de evaluacion interactiva)
-- IMPACTO EN MEMORIA: No destructiva
-- =============================================================================
main :: IO ()
main = do
    putStrLn "=== VALIDACION DE SISTEMA ACTUALIZADO ==="
    
    
    let timestampEvaluacion = 2147483647 + 1 :: Int64
    let estadoActual = timer timestampEvaluacion
    
    
    registrarCambio timestampEvaluacion Rojo estadoActual
