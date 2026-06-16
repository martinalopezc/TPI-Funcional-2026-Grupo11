import Data.Int

data Estado = Rojo | Verde | Amarillo | AmarilloIntermitente deriving (Show, Eq)

-- =============================================================================
-- FUNCION: transicion
-- NATURALEZA: Pura (sin efectos secundarios, no modifica el entorno)
-- ESTRATEGIA DE CONTROL: Funcion predicado (evalua condiciones logicas usando pattern matching)
-- IMPACTO EN MEMORIA: No destructiva (retorna una tupla nueva sin alterar parametros)
-- =============================================================================
transicion :: Estado -> Estado -> (Estado, String)
transicion Rojo AmarilloIntermitente = (AmarilloIntermitente, "cambiar-a-amarillo-intermitente")
transicion AmarilloIntermitente Verde = (Verde, "cambiar-a-verde")
transicion Verde AmarilloIntermitente = (AmarilloIntermitente, "cambiar-a-amarillo-intermitente")
transicion AmarilloIntermitente Amarillo = (Amarillo, "cambiar-a-en-amarillo")
transicion Amarillo Rojo = (AmarilloIntermitente, "cambiar-a-amarillo-intermitente")
transicion AmarilloIntermitente Rojo = (Rojo, "cambiar-a-en-rojo")
transicion estadoActual _ = (estadoActual, "accion-por-defecto")

-- =============================================================================
-- FUNCION: timer
-- NATURALEZA: Pura (dado un timestamp, siempre retorna el mismo color de estado)
-- ESTRATEGIA DE CONTROL: Funcion predicado (evalua rangos numericos usando guardas)
-- IMPACTO EN MEMORIA: No destructiva
-- =============================================================================
timer :: Int64 -> Estado
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

-- =============================================================================
-- FUNCION: main
-- NATURALEZA: Impura (modulo de ejecucion principal con efectos secundarios de IO)
-- ESTRATEGIA DE CONTROL: Secuencial (bloque do de evaluacion interactiva)
-- IMPACTO EN MEMORIA: No destructiva
-- =============================================================================
main :: IO ()
main = do
    putStrLn "=== PRUEBA MINIMA DE FUNCIONES OBLIGATORIAS ==="
    
    putStrLn $ "Resultado Transicion: " ++ show (transicion Rojo AmarilloIntermitente)
    
    let timestampCritico = 2147483647 + 1 :: Int64
    putStrLn $ "Resultado Timer en tiempo critico: " ++ show (timer timestampCritico)