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
-- se para registrar el cambio de luces según la marca de tiempo dada
registrarCambio :: Int32 -> Estado -> Estado -> IO ()
registrarCambio epoch colorAnterior colorNuevo = do
    let dt = posixSecondsToUTCTime (fromIntegral epoch)
    putStrLn $ "Tiempo [" ++ iso8601Show dt ++ "]: la luz cambio de " ++ show colorAnterior ++ " a " ++ show colorNuevo

-- evaluacion del comportamiento del sistema bajo limites de datos
main :: IO ()
main = do
    putStrLn "=== EVALUACION DE SISTEMA==="
    
   
    let timestampEvaluacion = 2147483647 + 1 :: Int32
    
  
    let estadoActual = timer timestampEvaluacion
    
    
    registrarCambio timestampEvaluacion Rojo estadoActual
