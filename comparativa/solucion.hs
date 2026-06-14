data EstadoSemaforo = EnRojo | EnAmarillo | EnVerde | AmarilloIntermitente
  deriving (Show, Eq) 

  transicion :: EstadoSemaforo -> ColorDestino -> (EstadoSemaforo, String)
  transicion EnRojo Verde               = (AmarilloIntermitente, "cambiar-a-amarillo-intermitente")
  transicion AmarilloIntermitente Verde = (EnVerde, "cambiar-a-verde")
  transicion EnVerde Amarillo               = (AmarilloIntermitente, "cambiar-a-amarillo-intermitente")
  transicion AmarilloIntermitente Amarillo = (EnAmarillo, "cambiar-a-en-amarillo")
  transicion EnAmarillo Rojo               = (AmarilloIntermitente, "cambiar-a-amarillo-intermitente")
  transicion AmarilloIntermitente Rojo     = (EnRojo, "cambiar-a-en-rojo")
  
