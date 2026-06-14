;; =============================================================================
;; PARTE 3: MODULO DE PLANIFICACION, ESTADISTICA Y BATERIA DE PRUEBAS
;; =============================================================================
(ql:quickload :local-time)

;; =============================================================================
;; REQUERIMIENTO 4: ANALISIS DE CICLOS SEMAFORICOS
;; =============================================================================

;; =============================================================================
;; FUNCION: duracion-ciclo-total
;; NATURALEZA: Pura (Retorna una constante calculada sim alterar variables globales)
;; ESTRATEGIA DE CONTROL: Evaluacion aritmetica directa
;; IMPACTO EN MEMORIA: No destructiva
;; =============================================================================
(defun duracion-ciclo-total ()
  (+ 90 3 120 3 6 3))

;; =============================================================================
;; FUNCION: recomendacion-ciclo
;; NATURALEZA: Pura (Calcula cadenas de texto basadas estrictamente en la entrada numerica)
;; ESTRATEGIA DE CONTROL: Funcion Predicado (Estructura de analisis condicional)
;; IMPACTO EN MEMORIA: No destructiva
;; =============================================================================
(defun recomendacion-ciclo (duracion)
  (cond
    ((< duracion 35) "No recomendado: Ciclo demasiado corto (menor a 35s). Afecta negativamente la psicologia del conductor.")
    ((> duracion 150) "No recomendado: Ciclo demasiado largo (mayor a 150s). Provoca ansiedad e infracciones.")
    (t "Recomendado: El ciclo se encuentra dentro del rango psicofisico optimo optimo (35-150 segundos).")))

;; =============================================================================
;; REQUERIMIENTO 5: PLANIFICACION TEMPORAL
;; =============================================================================

;; =============================================================================
;; FUNCION: ciclos-por-tiempo
;; NATURALEZA: Pura (Realiza proyecciones temporales matematicas)
;; ESTRATEGIA DE CONTROL: Composicion de funciones aritmeticas (floor)
;; IMPACTO EN MEMORIA: No destructiva
;; =============================================================================
(defun ciclos-por-tiempo (minutos)
  (let ((segundos-totales (* minutos 60)))
    (floor segundos-totales (duracion-ciclo-total))))


;; =============================================================================
;; REQUERIMIENTO 6: INFORME DE DISTRIBUCION TEMPORAL
;; =============================================================================

;; =============================================================================
;; FUNCION: distribucion-porcentual-una-hora
;; NATURALEZA: Pura (Genera calculos de rendimiento porcentual)
;; ESTRATEGIA DE CONTROL: Evaluacion secuencial en bloques de enlace local (let*)
;; IMPACTO EN MEMORIA: No destructiva (Genera estructuras compuestas nuevas)
;; =============================================================================
(defun distribucion-porcentual-una-hora ()
  (let* ((total-ciclo (float (duracion-ciclo-total)))
         (pct-rojo (* (/ 90 total-ciclo) 100))
         (pct-verde (* (/ 120 total-ciclo) 100))
         (pct-amarillo (* (/ 6 total-ciclo) 100))
         (pct-intermitente (* (/ 9 total-ciclo) 100)))
    (list (list 'rojo pct-rojo)
          (list 'verde pct-verde)
          (list 'amarillo pct-amarillo)
          (list 'intermitente pct-intermitente))))

;; =============================================================================
;; REQUERIMIENTO 7: ASEGURAMIENTO DE LA CALIDAD (BATERIA DE PRUEBAS)
;; =============================================================================

;; =============================================================================
;; FUNCION: ejecutar-suite-pruebas
;; NATURALEZA: Impura (Despliega resultados de manera interactiva a traves de la terminal)
;; ESTRATEGIA DE CONTROL: Secuencial iterativa con funciones de orden superior encubiertas
;; IMPACTO EN MEMORIA: No destructiva
;; =============================================================================
(defun ejecutar-suite-pruebas ()
  (format t "=== DEMOSTRACION DE ASEGURAMIENTO DE LA CALIDAD ===~%~%")
  
  (format t "[Prueba Normal] Transicion de Rojo a Verde: ~A~%" (transicion 'en-rojo 'verde))
  (format t "[Prueba Alternativa] Transicion Intermitente a Verde: ~A~%" (transicion 'amarillo-intermitente 'verde))
  (format t "[Prueba Invalida] Transicion Incorrecta de Rojo a Amarillo: ~A~%" (transicion 'en-rojo 'amarillo))
  
  (format t "~%[Prueba Timer - T=0 (Rojo)]: ~A~%" (timer 0))
  (format t "[Prueba Timer - T=91 (Intermitente)]: ~A~%" (timer 91))
  (format t "[Prueba Timer - T=150 (Verde)]: ~A~%" (timer 150))
  
  (format t "~%[Prueba Auditoria (Quicklisp local-time)]~%")
  (registrar-cambio 1774881600 'en-rojo 'amarillo-intermitente)
  
  (let ((duracion (duracion-ciclo-total)))
    (format t "~%[Duracion Calculada]: ~A segundos~%" duracion)
    (format t "[Recomendacion]: ~A~%" (recomendacion-ciclo duracion)))
  
  (format t "~%[Planificacion] Ciclos completos en 15 minutos: ~A~%" (ciclos-por-tiempo 15))
  (format t "[Planificacion] Ciclos completos en 60 minutos: ~A~%" (ciclos-por-tiempo 60))
  
  (format t "~%[Distribucion Temporal Porcentual en 1 Hora]:~%~A~%" (distribucion-porcentual-una-hora))
  
  (informe '((1774881600 "ROJO -> AMARILLO-INTERMITENTE")
             (1774881603 "AMARILLO-INTERMITENTE -> VERDE")))
  (format t "~%[Persistencia] Archivo 'informe-ejecucion-semaforo.txt' escrito exitosamente.~%"))
