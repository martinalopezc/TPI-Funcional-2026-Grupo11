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

