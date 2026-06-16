;; =============================================================================
;; PARTE 1: NUCLEO DEL SEMAFORO (MODULO DE TRANSICION Y TIMER)
;; =============================================================================
(ql:quickload :local-time)

;; =============================================================================
;; REQUERIMIENTO 1: ESTADOS DE TRANSICION
;; =============================================================================

;; =============================================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Sin efectos secundarios, no modifica el entormo)
;; ESTRATEGIA DE CONTROL: Funcion Predicado (Evalua condiciones logicas usando cond)
;; IMPACTO EN MEMORIA: No destructiva (Retorna una lista nueva sin alterar parametros)
;; =============================================================================
(defun transicion (color-actual cambiar-a)
  (cond
    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
     (list 'amarillo-intermitente "cambiar-a-amarillo-intermitente"))
    ((and (eq color-actual 'amarillo-intermitente) (eq cambiar-a 'verde))
     (list 'verde "cambiar-a-verde"))
    
    ((and (eq color-actual 'verde) (eq cambiar-a 'amarillo))
     (list 'amarillo-intermitente "cambiar-a-amarillo-intermitente"))
    ((and (eq color-actual 'amarillo-intermitente) (eq cambiar-a 'amarillo))
     (list 'en-amarillo "cambiar-a-en-amarillo"))
    
    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
     (list 'amarillo-intermitente "cambiar-a-amarillo-intermitente"))
    ((and (eq color-actual 'amarillo-intermitente) (eq cambiar-a 'rojo))
     (list 'en-rojo "cambiar-a-en-rojo"))
    
    (t (list color-actual 'accion-por-defecto))))

;; =============================================================================
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMATICO (CON ITERACION 2)
;; =============================================================================

;; =============================================================================
;; FUNCION: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA DE CONTROL: Funcion Predicado (Mapea rangos numericos a estados simbolicos)
;; IMPACTO EN MEMORIA: No destructiva
;; =============================================================================
(defun timer (timestamp)
  (let* ((duracion-ciclo (+ 90 3 120 3 6 3))
         (segundo-actual (mod timestamp duracion-ciclo)))
    (cond
      ((< segundo-actual 90) 'en-rojo)
      ((< segundo-actual 93) 'amarillo-intermitente)
      ((< segundo-actual 213) 'verde)
      ((< segundo-actual 216) 'amarillo-intermitente)
      ((< segundo-actual 222) 'en-amarillo)
      (t 'amarillo-intermitente))))
