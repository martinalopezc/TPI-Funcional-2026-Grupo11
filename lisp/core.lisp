;; =============================================================================

;; PARTE 2: MODULO DE AUDITORIA, LOGGING Y PERSISTENCIA DE ARCHIVOS
;; =============================================================================

;; =============================================================================
;; REQUERIMIENTO 3 Y EXTENSION 2: SISTEMA DE AUDITORIA Y LOGGING
;; =============================================================================

;; =============================================================================
;; FUNCION : registrar-cambio
;; NATURALEZA: Impura (Produce efectos secundarios al imprimir logs en la consola)
;; ESTRATEGIA DE CONTROL: Secuencial con evaluacion de macros externas (local-time)
;; IMPACTO EN MEMORIA: No destructiva
;; =============================================================================
(defun registrar-cambio (epoch color-anterior color-nuevo)
  (let* ((dt (local-time:universal-to-timestamp (+ epoch 2208988800)))
         (fecha-legible (local-time:format-timestring nil dt 
                          :format '((:year #\-) (:month #\-) (:day #\ ) (:hour #\:) (:min #\:) (:sec)))))
    (format t "Tiempo [~A]: la luz ha cambiado de ~A a ~A~%" 
            fecha-legible color-anterior color-nuevo)))

;; =============================================================================
;; FUNCION: informe
;; NATURALEZA: Impura (Efecto secundario de persistencia, crea y escribe un archivo externo)
;; ESTRATEGIA DE CONTROL: Recursiva Simple (Implementada mediante la funcion interna escribir-lineas)
;; IMPACTO EN MEMORIA: No destructiva (Recorre la estructura de datos sin modificarla)
;; =============================================================================
(defun informe (datos)
  (with-open-file (stream "informe-ejecucion-semaforo.txt" 
                          :direction :output 
                          :if-exists :superserve 
                          :if-does-not-exist :create)
    (format stream "Informe de Ejecucion del Sistema Semaforico~%")
    (format stream "========================================~%")
    (labels ((escribir-lineas (lista-datos)
               (cond
                 ((null lista-datos) nil)
                 (t (let* ((registro (car lista-datos))
                           (epoch (car registro))
                           (trans (cadr registro))
                           (dt (local-time:universal-to-timestamp (+ epoch 2208988800)))
                           (fecha (local-time:format-timestring nil dt 
                                    :format '((:year #\-) (:month #\-) (:day #\ ) (:hour #\:) (:min #\:) (:sec)))))
                      (format stream "~A - Transicion: ~A~%" fecha trans)
                      (escribir-lineas (cdr lista-datos)))))))
      (escribir-lineas datos))
    (format stream "--- Fin del Informe ---~%")))

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

