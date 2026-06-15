;; =============================================================================
;; PARTE 2: MODULO DE AUDITORIA, LOGGING Y PERSISTENCIA DE ARCHIVOS
;; =============================================================================
(ql:quickload :local-time)

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
