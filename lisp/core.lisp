;; =============================================================================
;; REQUERIMIENTO DE INFRAESTRUCTURA: CARGA DE DEPENDENCIAS (FASE 2)
;; =============================================================================
;; Cargamos la libreria externa local-time usando el gestor Quicklisp para manejar las fechas despues
(ql:quickload :local-time)

;; =============================================================================
;; REQUERIMIENTO 1: ESTADOS DE TRANSICION
;; =============================================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Mismo par de entradas produce la misma lista de salida)
;; ESTRATEGIA: Funcion Predicado / Condicional Cond
;; IMPACTO: No destructiva
;; =============================================================================
(defun transicion (color-actual cambiar-a)
  (cond
    ;; Caminos normales entre las 3 luces basicas (Esto es lo primero de la fase 1)
    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
     (list 'en-verde "cambiar-a-verde"))
    
    ((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
     (list 'en-amarillo "cambiar-a-amarillo"))
    
    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
     (list 'en-rojo "cambiar-a-rojo"))
    
    ;; Comportamiento por defecto ante transiciones inválidas o no permitidas
    (t (list color-actual 'accion-por-defecto))))

(ql:quickload :local-time)

;; =============================================================================
;; Requerimiento 1: estados de transición ( actualizado con extensión 1 )
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
;; Requerimiento 2: temporizador automático ( actualizado con extensión 1 )
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

;; =============================================================================
;; Requerimiento 3: sistema de auditoría
;; =============================================================================
(defun registrar-cambio (epoch color-anterior color-nuevo)
  (let* ((dt (local-time:universal-to-timestamp epoch))
         (fecha-legible (local-time:format-timestring nil dt 
                          :format '((:year #\-) (:month #\-) (:day #\ ) (:hour #\:) (:min #\:) (:sec)))))
    (format t "Tiempo [A]: la luz cambió de ~A a ~A%" fecha-legible color-anterior color-nuevo)))

;; Agregamos la iteración 2 y modificamos la transición y el timer para agregar los 3 segundos de amarillo intermitente en el ciclo como pide la extensión 1. 
;También agreguamos la función de auditoría usando la librería local-time para poder ver mejor la estampa de tiempo, pero cuando le paso un tiempo cualquiera me tira fechas raras en el año 2096. 
;Estamos viendo porque pasa eso.

;; Armando la base del requrimiento 1, definimos la estructura principal de la funcion transicion usando un cond 
; para validar los cambios de las luces del semaforo. Por ahora este es el camino basico de los 3 colores para ver 
; si responde bien el flujo y rebota las transiciones invalidas con la accion por defecto.
