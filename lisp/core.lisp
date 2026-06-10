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



;; Armando la base del requrimiento 1, definimos la estructura principal de la funcion transicion usando un cond 
; para validar los cambios de las luces del semaforo. Por ahora este es el camino basico de los 3 colores para ver 
; si responde bien el flujo y rebota las transiciones invalidas con la accion por defecto.
