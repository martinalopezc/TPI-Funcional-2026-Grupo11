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


;; =============================================================================
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMATICO
;; =============================================================================
(defun timer (timestamp)
  (let* ((duracion-ciclo (+ 90 120 6))
         (segundo-actual (mod timestamp duracion-ciclo)))
    (cond
      ((< segundo-actual 90) 'en-rojo)
      ((< segundo-actual 210) 'en-verde)
      (t 'en-amarillo))))

; Agregamos el requerimiento 2 que es la funcion del timer, sacamos las cuentas de cuanto duraria el ciclo base de las tres
; luces que se armo antes y nos dio 216 segundos. Usamos la funcion mod para calcular el segundo exacto segun el tiempo que
; reciba como parametro.




;; =============================================================================
;; REQUERIMIENTO 4, 5 Y 6: ANALISIS Y PLANIFICACION
;; =============================================================================
(defun duracion-ciclo-total ()
  (+ 90 120 6))

(defun recomendacion-ciclo (duracion)
  (cond
    ((< duracion 35) "No recomendado: muy corto")
    ((> duracion 150) "No recomendado: muy largo")
    (t "Recomendado: rango optimo")))

(defun ciclos-por-tiempo (minutos)
  (floor (* minutes 60) (duracion-ciclo-total)))

(defun distribucion-porcentual-una-hora ()
  (let ((total (float (duracion-ciclo-total))))
    (list (list 'rojo (* (/ 90 total) 100))
          (list 'verde (* (/ 120 total) 100))
          (list 'amarillo (* (/ 6 total) 100)))))

;; =============================================================================
;; REQUERIMIENTO 7: PRUEBAS
;; =============================================================================
(defun probar-todo ()
  (let ((mi-funcion 'timer))
    (format t "Resultado: ~A~%" (mi-funcion 10))))


; Ya agregue las funciones de recomendacion, la cantidad de ciclos de tiempo y los porcentajes de cada luz
; en una hora. Agregue una funcion chica abajo de todo para probar si el timer responde usando una variable local
; pero me da error, dice "la funcion MI-FUNCION no esta definida". Pero no entiendo porque no me deja llamarla si
; la defini, lo estoy revisando.





