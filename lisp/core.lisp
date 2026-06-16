;; PARTE 1: NUCLEO DEL SEMAFORO (MODULO DE TRANSICION Y TIMER)
;; =============================================================================
(ql:quickload :local-time)

;; =============================================================================
;; REQUERIMIENTO 1: ESTADOS DE TRANSICION
;; =============================================================================

;; =============================================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (sin efectos secundarios, no modifica el entormo)
;; ESTRATEGIA DE CONTROL: Funcion predicado (evalua condiciones logicas usando cond)
;; IMPACTO EN MEMORIA: No destructiva (retorna una lista nueva sin alterar parametros)
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
;; NATURALEZA: Pura (dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA DE CONTROL: Funcion predicado (convierte rangos numericos a estados simbolicos)
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

;; =============================================================================
;; PARTE 2: MODULO DE AUDITORIA, LOGGING Y PERSISTENCIA DE ARCHIVOS
;; =============================================================================

;; =============================================================================
;; REQUERIMIENTO 3 Y EXTENSION 2: SISTEMA DE AUDITORIA Y LOGGING
;; =============================================================================

;; =============================================================================
;; FUNCION : registrar-cambio
;; NATURALEZA: Impura (produce efectos secundarios al imprimir logs en la consola)
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
;; NATURALEZA: Impura (efecto secundario de persistencia, crea y escribe un archivo externo)
;; ESTRATEGIA DE CONTROL: Recursiva simple (implementada mediante la funcion interna escribir-lineas)
;; IMPACTO EN MEMORIA: No destructiva (recorre la estructura de datos sin modificarla)
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

;; =============================================================================
;; PARTE 3: MODULO DE PLANIFICACION, ESTADISTICA Y BATERIA DE PRUEBAS
;; =============================================================================

;; =============================================================================
;; REQUERIMIENTO 4: ANALISIS DE CICLOS SEMAFORICOS
;; =============================================================================

;; =============================================================================
;; FUNCION: duracion-ciclo-total
;; NATURALEZA: Pura (retorna una constante calculada sim alterar variables globales)
;; ESTRATEGIA DE CONTROL: Evaluacion aritmetica directs
;; IMPACTO EN MEMORIA: No destructiva
;; =============================================================================
(defun duracion-ciclo-total ()
  (+ 90 3 120 3 6 3))

;; =============================================================================
;; FUNCION: recomendacion-ciclo
;; NATURALEZA: Pura (calcula cadenas de textp basadas estrictamente en la entrada numerica)
;; ESTRATEGIA DE CONTROL: Funcion predicado (estructura de analisis condicional)
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
;; NATURALEZA: Pura (realiza proyecciones temporales matematicas)
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
;; NATURALEZA: Pura (genera calculos de rendimiento porcentual)
;; ESTRATEGIA DE CONTROL: Evaluacion secuencial en bloques de enlace local (let*)
;; IMPACTO EN MEMORIA: No destructiva (genera estructuras compuestas nuevas)
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
;; NATURALEZA: Impura (despliega resultados de manera interactiva a traves de la terminal)
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
