;; ###################### SISTEMA DE SEMÁFOROS INTELIGENTES ######################

;; ========================================================
;; FUNCIÓN: Transicion
;; NATURALEZA: Pura (dados los mismos argumentos siempre devuelve el mismo resultado)
;; ESTRATEGIA: Selección por casos (evalua el par de estados mediante cond)
;; IMPACTO: No destructiva (No modifica estructuras existentes) 
;; ========================================================

(defun transicion (color-actual cambiar-a)
  (cond
    ((and (equal color-actual 'en-rojo) (equal cambiar-a 'verde))
     (list color-actual "cambiar-a-verde"))

    ((and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo))
     (list color-actual "cambiar-a-amarillo"))

    ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo))
     (list color-actual "cambiar-a-rojo"))

    (t (list color-actual 'accion-por-defecto))
  )
)

;; Se contempló refactorizar la construcción de la lista de retorno
;; en una función auxiliar para reducir la repetición de código,
;; aprovechando format para construir el string dinámicamente.
;; Se decidió mantener esta implementación por claridad y legibilidad.

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (dado el mismo timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Selección por casos (evalua el rango del ciclo mediante cond)
;; IMPACTO: No destructiva (retorna un símbolo sin modificar estructuras existentes)
;; ========================================================

(shadow 'timer) ;Para la llamada en sbcl

(defun timer (timestamp)
  (let ((ciclo (mod timestamp 216)))
    (cond
      ((< ciclo 90)  'en-rojo)
      ((< ciclo 210) 'en-verde)
      (t             'en-amarillo)
    )
  )
)

;; El ciclo comienza en Rojo en el timestamp 0 y se repite indefinidamente.
;; Esto corresponde a la fecha 1970-01-01 00:00:00 UTC pero
;; al implementarlo con MOD funciona tambien ciclicamente para fechas anteriores
;; Es decir, utilizando timestamps negativos.

;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Selección por condición (Compara colores mediante if)
;; IMPACTO: No destructiva (No modifica estructuras en memoria)
;; ========================================================

(defun auditoria (timestamp)
  (let ((color-anterior (timer (- timestamp 1)))
        (color-actual   (timer timestamp)))
    (if (not (equal color-anterior color-actual))
      (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" 
              timestamp
              color-anterior
              color-actual)
    )
  )
)

;; El parámetro timestamp(entero) representa segundos transcurridos
;; desde el inicio de la época Unix (01/01/1970 00:00:00 UTC) 
;; La función solo imprime información cuando ocurre un cambio
;; de estado en el semáforo.

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura(dado su entrada en segundos solo devuelve el mismo valor matematico, 
;;en este caso el valor de los segundos)
;; ESTRATEGIA: Recibe la entrada en segundos y la dividimos por 216 
;; IMPACTO: No destructiva
;; ========================================================


(defun duracion-ciclo (segundos)
  ;; Calcula la cantidad de ciclos semafóricos completos en un período dado en segundos.
  ;; Se divide por 216 y 'truncate' se encarga de descartar la fracción, 
  ;; dejando solo la cantidad de ciclos enteros completados.
  (truncate (/ segundos 216))
  )

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Selección por casos (cond)
;; IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo (duracion)
  (cond
    ((< duracion 35) (print "Se recomienda aumentar la duración del ciclo."))
    ((> duracion 150) (print "Se recomienda reducir la duración del ciclo."))
    (t (print "La duración del ciclo se encuentra dentro del rango recomendado."))
  )
)

;; Evalúa la duración del ciclo en segundos y e imprime recomendaciones basadas en rangos específicos.

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo:
;; NATURALEZA: Pura (dado el mismo valor en minutos, devuelve el mismo valor matematico)
;; ESTRATEGIA: trasformacion de valores y calculos matematicos directos  
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
  (truncate (/ (* minutos 60) 216))
)

;; Calcula la cantidad de ciclos semafóricos completos en un período dado en minutos.
;; Se multiplican los minutos por 60 para convertirlos a segundos.
;; Se divide por 216 y 'truncate' se encarga de descartar la fracción, 
;; dejando solo la cantidad de ciclos enteros completados.


;; ========================================================
;; FUNCIÓN: distribucion-hora
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Calculo aritmetico directo sobre los parametros de entrada,
;;             seguido de impresión formateada.
;; IMPACTO: No destructiva (no modifica estructuras en memoria)
;; ========================================================

(defun distribucion-hora (rojo verde amarillo)
  (let ((total (+ rojo verde amarillo)))
    (format t "=== Distribucion Temporal (1 hora) ===~%")
    (format t "Rojo:     ~,2F%~%" (* (/ (float rojo)     total) 100))
    (format t "Verde:    ~,2F%~%" (* (/ (float verde)    total) 100))
    (format t "Amarillo: ~,2F%~%" (* (/ (float amarillo) total) 100))
  )
)

;; Calcula e imprime el porcentaje de tiempo que ocupa cada color
;; en un ciclo semaforico, dado que se conocen las duraciones en segundos.
;; El total del ciclo es la suma de las tres duraciones recibidas.
;; Cada porcentaje se obtiene dividiendo la duracion del color por el total
;; y multiplicando por 100. Se usa float para obtener decimales en lugar de fracciones.


;; ###################### IMPLEMENTACION QUICKLISP ######################
;; Nota: No Ejecutar en REPL De Sublime con CLISP sino con SBCL
;; Por compatibilidad con QUICKLISP


;; Evita conflicto con el símbolo TIMER definido en SBCL (paquete SB-EXT).
;; Se crea un símbolo local para mantener el nombre exigido por la consigna.

;; RECORDAR DEFINIR TIMER PARA LLAMAR A auditoria-quicklisp

;; ========================================================
;; FUNCIÓN: auditoria-quicklisp
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Selección por condición (Compara colores mediante if y formatea timestamp con local-time)
;; IMPACTO: No destructiva (No modifica estructuras en memoria)
;; ========================================================

(shadow 'timer)

(defun timer (timestamp)
  (let ((ciclo (mod timestamp 216)))
    (cond
      ((< ciclo 90)  'en-rojo)
      ((< ciclo 210) 'en-verde)
      (t             'en-amarillo)
    )
  )
)

;Copia exacta de la funcion timer definida anteriormente


(ql:quickload "local-time")

(defun auditoria-quicklisp (timestamp)
  (let ((color-anterior (timer (- timestamp 1)))
        (color-actual   (timer timestamp)))
    (if (not (equal color-anterior color-actual))
      (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" 
              (local-time:format-timestring nil 
                (local-time:unix-to-timestamp timestamp)
                :format '((:year 4) "-" (:month 2) "-" (:day 2)
                          " " (:hour 2) ":" (:min 2) ":" (:sec 2))

              )
              color-anterior
              color-actual)
    )
  )
)

;; Cargo la librería y modifico sólo el argumento timestamp
;; Para formatearla.

;; ========================================================
;; CASOS DE PRUEBA
;; ASEGURAMIENTO DE LA CALIDAD 
;; ========================================================

;; TRANSICION
;; Normal:
;; (transicion 'en-rojo 'verde)
;; Alternativo:
;; (transicion 'en-verde 'amarillo)
;; Error:
;; (transicion 10 'verde)

;; TIMER
;; Normal:
;; (timer 50)
;; Alternativo:
;; (timer 200)
;; Error:
;; (timer "hola")

;; AUDITORIA
;; Normal:
;; (auditoria 90)
;; Alternativo:
;; (auditoria 50)
;; Error:
;; (auditoria "abc")

;; DURACION-CICLO
;; Normal:
;; (duracion-ciclo 432)
;; Alternativo:
;; (duracion-ciclo 216)
;; Error:
;; (duracion-ciclo "texto")

;; RECOMENDACION-CICLO
;; Normal:
;; (recomendacion-ciclo 20)
;; Alternativo:
;; (recomendacion-ciclo 100)
;; Error:
;; (recomendacion-ciclo "error")

;; CICLOS-POR-TIEMPO
;; Normal:
;; (ciclos-por-tiempo 15)
;; Alternativo:
;; (ciclos-por-tiempo 30)
;; Error:
;; (ciclos-por-tiempo "quince")

;; DISTRIBUCION-HORA
;; Normal:
;; (distribucion-hora 90 120 6)
;; Alternativo:
;; (distribucion-hora 30 30 30)
;; Error:
;; (distribucion-hora "rojo" 120 6)

;; ========================================================
;; Segunda Iteracion
;; ========================================================

;; ========================================================
;; Extension 1
;; ========================================================

;; ========================================================
;; FUNCIÓN: Transicion
;; NATURALEZA: Pura (dados los mismos argumentos siempre devuelve el mismo resultado)
;; ESTRATEGIA: Selección por casos (evalua el par de estados mediante cond)
;; IMPACTO: No destructiva (No modifica estructuras existentes) 
;; ========================================================

(defun transicion2 (color-actual cambiar-a)
  (cond
    ((and (equal color-actual 'en-rojo) (equal cambiar-a 'verde))
     (list color-actual "cambiar-a-verde"))

    ((and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo))
     (list color-actual "cambiar-a-amarillo"))

    ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo))
     (list color-actual "cambiar-a-rojo"))

    (t (list color-actual 'accion-por-defecto))
  )
)

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (dado el mismo timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Selección por casos (evalua el rango del ciclo mediante cond)
;; IMPACTO: No destructiva (retorna un símbolo sin modificar estructuras existentes)
;; ========================================================

(defun timer2 (timestamp)
  (let ((ciclo (mod timestamp 216)))
    (cond
      ((< ciclo 90)  'en-rojo)
      ((< ciclo 210) 'en-verde)
      (t             'en-amarillo)
    )
  )
)

;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Selección por condición (Compara colores mediante if)
;; IMPACTO: No destructiva (No modifica estructuras en memoria)
;; ========================================================

(defun auditoria2 (timestamp)
  (let ((color-anterior (timer2 (- timestamp 1)))
        (color-actual   (timer2 timestamp)))
    (if (not (equal color-anterior color-actual))
      (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" 
              timestamp
              color-anterior
              color-actual)
    )
  )
)

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura(dado su entrada en segundos solo devuelve el mismo valor matematico, 
;;en este caso el valor de los segundos)
;; ESTRATEGIA: Recibe la entrada en segundos y la dividimos por 216 
;; IMPACTO: No destructiva
;; ========================================================


(defun duracion-ciclo2 (segundos)
  (truncate (/ segundos 216))
  )

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Selección por casos (cond)
;; IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo2 (duracion)
  (cond
    ((< duracion 35) (print "Se recomienda aumentar la duración del ciclo."))
    ((> duracion 150) (print "Se recomienda reducir la duración del ciclo."))
    (t (print "La duración del ciclo se encuentra dentro del rango recomendado."))
  )
)

;; NO HAY CAMBIOS

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo:
;; NATURALEZA: Pura (dado el mismo valor en minutos, devuelve el mismo valor matematico)
;; ESTRATEGIA: trasformacion de valores y calculos matematicos directos  
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo2 (minutos)
  (truncate (/ (* minutos 60) 216))
)

;; ========================================================
;; FUNCIÓN: distribucion-hora
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Calculo aritmetico directo sobre los parametros de entrada,
;;             seguido de impresión formateada.
;; IMPACTO: No destructiva (no modifica estructuras en memoria)
;; ========================================================

(defun distribucion-hora2 (rojo verde amarillo)
  (let ((total (+ rojo verde amarillo)))
    (format t "=== Distribucion Temporal (1 hora) ===~%")
    (format t "Rojo:     ~,2F%~%" (* (/ (float rojo)     total) 100))
    (format t "Verde:    ~,2F%~%" (* (/ (float verde)    total) 100))
    (format t "Amarillo: ~,2F%~%" (* (/ (float amarillo) total) 100))
  )
)

;; ###################### IMPLEMENTACION QUICKLISP ######################
;; Nota: No Ejecutar en REPL De Sublime con CLISP sino con SBCL
;; Por compatibilidad con QUICKLISP

;; ========================================================
;; FUNCIÓN: auditoria-quicklisp
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Selección por condición (Compara colores mediante if y formatea timestamp con local-time)
;; IMPACTO: No destructiva (No modifica estructuras en memoria)
;; ========================================================

(ql:quickload "local-time")

(defun auditoria-quicklisp2 (timestamp)
  (let ((color-anterior (timer2 (- timestamp 1)))
        (color-actual   (timer2 timestamp)))
    (if (not (equal color-anterior color-actual))
      (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" 
              (local-time:format-timestring nil 
                (local-time:unix-to-timestamp timestamp)
                :format '((:year 4) "-" (:month 2) "-" (:day 2)
                          " " (:hour 2) ":" (:min 2) ":" (:sec 2))

              )
              color-anterior
              color-actual)
    )
  )
)

;; ========================================================
;; Extension 2 
;; ========================================================

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (produce efectos secundarios: escribe en archivo)
;; ESTRATEGIA: Selección por condición (escribe en archivo solo si hay transición mediante if)
;; IMPACTO: No destructiva en memoria, pero modifica el archivo informe-ejecucion-semaforo.txt
;; ========================================================

(defun informe (timestamp)
  (with-open-file (stream "informe-ejecucion-semaforo.txt"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (let ((color-anterior (timer (- timestamp 1)))
          (color-actual   (timer timestamp)))
      (if (not (equal color-anterior color-actual))
        (format stream "~A - Transicion: ~A -> ~A~%"
                (local-time:format-timestring nil
                  (local-time:unix-to-timestamp timestamp)
                  :format '((:year 4) "-" (:month 2) "-" (:day 2)
                            " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
                color-anterior
                color-actual)))))


;; ========================================================
;; Extra
;; ========================================================
;; Para implementar la persistencia en tiempo real se necesitaron
;; dos funciones auxiliares:
;;
;; - universal-a-unix: convierte el tiempo de Common Lisp a epoch Unix,
;;   necesario para que local-time pueda formatear la fecha correctamente.
;;
;; - ejecutar-sistema: llama a informe cada segundo durante el tiempo
;;   indicado usando recursividad de cola, evitando bucles imperativos
;;   y respetando las restricciones de diseño del proyecto.
;; ========================================================


;; ========================================================
; FUNCIÓN: universal-a-unix
;; NATURALEZA: Pura (Dado un tiempo universal, siempre retorna el mismo unix timestamp)
;; ESTRATEGIA: Aplicación aritmética directa (resta constante de conversión entre épocas)
;; IMPACTO: No destructiva (No modifica estructuras en memoria)
;; ========================================================

(defun universal-a-unix (tiempo-universal)
  (- tiempo-universal 2208988800))

;; 2208988800 es la diferencia en segundos entre el epoch de Common Lisp (1 de enero de 1900)
;; y el epoch Unix (1 de enero de 1970), equivalente a 70 años.

;; ========================================================
;; FUNCIÓN: ejecutar-sistema
;; NATURALEZA: Impura (produce efectos secundarios: escribe en archivo, espera con sleep)
;; ESTRATEGIA: Recursividad de cola (se llama a sí misma decrementando segundos-restantes)
;; IMPACTO: No destructiva en memoria, pero modifica el archivo informe-ejecucion-semaforo.txt
;; ========================================================

(defun ejecutar-sistema (segundos-restantes)
  (cond
    ((<= segundos-restantes 0) nil)
    (t
     (informe (universal-a-unix (get-universal-time)))
     (sleep 1)
     (ejecutar-sistema (- segundos-restantes 1))))) 

