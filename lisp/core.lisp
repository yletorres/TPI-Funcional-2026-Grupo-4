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
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo:
;; NATURALEZA: Pura (dado el mismo valor en minutos, devuelve el mismo valor matematico)
;; ESTRATEGIA:trasformacion de valores y calculos matematicos directos  
;; IMPACTO: No destructiva
;; ========================================================
(defun ciclos-por-tiempo (minutos)
  ;;Calcula la cantidad de ciclos semafóricos completos en un período dado en minutos.
  ;; Se multiplican los minutos por 60 para convertirlos a segundos.
  ;; Se divide por 216 y 'truncate' se encarga de descartar la fracción, 
  ;; dejando solo la cantidad de ciclos enteros completados.
  (truncate (/ (* minutos 60) 216))
  )


;; ========================================================
;; FUNCIÓN: distribucion-hora
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================

(defun distribucion-hora (rojo verde amarillo)
  (Let (ciclo (+ rojo amarillo verde))
    ()

  )
)


;; ###################### IMPLEMENTACION QUICKLISP ######################
;; Nota: No Ejecutar en REPL De Sublime con CLISP sino con SBCL
;; Por compatibilidad con QUICKLISP


;; Evita conflicto con el símbolo TIMER definido en SBCL (paquete SB-EXT).
;; Se crea un símbolo local para mantener el nombre exigido por la consigna.
(shadow 'timer)

;; Copia exacta de la funcion timer definida anteriormente.
(defun timer (timestamp)
  (let ((ciclo (mod timestamp 216)))
    (cond
      ((< ciclo 90)  'en-rojo)
      ((< ciclo 210) 'en-verde)
      (t             'en-amarillo)))
)

;; ========================================================
;; FUNCIÓN: auditoria-quicklisp
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: No destructiva (No modifica estructuras en memoria)
;; IMPACTO: No destructiva (No modifica estructuras en memoria)
;; ========================================================

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