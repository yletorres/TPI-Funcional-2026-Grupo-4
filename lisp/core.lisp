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

;Hardcodeado con las reglas del negocio actuales respetando la consigna.

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
;; NATURALEZA: Pura (A mismas entradas, misma salida)
;; ESTRATEGIA: Calculo Aritmetico 
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo (segundos-rojo segundos-verde segundos-amarillo)
  (+ segundos-rojo segundos-verde segundos-amarillo)
  )

;; Calcula la duración total de un ciclo semafórico completo, 
;; sumando los tiempos de las fases roja, verde y amarilla (en segundos)

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
;; ESTRATEGIA: Calculos Aritmeticos. 
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
  (truncate (/ (* minutos 60) 216))
)

;; Calcula la cantidad de ciclos completos en un período (minutos).
;; El 216 corresponde a las reglas de negocio actuales. La consigna solo
;; permite recibir 'minutos', por lo que no se puede generalizar con
;; duracion-ciclo; hubiésemos preferido evitar este hardcodeo para reutilizacion.

;; ========================================================
;; FUNCIÓN: repartir-resto
;; NATURALEZA: Pura (a misma entrada, misma salida)
;; ESTRATEGIA: Recursion de Cola
;; IMPACTO: No destructiva
;; ========================================================

(defun repartir-resto (resto duraciones)
  (cond
    ((null duraciones) nil)
    (t (let ((extra (min resto (car duraciones))))
         (cons extra
               (repartir-resto (- resto extra) (cdr duraciones)))
       ))
  )
)

;Funcion Auxiliar de distribucion-hora

;; ========================================================
;; FUNCIÓN: distribucion-hora
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Funcion de orden superior (Mapcar) y Calculos arimeticos
;; IMPACTO: No destructiva (no modifica estructuras en memoria)
;; ========================================================

(defun distribucion-hora (rojo verde amarillo)
  (let* ((duracion    (duracion-ciclo rojo verde amarillo))
         (ciclos      (truncate (/ 3600 duracion)))
         (resto       (- 3600 (* ciclos duracion)))
         (duraciones  (list rojo verde amarillo))
         (extras      (repartir-resto resto duraciones))

         (nombres     '("Rojo" "Verde" "Amarillo"))

         (totales     (mapcar (lambda (d e) (+ (* ciclos d) e)) duraciones extras)))
    (format t "=== Distribucion Temporal (1 hora) ===~%")
    (mapcar (lambda (nombre total)
              (format t "~A: ~,2F%~%" nombre (* (/ (float total) 3600) 100)))
            nombres totales)
  )
)


;; Calcula el % de tiempo en rojo, verde y amarillo durante 1 hora,
;; repartiendo secuencialmente el tiempo sobrante de los ciclos.

;; ###################### IMPLEMENTACION QUICKLISP ######################
;; Nota: No Ejecutar en REPL De Sublime con CLISP sino con SBCL
;; Por compatibilidad con QUICKLISP


;; SHADOW Evita conflicto con el símbolo TIMER definido en SBCL (paquete SB-EXT).
;; Se crea un símbolo local para mantener el nombre exigido por la consigna.

;; RECORDAR DEFINIR TIMER PARA LLAMAR A auditoria-quicklisp

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

;Copia exacta de la funcion timer definida anteriormente.
;Se repite aca para que auditoria-quicklisp funcione al copiar y pegar
;solo este bloque (sin depender del timer definido mas arriba).

;; ========================================================
;; FUNCIÓN: auditoria-quicklisp
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Selección por condición (Compara colores mediante if y formatea timestamp con local-time)
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
;; FUNCIÓN: Transicion2   
;; NATURALEZA: Pura (dados los mismos argumentos siempre devuelve el mismo resultado)
;; ESTRATEGIA: Selección por casos (evalua el par de estados mediante cond)
;; IMPACTO: No destructiva (No modifica estructuras existentes) 
;; ========================================================

  (defun transicion2 (color-actual cambiar-a)
  (cond

    ;; rojo a rojo-intermitente (aviso antes de pasar a verde)
    ((and (equal color-actual 'en-rojo)
          (equal cambiar-a 'en-rojo-intermitente))
     (list color-actual "cambiar-a-rojo-intermitente"))

    ;; rojo-intermitente a Verde
    ((and (equal color-actual 'en-rojo-intermitente)
          (equal cambiar-a 'verde))
     (list color-actual "cambiar-a-verde"))

    ;; verde a verde-intermitente (aviso antes de pasar a amarillo)
    ((and (equal color-actual 'en-verde)
          (equal cambiar-a 'en-verde-intermitente))
     (list color-actual "cambiar-a-verde-intermitente"))

    ;; verde-intermitente a amarillo
    ((and (equal color-actual 'en-verde-intermitente)
          (equal cambiar-a 'amarillo))
     (list color-actual "cambiar-a-amarillo"))

    ;; amarillo a amarillo-intermitente (aviso antes de pasar a rojo)
    ((and (equal color-actual 'en-amarillo)
          (equal cambiar-a 'en-amarillo-intermitente))
     (list color-actual "cambiar-a-amarillo-intermitente"))

    ;; amarillo-intermitente a rojo
    ((and (equal color-actual 'en-amarillo-intermitente)
          (equal cambiar-a 'rojo))
     (list color-actual "cambiar-a-rojo"))

    (t (list color-actual 'accion-por-defecto))
  )
)

;; ========================================================
;; FUNCIÓN: timer2
;; NATURALEZA: Pura (dado el mismo timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Selección por casos (evalua el rango del ciclo mediante cond)
;; IMPACTO: No destructiva (retorna un símbolo sin modificar estructuras existentes)
;; ========================================================

(defun timer2 (timestamp)
  (let ((ciclo (mod timestamp 225)))   
    (cond
      ((< ciclo 90)  'en-rojo)
      ((< ciclo 93)  'en-rojo-intermitente)
      ((< ciclo 213) 'en-verde)
      ((< ciclo 216) 'en-verde-intermitente)
      ((< ciclo 222) 'en-amarillo)
      (t             'en-amarillo-intermitente)
    )
  )
)

;; 216 pasa a 225 (3 Segundos mas por color)

;; ========================================================
;; FUNCIÓN: auditoria2
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

;; Sin cambios respecto a auditoria: solo se reemplaza timer por timer2.

;; ========================================================
;; FUNCIÓN: duracion-ciclo2
;; NATURALEZA: Pura(dado su entrada en segundos solo devuelve el mismo valor matematico)
;; ESTRATEGIA: Calculo Aritmetico
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo2 (segundos-rojo segundos-verde segundos-amarillo)
  (+ segundos-rojo segundos-verde segundos-amarillo)
  )

;; Sin cambios respecto a duracion-ciclo

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

;; Sin cambios respecto a recomendacion-ciclo

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo2
;; NATURALEZA: Pura (dado el mismo valor en minutos, devuelve el mismo valor matematico)
;; ESTRATEGIA: trasformacion de valores y calculos matematicos directos  
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo2 (minutos)
  (truncate (/ (* minutos 60) 225))
)

;; Calcula la cantidad de ciclos completos en un período (minutos).
;; El 225 corresponde a las reglas de negocio actuales con los estados
;; intermitentes (90+3+120+3+6+3). Misma limitación que ciclos-por-tiempo:
;; la consigna solo permite recibir 'minutos'.

;; ========================================================
;; FUNCIÓN: repartir-resto2
;; NATURALEZA: Pura (a misma entrada, misma salida)
;; ESTRATEGIA: Recursion de Cola
;; IMPACTO: No destructiva
;; ========================================================

(defun repartir-resto2 (resto duraciones)
  (cond
    ((null duraciones) nil)
    (t (let ((extra (min resto (car duraciones))))
         (cons extra
               (repartir-resto2 (- resto extra) (cdr duraciones)))
       ))
  )
)
;; Sin cambios respecto a repartir-resto: la lógica es genérica
;; (funciona para una lista de cualquier largo), se repite el
;; nombre con el sufijo 2 para mantener la convención de la
;; segunda iteración.

;; ========================================================
;; FUNCIÓN: distribucion-hora2
;; NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
;; ESTRATEGIA: Funcion de orden superior (Mapcar) y Calculos aritmeticos
;; IMPACTO: No destructiva (no modifica estructuras en memoria)
;; ========================================================
(defun distribucion-hora2 (rojo verde amarillo)
  (let* ((interm      3)  ;; regla de negocio actual: cada intermitente dura 3 seg
         (duracion    (duracion-ciclo2 (+ rojo interm) (+ verde interm) (+ amarillo interm)))
         (ciclos      (truncate (/ 3600 duracion)))
         (resto       (- 3600 (* ciclos duracion)))
         (duraciones  (list rojo interm verde interm amarillo interm))
         (extras      (repartir-resto2 resto duraciones))
         (nombres     '("Rojo" "Rojo intermitente" "Verde" "Verde intermitente"
                        "Amarillo" "Amarillo intermitente"))
         (totales     (mapcar (lambda (d e) (+ (* ciclos d) e)) duraciones extras)))
    (format t "=== Distribucion Temporal (1 hora) ===~%")
    (mapcar (lambda (nombre total)
              (format t "~A: ~,2F%~%" nombre (* (/ (float total) 3600) 100)))
            nombres totales)
  )
)

;; ###################### IMPLEMENTACION QUICKLISP ######################
;; Nota: No Ejecutar en REPL De Sublime con CLISP sino con SBCL
;; Por compatibilidad con QUICKLISP

;; ========================================================
;; FUNCIÓN: auditoria-quicklisp2
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
;; Sin cambios respecto a auditoria-quicklisp: solo se reemplaza timer por timer2.

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

