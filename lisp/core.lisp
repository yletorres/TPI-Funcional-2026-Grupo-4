; ########### SISTEMA DE SEMÁFOROS INTELIGENTES ###########

;; ========================================================
;; FUNCIÓN: Transicion
;; NATURALEZA: Pura (dados los mismos argumntos siempre devuelve el mismo resultado)
;; ESTRATEGIA: Selección por casos (evalua el par de estados mediante cond)
;; IMPACTO: No destructiva (construye y retorna una nueva lista sin modificar estructuras existentes) 
;; ========================================================

(defun transicion (color-actual cambiar-a)
  (cond
    ((and (equal color-actual 'en-rojo)     (equal cambiar-a 'verde))
     (list color-actual "cambiar-a-verde"))

    ((and (equal color-actual 'en-verde)    (equal cambiar-a 'amarillo))
     (list color-actual "cambiar-a-amarillo"))

    ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo))
     (list color-actual "cambiar-a-rojo"))

    (t
     (list color-actual 'accion-por-defecto)))
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
      (t             'en-amarillo)))
)

; El ciclo comienza en el timestamp 0 y se repite indefinidamente.

;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo:
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================

;; ========================================================
;; FUNCIÓN: distribucion-hora
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================

;; ========================================================
;; FUNCIÓN: distribucion-hora
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================
