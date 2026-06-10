(*
  =============================================================================
  FUNCIÓN: timer
  NATURALEZA: Pura (dado el mismo timestamp, siempre retorna el mismo color)
  ESTRATEGIA: Selección por casos (evalúa el rango del ciclo mediante match)
  IMPACTO: No destructiva (retorna un string sin modificar estructuras existentes)
  =============================================================================
*)

let timer timestamp =
    let ciclo = timestamp mod 216 in
    match ciclo with
    | _ when ciclo < 90  -> "en-rojo"
    | _ when ciclo < 210 -> "en-verde"
    | _ -> "en-amarillo"

(*
  =============================================================================
  FUNCIÓN: auditoria
  NATURALEZA: Impura (produce efectos secundarios: imprime en terminal)
  ESTRATEGIA: Selección por condición (compara colores mediante if)
  IMPACTO: No destructiva (no modifica estructuras en memoria)
  =============================================================================
*)

let   auditoria timestamp =
    let  color_anterior =   timer (timestamp - 1) in
    let  color_actual   =   timer timestamp in
    if  color_anterior <> color_actual then
        Printf.printf "Tiempo %d: cambio de %s a %s\n"  timestamp color_anterior color_actual 

(* Llamadas de ejemplo *)

let () =
    auditoria 89;
    auditoria 90;
    auditoria 210