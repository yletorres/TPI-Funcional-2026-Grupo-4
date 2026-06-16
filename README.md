![Image](https://github.com/user-attachments/assets/8ad4d870-508c-45e9-adb6-417659eb8480)
# Sistema de Semáforos Inteligentes

### Trabajo Práctico Integrador 2026 — Paradigmas y Lenguajes de Programación

**FaCENA – UNNE | Licenciatura en Sistemas de Información**

---

## Descripción

Este repositorio contiene la implementación de un **Sistema de Semáforos Inteligentes** desarrollado en **Common Lisp**, aplicando conceptos del paradigma funcional tales como funciones puras, recursión, composición de funciones e inmutabilidad de datos.

Además, incluye una implementación comparativa en **OCaml**, junto con el informe conceptual y la documentación requerida por la cátedra.

---

## Grupo 4

### Integrantes

| Integrante                     | DNI        |
| ------------------------------ | ---------- |
| Torres Yleana Beatriz          | 44.982.909 |
| Lagraña Balzaretti Juan Martín | 46.523.195 |
| Almirón José                   | 46.602.154 |
| López Nicolás Jonatan          | 40.509.009 |

---

## Videos

### 🎥 Video demostración del sistema

[(https://youtu.be/hh552Fff0yY?si=TJ0DU0AJLKby50Md)]

---

## Estructura del repositorio

```text
TPI-Funcional-2026-Grupo4/
├── lisp/
│   ├── core.lisp
├── comparativa/
│   └── solucion.ml
├── docs/
│   ├── INFORME.pdf
│   └── HONOR.md
└── README.md
```

---

## Requisitos para ejecutar el código

### Common Lisp

El proyecto utiliza funciones de la librería **local-time**, instalada mediante **Quicklisp**. Por compatibilidad, **se requiere ejecutar el código con SBCL** (Steel Bank Common Lisp), no con CLISP, ya que Quicklisp no es compatible con CLISP.

**Pasos sugeridos:**

1. Instalar SBCL.
2. Instalar Quicklisp siguiendo las instrucciones oficiales (ejecutar el instalador desde el REPL de SBCL).
3. Cargar el archivo `lisp/core.lisp` en el REPL de SBCL.
4. Las llamadas de ejemplo (Requerimiento 7) están comentadas al final del archivo, junto a cada función. Descomentar y ejecutar en el REPL para probar cada caso.

> Recomendamos usar **Visual Studio Code con la extensión Alive**, que provee un REPL conectado directamente a SBCL, facilitando la carga del archivo y la ejecución interactiva de las funciones.

### OCaml

El código de la Fase 3 puede probarse directamente en el playground oficial de OCaml, pegando el contenido de `comparativa/solucion.ml` y ejecutando con la opción "Run". Las llamadas de ejemplo están incluidas dentro del archivo para poder ver los resultados por consola.

---

## Informe

El informe técnico completo (fundamentos de diseño, justificación de librerías, bitácora de depuración, análisis comparativo con OCaml y conclusiones) se encuentra en `docs/INFORME.pdf`.

---

## Código de Honor

La declaración de Código de Honor requerida por la asignatura se encuentra en `docs/HONOR.md`.

---

## Materia

**Paradigmas y Lenguajes de Programación**
Licenciatura en Sistemas de Información
FaCENA – Universidad Nacional del Nordeste (UNNE)

Año 2026.
