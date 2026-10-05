# Más allá de OLS: modelando relaciones espacialmente variables con GWR

Material de práctica del minicurso de GWR y MGWR en R, dictado en el II Congreso Argentino de Estadística.

Las clases (diapositivas) están publicadas en https://agustin-alesso.github.io/minicurso-gwr-cae2026/, junto con los links a los entornos en Posit Cloud (Positron y RStudio).

Este repositorio contiene lo necesario para seguir la práctica en tu propia computadora.

## Contenido

- `practica/`: un script por día y su notebook equivalente.
  - `00_setup.R`: instala los paquetes del curso.
  - `01_dia1-gwr`, `02_dia2-diagnostico`, `03_dia3-mgwr`: archivos `.R` (script plano organizado en secciones) y `.qmd` (notebook) con el mismo código.
- `cache/`: resultados precalculados de los tests de Monte Carlo, que tardan más de una hora en correr. Los scripts los leen con `readRDS()`. El código que los genera está comentado en los scripts de práctica.

## Cómo empezar

1. Clonar o descargar el repositorio (`Code > Download ZIP` en GitHub).
2. Abrir la carpeta raíz como proyecto en RStudio o Positron. El archivo `.here` marca la raíz para que `here::here()` resuelva las rutas.
3. Instalar los paquetes, una sola vez:

   ```r
   source("practica/00_setup.R")
   ```

   La primera instalación puede tardar varios minutos.
4. Abrir el script del día y correrlo línea por línea (`Ctrl+Enter`). En los notebooks `.qmd`, chunk por chunk (`Ctrl+Enter`) o completo (`Ctrl+Shift+K`).

Los bloques marcados `# PUEDE DEMORAR` tardan hasta 1 o 2 minutos. Los marcados `# COSTOSO` están desactivados y en su lugar se leen los resultados de `cache/`.
