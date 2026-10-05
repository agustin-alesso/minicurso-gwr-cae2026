# Prepara el ambiente para el curso GWR/MGWR.
# Funciona en RStudio y Positron. Correr una vez desde la raíz del proyecto:
# source("setup.R")

# 1. Instalador pak ---------------------------------------------------------

if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")

# Instalar paquetes para el curso
pkgs <- c(
  "GWmodel",   # GWR, MGWR, GWGLM
  "sf",        # datos espaciales
  "tidyverse", # manipulación de datos y ggplot2
  "patchwork", # composición de gráficos
  "GGally",    # splom (ggpairs)
  "agridat",   # dataset lasrosas.corn
  "gstat",     # variogramas
  "here",      # rutas relativas a la raíz del proyecto
  "knitr",     # renderizar los notebooks .qmd
  "rmarkdown"
)
pak::pak(pkgs)
gc()