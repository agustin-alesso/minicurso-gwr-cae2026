## -----------------------------------------------------------------------------
# Práctica Sesión 1: Primeros pasos con GWR
## -----------------------------------------------------------------------------

## Paquetes --------------------------------------------------------------------
# Instalación (una sola vez, o correr source("setup.R"))
# pkgs <- c("GWmodel", "sf", "tidyverse", "patchwork", "GGally", "agridat", "here")
# install.packages("pak")
# pak::pak(pkgs)

library(GWmodel)   # ajuste de GWR
library(sf)        # manejo de datos espaciales
library(tidyverse) # manipulación de datos y ggplot2
library(agridat)   # dataset lasrosas.corn

## Cargar y explorar los datos -------------------------------------------------
lr1999 <- lasrosas.corn |>
  filter(year == 1999) |>
  st_as_sf(coords = c("long", "lat")) |>
  st_set_crs(4326) |>
  st_transform(32720)

glimpse(lr1999)

## Mapa de rendimiento ---------------------------------------------------------
ggplot(lr1999) +
  aes(color = yield) +
  geom_sf(size = 2, shape = "square") +
  scale_color_viridis_c() +
  labs(title = "Yield (qq/ha)", color = NULL) +
  theme_void()

## Seleccionar el bandwidth ----------------------------------------------------
# PUEDE DEMORAR (~5-10 s): búsqueda del bandwidth óptimo
# Seleccionar criterio
approach <- "AICc"   # otra opción CV
kernel <- "bisquare" # otros: exponential, gaussian, etc
ad <- TRUE # FALSE para fixed

bw_bv <- bw.gwr(
  yield ~ bv,
  data = lr1999,
  approach = approach,
  kernel = kernel,
  adaptive = ad
)

# El bandwidth optimizado
bw_bv

## Ajustar el modelo GWR -------------------------------------------------------
gwr_bv <- gwr.basic(
  yield ~ bv,
  data = lr1999,
  bw = bw_bv,
  kernel = kernel,
  adaptive = ad
)
gwr_bv

## Explorando el objeto SDF ----------------------------------------------------
glimpse(gwr_bv$SDF)

# Columnas de SDF:
#   Intercept, bv: coeficientes locales
#   y, yhat: valor observado y ajustado localmente
#   residual: y - yhat
#   CV_Score: residuo de validación cruzada
#   Stud_residual: residuo estudentizado
#   *_SE: error estándar de cada coeficiente local
#   *_TV: estadístico t de cada coeficiente local
#   Local_R2: bondad de ajuste local

## Distribución de la pendiente local ------------------------------------------
ggplot(st_drop_geometry(gwr_bv$SDF)) +
  aes(x = bv) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  geom_vline(xintercept = coef(gwr_bv$lm)["bv"], color = "firebrick", linetype = "dashed", linewidth = 1) +
  labs(x = "Pendiente local de bv", y = "Frecuencia")

## Mapa de R² local ------------------------------------------------------------
ggplot(gwr_bv$SDF) +
  aes(color = Local_R2) +
  geom_sf(size = 2, shape = "square") +
  scale_color_gradient2(midpoint = 0.5, limits = c(0, 1)) +
  labs(title = "R² local", color = NULL) +
  theme_void()


## Mapa coeficiente `bv` -------------------------------------------------------
# Completar