## -----------------------------------------------------------------------------
# Práctica Sesión 2: Diagnóstico e inferencia en GWR
## -----------------------------------------------------------------------------

## Paquetes --------------------------------------------------------------------
# Instalación (una sola vez, o correr source("setup.R"))
# pkgs <- c("GWmodel", "sf", "tidyverse", "patchwork", "agridat", "gstat", "here")
# install.packages("pak")
# pak::pak(pkgs)

library(GWmodel)   # ajuste de GWR
library(sf)        # manejo de datos espaciales
library(tidyverse) # manipulación de datos y ggplot2
library(patchwork) # composición de gráficos
library(agridat)   # dataset lasrosas.corn
library(gstat)     # variogramas
library(here)      # rutas relativas a la raíz del proyecto

## Repaso: datos y bandwidth ---------------------------------------------------
# PUEDE DEMORAR (~5-10 s): búsqueda del bandwidth óptimo
# Lectura y preparación datos
data(lasrosas.corn)
lr1999 <- lasrosas.corn |>
  filter(year == 1999) |>
  st_as_sf(coords = c("long", "lat")) |>
  st_set_crs(4326) |>
  st_transform(32720)

# Configuración y estimación bandwidth
approach <- "AICc"   # otra opción CV
kernel <- "bisquare" # otros: exponential, gaussian, etc
ad <- TRUE # FALSE para fixed
bw_bv <- bw.gwr(
  yield ~ bv, data = lr1999,
  approach = approach, kernel = kernel,
  adaptive = ad
)
bw_bv

## Repaso: ajuste GWR ----------------------------------------------------------
gwr_bv <- gwr.basic(
  yield ~ bv, data = lr1999,
  bw = bw_bv, kernel = kernel,
  adaptive = ad
)
gwr_bv

## Repaso: objeto SDF con coeficientes locales ---------------------------------
glimpse(gwr_bv$SDF)

## Repaso: mapas de pendiente y R² local ---------------------------------------
p_b1 <- ggplot(gwr_bv$SDF) +
  aes(color = bv) +
  geom_sf(size = 2, shape = "square") +
  scale_color_gradient2(low = "#440154", mid = "#21908C", high = "#FDE725", midpoint = 0) +
  labs(title = "Pendiente local de bv", color = NULL) +
  theme_void()

p_r2 <- ggplot(gwr_bv$SDF) +
  aes(color = Local_R2) +
  geom_sf(size = 2, shape = "square") +
  scale_color_viridis_c(limits = c(0, 1)) +
  labs(title = "Coeficiente de determinación local", color = NULL) +
  theme_void()

p_b1 / p_r2

## Explorando opciones de kernel -----------------------------------------------
# PUEDE DEMORAR (~20-60 s): 6 búsquedas de bandwidth + 6 ajustes
grid_kernel <- expand_grid(
  kernel = c("bisquare", "gaussian", "exponential"),
  adaptive = c(TRUE, FALSE)
) |>
  mutate(
    bw = map2_dbl(kernel, adaptive, \(k, a) {
      bw.gwr(
        yield ~ bv, data = lr1999,
        approach = "AICc", kernel = k, adaptive = a
      )
    }),
    AICc = pmap_dbl(list(kernel, adaptive, bw), \(k, a, b) {
      gwr.basic(
        yield ~ bv, data = lr1999,
        bw = b, kernel = k, adaptive = a
      ) |> pluck("GW.diagnostic", "AICc")
    })
  ) |>
  arrange(AICc)

grid_kernel |> mutate(bw = round(bw, 1), AICc = round(AICc, 1))

## Residuos locales ------------------------------------------------------------
ggplot(gwr_bv$SDF) +
  aes(color = residual) +
  geom_sf(size = 2, shape = "square") +
  scale_color_viridis_c() +
  labs(title = "Residuos locales del GWR", color = NULL) +
  theme_void()

## Variograma de residuos ------------------------------------------------------
v_resid <- variogram(residual ~ 1, data = gwr_bv$SDF)
plot(v_resid, main = "Semivarianza de los residuos del GWR")

## Bondad de ajuste: OLS vs. GWR -----------------------------------------------
expand_grid(
  Modelo = c("OLS (global)", "GWR"),
  Métrica = c("R²", "R² aj.")
) |>
  mutate(
    valor = c(
      summary(gwr_bv$lm)$r.squared,
      summary(gwr_bv$lm)$adj.r.squared,
      gwr_bv$GW.diagnostic$gw.R2,
      gwr_bv$GW.diagnostic$gwR2.adj
    )
  ) |>
  ggplot() +
  aes(x = Modelo, y = valor, fill = Métrica) +
  geom_col(position = "dodge") +
  lims(y = c(0, 1))

## Mapa de R² local ------------------------------------------------------------
ggplot(gwr_bv$SDF) +
  aes(color = Local_R2) +
  geom_sf(size = 2, shape = "square") +
  scale_color_viridis_c(limits = c(0, 1)) +
  labs(title = "R² local", color = NULL) +
  theme_void()

## Complejidad del modelo: ENP -------------------------------------------------
# Numero de parametros efectivos
gwr_bv$GW.diagnostic$enp

# Numéro de gl efectivos
gwr_bv$GW.diagnostic$edf

## AICc: OLS vs. GWR -----------------------------------------------------------
# AIC modelo global
k <- 2
m <- nrow(lr1999)
AIC(gwr_bv$lm) + (2 * k * (k + 1)) / (m - k - 1)

# AICc GWR
gwr_bv$GW.diagnostic$AICc

## Interpretación: coeficientes locales en SDF ---------------------------------
glimpse(gwr_bv$SDF)

## Distribución de coeficientes locales y R² -----------------------------------
gwr_bv$SDF |>
  st_drop_geometry() |>
  select(Intercept, bv, Local_R2) |>
  pivot_longer(everything()) |>
  ggplot() +
  aes(x = value) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "white"
  ) +
  facet_wrap(
    ~name, ncol = 1,
    scales = "free_x"
  ) +
  theme_minimal()

## Mapas de coeficientes locales -----------------------------------------------
p_base <- ggplot(gwr_bv$SDF) +
  geom_sf(
    size = 2,
    shape = "square"
  ) +
  scale_color_gradient2(midpoint = 0) +
  theme_void()

p_b0 <- p_base +
  aes(color = Intercept) +
  labs(
    title = "Intercept local",
    color = NULL
  )

p_b1 <- p_base +
  aes(color = bv) +
  labs(
    title = "Pendiente local de bv",
    color = NULL
  )

p_b0 / p_b1

## Significancia local ---------------------------------------------------------
# Valor crítico con los grados de libertad efectivos (edf = m - enp)
gwr_edf <- gwr_bv$GW.diagnostic$edf
t_crit <- qt(0.975, df = gwr_edf)
t_crit

ggplot(gwr_bv$SDF) +
  aes(color = abs(bv_TV) > t_crit) +
  geom_sf(size = 2, shape = "square") +
  scale_color_manual(
    values = c("#440154", "#FDE725"),
    name = "Significancia local del coeficiente de bv"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

## Problema 1: valores p ajustados por multiplicidad ---------------------------
# Calculo valores p con gwr.t.adjust()
gwr_bv_sp <- gwr_bv
gwr_bv_sp$SDF <- as(gwr_bv_sp$SDF, "Spatial")
res <- gwr.t.adjust(gwr_bv_sp)$results

# Fotheringham-Byrne, 2009
p_fb <- res$fb

# da Silva-Fotheringham, 2016
k <- gwr_bv$lm$rank - 1
enp <- gwr_bv$GW.diagnostic$enp
p_raw <- as_tibble(res$p)
p_dsf <- p_raw |>
  mutate(
    across(
      .cols = ends_with("_p"),
      .fns = \(x) x * (enp / k),
      .names = "{col}_dsf"
    ),
    .keep = "none"
)

# Agregar resultados a SDF
gwr_bv$SDF <- bind_cols(gwr_bv$SDF, p_raw, p_fb, p_dsf)

## Problema 1: significancia sin corregir --------------------------------------
ggplot(gwr_bv$SDF) +
  aes(color = bv_p < 0.05) +
  geom_sf(size = 2, shape = "square") +
  scale_color_manual(
    values = c("#440154", "#FDE725"),
    name = "Significancia local del coeficiente de bv"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

## Problema 1: corrección Fotheringham-Byrne (2009) ----------------------------
ggplot(gwr_bv$SDF) +
  aes(color = bv_p_fb < 0.05) +
  geom_sf(size = 2, shape = "square") +
  scale_color_manual(
    values = c("#440154", "#FDE725"),
    name = "Significancia local del coeficiente de bv"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

## Problema 1: corrección da Silva & Fotheringham (2016) -----------------------
ggplot(gwr_bv$SDF) +
  aes(color = bv_p_dsf < 0.05) +
  geom_sf(size = 2, shape = "square") +
  scale_color_manual(
    values = c("#440154", "#FDE725"),
    name = "Significancia local del coeficiente de bv"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

## Problema 2: test Monte Carlo de variabilidad espacial -----------------------
# COSTOSO (~1 h): 100 simulaciones, cada una recalibra el GWR.
# Descomentar para correr.
# set.seed(1)
# mc_bv <- gwr.montecarlo(
#   yield ~ bv, data = lr1999,
#   nsims = 100, # Lleva tiempo
#   kernel = kernel, adaptive = ad, bw = bw_bv
# )
# mc_bv

# Resultado precalculado con el bloque de arriba (guardado en cache/)
mc_bv <- readRDS(here("cache", "mc_bv_gwr.rds"))
mc_bv

## Problema 3: incertidumbre del bandwidth -------------------------------------
# Grilla bandwidth
grilla_bw <- tibble(
  bw = seq(20, 100, by = 10),
  AICc = map_dbl(bw, \(bw) {
    gwr.basic(
      yield ~ bv, data = lr1999,
      bw = bw, kernel = kernel, adaptive = ad
    ) |> pluck("GW.diagnostic", "AICc")
  }, .progress = TRUE)
)

# Plot AICc vs bandwidth
ggplot(grilla_bw) +
  aes(x = bw, y = AICc) +
  geom_line() +
  geom_vline(xintercept = bw_bv, linetype = "dashed") +
  labs(
    title = "Sensibilidad del AICc al bandwidth",
    x = "Bandwidth (vecinos)",
    y = "AICc"
  )

## Problema 4: multicolinealidad -----------------------------------------------
# PUEDE DEMORAR (~30-90 s): diagnóstico local en 1738 puntos
# Suponiendo que agregamos nitro como segundo predictor
res <- gwr.collin.diagno(
  yield ~ bv + nitro, data = lr1999,
  bw = bw_bv, kernel = kernel, adaptive = ad
)

ggplot(res$SDF) +
  aes(color = bv_VIF) +
  geom_sf(shape = "square", size = 2) +
  theme_void()
