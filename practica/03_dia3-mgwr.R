## -----------------------------------------------------------------------------
# Práctica Día 3: MGWR y extensiones de GWR
## -----------------------------------------------------------------------------

## Paquetes --------------------------------------------------------------------
# Instalación (una sola vez, o correr source("setup.R"))
# pkgs <- c("GWmodel", "sf", "tidyverse", "patchwork", "GGally", "gstat", "here")
# install.packages("pak")
# pak::pak(pkgs)

library(GWmodel)   # GWR, MGWR, GWGLM
library(sf)        # manejo de datos espaciales
library(tidyverse) # manipulación de datos y ggplot2
library(patchwork) # composición de mapas/gráficos
library(GGally)    # splom (ggpairs)
library(gstat)     # variogramas

## El dataset: Georgia ---------------------------------------------------------
data(Georgia)
data(GeorgiaCounties)

georgia_sf <- st_join(
  st_as_sf(Gedu.counties),
  st_as_sf(Gedu.df, coords = c("X", "Y"))
)

# Estandarizamos a N(0,1)
x_vars <- c("PctRural", "PctFB", "PctBlack", "PctPov")
y_var <- c("PctBach")
vars <- c(y_var, x_vars)

georgia_std <- georgia_sf |>
  mutate(across(all_of(vars), \(x) as.numeric(scale(x))))

georgia_sf |>
  st_drop_geometry() |>
  select(AreaKey, all_of(vars)) |>
  head()

## Exploración -----------------------------------------------------------------
georgia_sf |>
  st_drop_geometry() |>
  select(all_of(vars)) |>
  ggpairs(
    lower = list(
      continuous = wrap(
        "points", color = "steelblue", alpha = 0.5
      )
    )
  ) +
  theme(strip.text = element_text(size = 10))

## Ajustar GWR -----------------------------------------------------------------
bw_ga <- bw.gwr(
  PctBach ~ PctRural + PctFB + PctBlack + PctPov,
  data = georgia_std, approach = "AICc",
  kernel = "gaussian", adaptive = TRUE
)
bw_ga

gwr_ga <- gwr.basic(
  PctBach ~ PctRural + PctFB + PctBlack + PctPov,
  data = georgia_std, bw = bw_ga,
  kernel = "gaussian", adaptive = TRUE
)
gwr_ga

## GWR: coeficientes y R² local ------------------------------------------------
coeficientes <- c("Intercept", x_vars)

mapas <- map(coeficientes, \(v) {
  ggplot(gwr_ga$SDF) +
    aes(fill = .data[[v]]) +
    geom_sf(linewidth = 0.1) +
    scale_fill_gradient2(midpoint = 0) +
    labs(title = v, fill = NULL) +
    theme_void()
})

mapa_r2 <- ggplot(gwr_ga$SDF) +
  aes(fill = Local_R2) +
  geom_sf(linewidth = 0.1) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  labs(title = "R² local", fill = NULL) +
  theme_void()

wrap_plots(c(mapas, mapa_r2), ncol = 3)

## Ajustar MGWR con gwr.multiscale() -------------------------------------------
# bws0: bandwidths iniciales, uno por término (incluido el intercepto)
# criterion = "dCVR": criterio de convergencia
# approach = "AICc": criterio para optimizar cada bw_k en cada iteración
# verbose = TRUE imprime cada iteración del backfitting
mgwr_std <- gwr.multiscale(
  PctBach ~ PctRural + PctFB + PctBlack + PctPov,
  data = georgia_std,
  kernel = "gaussian",
  adaptive = TRUE,
  criterion = "dCVR",
  bws0 = rep(100, 5),
  approach = "AICc",
  verbose = FALSE
)

# Bandwidths finales, uno por término
mgwr_std$GW.arguments$bws

## MGWR: convergencia de los bandwidths ----------------------------------------
# Historia de bandwidths: una fila por iteración, una columna por término
# (elemento 5, sin nombre, de la lista que devuelve gwr.multiscale())
mgwr_std[[5]] |>
  as_tibble(.name_repair = \(x) coeficientes) |>
  mutate(iteracion = 1:n()) |>
  pivot_longer(-iteracion, names_to = "variable") |>
  ggplot() +
  aes(x = iteracion, y = value, color = variable) +
  geom_line()

## MGWR: diagnóstico general ---------------------------------------------------
as_tibble(mgwr_std$GW.diagnostic) |>
  mutate(across(everything(), \(x) round(x, 3)))

## MGWR: mapa de residuos ------------------------------------------------------
ggplot(mgwr_std$SDF) +
  aes(fill = residual) +
  geom_sf(linewidth = 0.1) +
  scale_fill_gradient2(midpoint = 0) +
  labs(title = "Residuos MGWR", fill = NULL) +
  theme_void()

## MGWR: variograma de residuos ------------------------------------------------
v_res <- variogram(residual ~ 1, data = mgwr_std$SDF)
plot(v_res)

## GWR vs. MGWR: PctFB ---------------------------------------------------------
lims_fb <- range(gwr_ga$SDF$PctFB, mgwr_std$SDF$PctFB)

p_gwr_fb <- ggplot(gwr_ga$SDF) +
  aes(fill = PctFB) +
  geom_sf(linewidth = 0.1) +
  scale_fill_gradient2(midpoint = 0, limits = lims_fb) +
  labs(
    title = "GWR", fill = NULL,
    subtitle = paste("bw =", bw_ga)
  ) +
  theme_void()

p_mgwr_fb <- ggplot(mgwr_std$SDF) +
  aes(fill = PctFB) +
  geom_sf(linewidth = 0.1) +
  scale_fill_gradient2(midpoint = 0, limits = lims_fb) +
  labs(
    title = "MGWR", fill = NULL,
    subtitle = paste(
      "bw =", mgwr_std$GW.arguments$bws[3]
    )
  ) +
  theme_void()

p_gwr_fb + p_mgwr_fb + plot_layout(guides = "collect")

## Problema 1: ENP y corrección por variable -----------------------------------
# ENP_j aproximado: GWR univariado sin intercepto, con el bw_j de MGWR
# Corrección: alpha_j = alpha / ENP_j
alpha <- 0.05
m <- nrow(georgia_std)

tests <- tibble(
  variable = vars[-1],
  bw = mgwr_std$GW.arguments$bws[-1]
) |>
  mutate(
    enp_j = map2_dbl(variable, bw, \(v, b) {
      gwr.basic(
        reformulate(c(v, "-1"), response = "PctBach"),
        data = georgia_std, bw = b,
        kernel = "gaussian", adaptive = TRUE
      ) |> pluck("GW.diagnostic", "enp")
    }),
    d_j = (m - enp_j) / (m - 1),
    alpha_j = alpha / enp_j
  )
tests

## Problema 1: significancia sin corregir vs. corregida ------------------------
edf <- mgwr_std$GW.diagnostic$edf

signif_long <- mgwr_std$SDF |>
  select(PctFB_TV, PctBlack_TV) |>
  pivot_longer(-geometry, names_to = "variable",
               values_to = "t", names_pattern = "(.*)_TV") |>
  left_join(tests, by = "variable") |>
  mutate(
    `Sin corregir` = abs(t) > qt(1 - alpha / 2, edf),
    Corregida = abs(t) > qt(1 - alpha_j / 2, edf)
  ) |>
  pivot_longer(c(`Sin corregir`, Corregida),
               names_to = "test", values_to = "signif")

ggplot(signif_long) +
  aes(fill = signif) +
  geom_sf(linewidth = 0.1) +
  scale_fill_manual(values = c("#440154", "#FDE725")) +
  facet_grid(variable ~ fct_rev(test)) +
  theme_void()

## Problema 3: incertidumbre del bandwidth, por variable -----------------------
# Aproximación univariada: un GWR por variable y por bandwidth de la grilla.
# La línea punteada marca el bandwidth encontrado por MGWR.
grilla_bw <- expand_grid(
  variable = vars[-1],
  bw = seq(10, 155, by = 15)
) |>
  mutate(AICc = map2_dbl(variable, bw, \(v, b) {
    gwr.basic(
      reformulate(v, response = "PctBach"),
      data = georgia_std, bw = b,
      kernel = "gaussian", adaptive = TRUE
    ) |> pluck("GW.diagnostic", "AICc")
  }, .progress = TRUE))

ggplot(grilla_bw) +
  aes(x = bw, y = AICc) +
  geom_line() +
  geom_vline(
    data = select(tests, variable, bw),
    aes(xintercept = bw), linetype = "dashed"
  ) +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Bandwidth (vecinos)", y = "AICc")

## Problema 4: multicolinealidad -----------------------------------------------
collin <- gwr.collin.diagno(
  PctBach ~ PctRural + PctFB + PctBlack + PctPov,
  data = georgia_std, bw = bw_ga,
  kernel = "gaussian", adaptive = TRUE
)
collin$SDF |>
  st_drop_geometry() |>
  select(ends_with("_VIF")) |>
  summary()

## GWGLM: código mínimo --------------------------------------------------------
# Ejemplo ilustrativo: dicotomizamos PctBach solo para mostrar la sintaxis
georgia_std <- georgia_std |>
  mutate(HighBach = as.numeric(PctBach > median(PctBach)))
dMat <- gw.dist(dp.locat = st_coordinates(st_centroid(georgia_std)))

bw_bin <- bw.ggwr(
  HighBach ~ PctRural + PctFB + PctBlack + PctPov,
  data = georgia_std, family = "binomial",
  approach = "AICc", kernel = "gaussian",
  adaptive = TRUE, dMat = dMat
)

ggwr_ga <- ggwr.basic(
  HighBach ~ PctRural + PctFB + PctBlack + PctPov,
  data = georgia_std, bw = bw_bin, family = "binomial",
  kernel = "gaussian", adaptive = TRUE, dMat = dMat
)

ggwr_ga$SDF |>
  st_drop_geometry() |>
  select(PctRural, PctFB, PctBlack, PctPov) |>
  summary()
