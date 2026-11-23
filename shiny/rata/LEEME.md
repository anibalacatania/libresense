# RATA · análisis completo (Shiny)

App Shiny que acompaña a la app **LibreSense**. Analiza los estudios RATA (y también ADC, escala 0–10) con R, FactoMineR y
SensoMineR, siguiendo a Meyners, Jaeger y Ares (2016).
Se publica en **https://acatania.shinyapps.io/rata/** (dirección configurada en `config.json` de la app LibreSense, `shinyRata`).

Pestañas: datos (panelistas y controles), descriptores (medias con letras de Tukey, % de citación, intensidad cuando se marcó,
ANOVA con panelista aleatorio, detalle por descriptor y gráfico producto × panelista), citación (Cochran y McNemar), mapa (PCA con
elipses por remuestreo de panelistas y clasificación), perfiles (araña), panel (acuerdo, discriminación, repetibilidad),
descargas (PNG, Excel, CSV, informe Word/HTML, código R) y ayuda. Si el estudio agrupa los descriptores (# Aroma, # Boca…),
se puede filtrar por modalidad.

La conexión con la app es la misma que la de las otras técnicas: la administración le pasa a esta app, y solo a ella, el token
de la sesión del administrador. También se puede usar subiendo el CSV de Resultados.

## Publicar (desde RStudio)
```r
d <- path.expand("~/libresense_app"); z <- tempfile(fileext = ".zip")
download.file("https://github.com/anibalacatania/libresense/archive/refs/heads/main.zip", z, mode = "wb")
unzip(z, exdir = d, overwrite = TRUE)
rsconnect::deployApp(file.path(d, "libresense-main/shiny/rata"), appName = "rata", account = "acatania", launch.browser = TRUE)
```
