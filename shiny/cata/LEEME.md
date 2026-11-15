# CATA con aceptabilidad · análisis completo (Shiny)

App Shiny que acompaña a la app **LibreSense**. Analiza los estudios CATA con aceptabilidad (y producto ideal, si se pidió)
con R y FactoMineR, siguiendo a Meyners y Castura (2014, cap. 11 de *Novel Techniques in Sensory Characterization and
Consumer Profiling*), Ares y Jaeger (2015, cap. 11 de *Rapid Sensory Profiling Techniques*), Meyners, Castura y Carr (2013) y Ares et al. (2014).
Se publica en **https://acatania.shinyapps.io/cata/** (dirección configurada en `config.json` de la app LibreSense, `shinyCata`).

Pestañas: datos (consumidores y controles), aceptabilidad (ANOVA, Tukey, distribución), términos (frecuencias, Q de Cochran,
McNemar de a pares), mapa (análisis de correspondencias con ideal y aceptabilidad suplementarios), penalidades (impacto de
cada término, penalidades respecto del ideal, diferencia con el ideal por producto), consumidores (mapa de preferencias
interno y segmentos), descargas (PNG, Excel, CSV, informe Word/HTML, código R) y ayuda.

La conexión con la app es la misma que la de Perfil Flash y Napping: la administración le pasa a esta app, y solo a ella,
el token de la sesión del administrador. También se puede usar subiendo el CSV de Resultados.

## Publicar (desde RStudio)
```r
d <- path.expand("~/libresense_app"); z <- tempfile(fileext = ".zip")
download.file("https://github.com/anibalacatania/libresense/archive/refs/heads/main.zip", z, mode = "wb")
unzip(z, exdir = d, overwrite = TRUE)
rsconnect::deployApp(file.path(d, "libresense-main/shiny/cata"), appName = "cata", account = "acatania", launch.browser = TRUE)
```
