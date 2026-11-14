# Napping · análisis completo (Shiny)

App Shiny que acompaña a la app **panel**. Analiza los estudios de Napping / mapeo proyectivo con FactoMineR y
SensoMineR, siguiendo a Dehlholm (2015, cap. 9 de *Novel Techniques in Sensory Characterization and Consumer Profiling*).
Se publica en **https://acatania.shinyapps.io/napping/** (dirección configurada en `config.json` de la app panel, `shinyNapping`).

Pestañas: datos, mapa de consenso (MFA con coordenadas sin estandarizar), elipses de confianza (Dehlholm et al. 2012 o
Cadoret y Husson 2013), palabras (mapa, descfreq, frecuencias, nube), panelistas (RV, uso de la hoja), hojas individuales
(originales y superpuestas al consenso), clasificación (HCPC), repetibilidad (duplicados, índice de Hopfer y Heymann,
sesiones), descargas (PNG, Excel, matriz, informe Word/HTML, código R) y ayuda.

La conexión con la app es la misma que la del Perfil Flash: la administración le pasa a esta app, y solo a ella, el token
de la sesión del administrador; también se puede usar subiendo la matriz o el formato largo (CSV).

## Publicar (desde RStudio)
```r
d <- path.expand("~/perfilflash_app"); z <- tempfile(fileext = ".zip")
download.file("https://github.com/anibalacatania/panel/archive/refs/heads/main.zip", z, mode = "wb")
unzip(z, exdir = d)
rsconnect::deployApp(file.path(d, "panel-main/shiny/napping"), appName = "napping", account = "acatania", launch.browser = TRUE)
```
