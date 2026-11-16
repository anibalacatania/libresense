# Sorting · análisis completo (Shiny)

App Shiny que acompaña a la app **LibreSense**. Analiza los estudios de Sorting (agrupamiento libre) con R y FactoMineR,
siguiendo a Chollet, Valentin y Abdi (2014, cap. "Free sorting task" de *Novel Techniques in Sensory Characterization and
Consumer Profiling*), Cadoret, Lê y Pagès (2009), Abdi et al. (2007) y Cadoret y Husson (2013).
Se publica en **https://acatania.shinyapps.io/sorting/** (dirección configurada en `config.json` de la app LibreSense, `shinySorting`).

Pestañas: datos (panelistas y controles), mapa (MCA, DISTATIS o MDS, con palabras, grupos y elipses por bootstrap),
co-ocurrencias y clasificación, palabras (frecuencias, descfreq, palabras por grupo), panelistas (pesos DISTATIS, mapa,
RV), repetibilidad (duplicados, sesiones), descargas (PNG, Excel, matriz, informe Word/HTML, código R) y ayuda.

La conexión con la app es la misma que la de las otras técnicas: la administración le pasa a esta app, y solo a ella, el
token de la sesión del administrador. También se puede usar subiendo el formato largo o la matriz de grupos (CSV).

> Normalmente se publica **junto con las otras técnicas** como una sola app (`shiny/app.R`, ver `shiny/LEEME.md`):
> https://acatania.shinyapps.io/libresense/. Las instrucciones de abajo son para publicarla sola.

## Publicar (desde RStudio)
```r
d <- path.expand("~/libresense_app"); z <- tempfile(fileext = ".zip")
download.file("https://github.com/anibalacatania/libresense/archive/refs/heads/main.zip", z, mode = "wb")
unzip(z, exdir = d, overwrite = TRUE)
rsconnect::deployApp(file.path(d, "libresense-main/shiny/sorting"), appName = "sorting", account = "acatania", launch.browser = TRUE)
```
