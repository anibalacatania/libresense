# Análisis completo en R (Shiny)

Las cuatro apps (Perfil Flash, Napping, CATA con aceptabilidad y Sorting) se publican **juntas, como una sola app** de
shinyapps.io (el plan gratuito permite pocas apps activas). `shiny/app.R` carga cada una en su propio entorno y muestra
la que corresponde según la dirección:

| Técnica | Dirección |
|---|---|
| Perfil Flash | https://acatania.shinyapps.io/libresense/?t=flash |
| Napping | https://acatania.shinyapps.io/libresense/?t=napping |
| CATA con aceptabilidad | https://acatania.shinyapps.io/libresense/?t=cata |
| Sorting | https://acatania.shinyapps.io/libresense/?t=sorting |

Sin `?t=` muestra una portada con los cuatro enlaces. Las direcciones están en `config.json` de LibreSense
(`shiny`, `shinyNapping`, `shinyCata`, `shinySorting`). Cada carpeta (`perfilflash/`, `napping/`, `cata/`, `sorting/`)
se puede seguir publicando sola si alguna vez se prefiere.

## Publicar o actualizar (desde RStudio)
```r
d <- path.expand("~/libresense_app"); z <- tempfile(fileext = ".zip")
download.file("https://github.com/anibalacatania/libresense/archive/refs/heads/main.zip", z, mode = "wb")
unzip(z, exdir = d, overwrite = TRUE)
rsconnect::deployApp(file.path(d, "libresense-main/shiny"), appName = "libresense", account = "acatania", launch.browser = TRUE)
```
Después se pueden archivar las apps sueltas anteriores (`perfilflash`, `napping`) en shinyapps.io: LibreSense ya no las usa.
