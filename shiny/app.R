# =====================================================================================
# LibreSense · análisis completo en R (todas las técnicas en UNA sola app de shinyapps.io)
#
# El plan gratuito de shinyapps.io permite pocas apps activas: esta app junta las cuatro
# (Perfil Flash, Napping, CATA y Sorting) y elige cuál mostrar según la dirección:
#   https://acatania.shinyapps.io/libresense/?t=flash
#   https://acatania.shinyapps.io/libresense/?t=napping
#   https://acatania.shinyapps.io/libresense/?t=cata
#   https://acatania.shinyapps.io/libresense/?t=sorting
# Cada técnica se carga en su propio entorno (sus funciones no se mezclan). Las carpetas
# perfilflash/, napping/, cata/ y sorting/ también se pueden seguir publicando por separado.
#
# Publicar (desde RStudio), con la carpeta "shiny" completa:
#   rsconnect::deployApp("<ruta>/libresense-main/shiny", appName = "libresense", account = "acatania")
# =====================================================================================
library(shiny)
library(bslib)

TECNICAS <- c(flash = "perfilflash", napping = "napping", cata = "cata", sorting = "sorting")
NOMBRES <- c(flash = "Perfil Flash", napping = "Napping", cata = "CATA con aceptabilidad", sorting = "Sorting")

cargar_app <- function(carpeta) {
  env <- new.env(parent = globalenv())
  env$APP_DIR <- carpeta
  eval(parse(file.path(carpeta, "app.R"), encoding = "UTF-8"), envir = env)
  env
}
APPS <- lapply(TECNICAS, cargar_app)

tecnica_de <- function(qs) {
  t <- parseQueryString(if (is.null(qs)) "" else qs)$t
  if (is.null(t) || !t %in% names(TECNICAS)) NULL else t
}

portada <- page_fillable(
  theme = bs_theme(version = 5, primary = "#7c3aed", base_font = font_collection("Inter", "system-ui", "sans-serif")),
  div(class = "container py-5", style = "max-width: 760px",
    h2("LibreSense · análisis completo en R"),
    p(class = "text-muted", "Esta app se abre desde la administración de LibreSense, con el botón “Análisis completo en R” de cada estudio: así recibe los datos del estudio automáticamente."),
    p("También se puede usar subiendo el CSV de resultados que descarga la app. Elegí la técnica:"),
    tags$ul(lapply(names(TECNICAS), function(t) tags$li(tags$a(href = paste0("?t=", t), NOMBRES[[t]])))),
    p(class = "small text-muted mt-4", "LibreSense · ", tags$a(href = "https://anibalacatania.github.io/libresense/", "anibalacatania.github.io/libresense"))))

ui <- function(req) {
  t <- tecnica_de(req$QUERY_STRING)
  if (is.null(t)) portada else APPS[[t]]$ui
}

server <- function(input, output, session) {
  t <- tecnica_de(isolate(session$clientData$url_search))
  if (!is.null(t)) APPS[[t]]$server(input, output, session)
}

shinyApp(ui, server)
