# =====================================================================================
# Sorting (agrupamiento libre) · Análisis completo
# App Shiny que acompaña a la app "LibreSense" (https://anibalacatania.github.io/libresense/).
# Se abre desde la administración de un estudio de Sorting ("Análisis completo en R"),
# o se usa sola subiendo el formato largo o la matriz de grupos que descarga la app.
#
# Métodos: Chollet, Valentin y Abdi (2014), cap. "Free sorting task" de Varela y Ares (eds.), Novel Techniques in
# Sensory Characterization and Consumer Profiling; Cadoret, Lê y Pagès (2009); Abdi et al. (2007); Cadoret y Husson (2013).
# =====================================================================================
library(shiny)
library(bslib)
library(ggplot2)
library(FactoMineR)
library(DT)
library(httr2)
requireNamespace("jsonlite")   # (sin library(): su validate() taparía al de shiny)
library(writexl)
library(ggrepel)
library(rmarkdown)
library(htmltools)

# carpeta de esta app: "." si se publica sola; la fija la app combinada (shiny/app.R) si se publica todo junto
APP_DIR <- if (exists("APP_DIR", inherits = FALSE)) APP_DIR else "."
ruta <- function(...) file.path(APP_DIR, ...)
for (f in list.files(ruta("R"), pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8", local = TRUE)

origenes_extra <- strsplit(Sys.getenv("PF_ORIGENES_EXTRA", ""), ",")[[1]]
origenes_extra <- origenes_extra[nzchar(origenes_extra)]
tema_app <- bs_theme(version = 5, primary = "#7c3aed", secondary = "#db2777", base_font = font_collection("Inter", "system-ui", "sans-serif"))
ayuda <- function(...) tags$p(class = "text-muted small mb-2", ...)
tarjeta <- function(titulo, ..., descargar = NULL) card(full_screen = TRUE,
  card_header(class = "d-flex justify-content-between align-items-center", titulo, descargar), card_body(...))
bajar <- function(id) downloadButton(id, "PNG", class = "btn-sm btn-outline-secondary")
tabla <- function(x, n = 25) datatable(x, rownames = FALSE, options = list(pageLength = n, dom = if (nrow(x) > n) "tp" else "t", scrollX = TRUE))
`%||%` <- function(a, b) if (is.null(a)) b else a

ui <- page_navbar(
  title = tags$b("Sorting"), window_title = "Sorting · análisis completo", id = "nav", theme = tema_app, fillable = FALSE,
  header = tagList(
    tags$script(HTML(sprintf("window.PF_ORIGENES_EXTRA = %s;", jsonlite::toJSON(origenes_extra)))),
    tags$script(src = "conexion.js"),
    tags$style(HTML(".shiny-output-error-validation{color:#6b7280;font-size:1rem;padding:1rem}"))),
  sidebar = sidebar(width = 290,
    uiOutput("estado"),
    uiOutput("selector_sesiones"),
    radioButtons("metodo", "Método del mapa", c("Correspondencias múltiples (MCA)" = "mca", "DISTATIS" = "distatis", "MDS de co-ocurrencias" = "mds")),
    selectInput("dims", "Dimensiones", c("1 y 2" = "1,2", "1 y 3" = "1,3", "2 y 3" = "2,3")),
    sliderInput("k", "Grupos de muestras (clasificación)", 2, 8, 3, step = 1),
    hr(),
    fileInput("archivo", "…o subí un archivo de la app", accept = c(".csv", ".txt"), buttonLabel = "Elegir", placeholder = "Formato largo o matriz (CSV)"),
    ayuda("En la app: Resultados → “Formato largo” o “Matriz de grupos”.")),

  nav_panel("Datos", icon = icon("table"),
    uiOutput("cajas"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Panelistas", DTOutput("tabla_pan")),
      tarjeta("Controles", uiOutput("controles"), plotOutput("p_ngrupos", height = "260px")))),

  nav_panel("Mapa", icon = icon("map"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Mapa de las muestras", plotOutput("p_mapa", height = "640px"), descargar = bajar("d_mapa")),
      tarjeta("Opciones",
        checkboxInput("ver_pal", "Mostrar las palabras", TRUE),
        sliderInput("minpal", "Palabras usadas al menos", 1, 10, 2, step = 1),
        checkboxInput("ver_grupos", "Colorear por grupo (clasificación)", TRUE),
        hr(),
        sliderInput("nboot", "Elipses: paneles simulados", 50, 500, 200, step = 50),
        actionButton("calc_boot", "Calcular elipses de confianza", class = "btn-primary w-100"),
        ayuda("Bootstrap de panelistas (Cadoret y Husson, 2013): se arman paneles virtuales sorteando panelistas, se rehace el mapa y se lleva al original. Dos muestras con elipses que no se superponen fueron diferenciadas por el panel."),
        hr(), h6("Inercia por dimensión"), tableOutput("t_inercia"))),
    tarjeta("Coordenadas", DTOutput("t_coord"))),

  nav_panel("Co-ocurrencias", icon = icon("table-cells"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Matriz de co-ocurrencias", plotOutput("p_cooc", height = "620px"), descargar = bajar("d_cooc")),
      tarjeta("Clasificación de las muestras", plotOutput("p_arbol", height = "420px"), descargar = bajar("d_arbol"),
        ayuda("Clasificación jerárquica (Ward) sobre las primeras dimensiones del mapa. La cantidad de grupos se elige a la izquierda."),
        tableOutput("t_grupos")))),

  nav_panel("Palabras", icon = icon("comment-dots"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Palabras más usadas", plotOutput("p_terminos", height = "520px"), descargar = bajar("d_terminos")),
      tarjeta("Palabras que caracterizan a cada muestra",
        ayuda("FactoMineR::descfreq: palabras usadas significativamente más (v.test > 0) o menos (v.test < 0) para esa muestra que para el resto (p < 0,05)."),
        DTOutput("t_descfreq"))),
    tarjeta("Palabras por grupo de muestras", ayuda("Palabras más usadas para las muestras de cada grupo de la clasificación."), DTOutput("t_pal_grupos")),
    tarjeta("Tabla de frecuencias (palabras × muestras)", DTOutput("t_pal"))),

  nav_panel("Panelistas", icon = icon("users"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Peso de cada panelista en el compromiso (DISTATIS)", ayuda("Los panelistas que coinciden más con el resto pesan más en el mapa DISTATIS."),
        plotOutput("p_alfa", height = "460px"), descargar = bajar("d_alfa")),
      tarjeta("Mapa de los panelistas", radioButtons("tipo_pan", NULL, c("DISTATIS (matriz RV)" = "distatis", "MCA (razones de correlación)" = "mca"), inline = TRUE),
        plotOutput("p_panelistas", height = "420px"), descargar = bajar("d_panelistas"))),
    tarjeta("Parecido entre panelistas", plotOutput("p_rv", height = "640px"), descargar = bajar("d_rv"))),

  nav_panel("Repetibilidad", icon = icon("repeat"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Duplicados", uiOutput("u_dup")),
      tarjeta("Sesiones (repeticiones)", uiOutput("u_ses")))),

  nav_panel("Descargas", icon = icon("download"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Tablas", p("Excel con coordenadas, co-ocurrencias, grupos, panelistas, palabras y los datos."),
        downloadButton("d_excel", "Descargar Excel", class = "btn-primary"), br(), br(), downloadButton("d_matriz", "Matriz de grupos (CSV, para R)")),
      tarjeta("Informe", p("Informe con los gráficos y tablas, con las opciones elegidas en el panel izquierdo."),
        radioButtons("formato", NULL, c("Word (.docx)" = "word", "Página web (.html)" = "html"), inline = TRUE),
        downloadButton("d_informe", "Generar informe", class = "btn-primary"))),
    tarjeta("Código R equivalente", verbatimTextOutput("codigo_r"))),

  nav_panel("Ayuda", icon = icon("circle-question"), card(card_body(includeHTML(ruta("ayuda.html")))))
)

server <- function(input, output, session) {
  datos <- reactiveVal(NULL); fb <- reactiveVal(NULL); mensaje <- reactiveVal(NULL)

  cargar_firebase <- function(x) {
    mensaje("Leyendo los datos del estudio…")
    r <- tryCatch(fs_leer_estudio(x$proyecto, x$eid, x$token), error = function(e) e)
    if (inherits(r, "error")) { mensaje(paste("No se pudieron leer los datos:", conditionMessage(r))); return() }
    if (!identical(r$estudio$tecnica, "sorting")) { mensaje("Este estudio no es de Sorting."); return() }
    datos(list(largo = r$largo, nombre = r$estudio$nombre %||% "Estudio", fuente = "Firebase (en vivo)")); mensaje(NULL)
  }
  observeEvent(input$fb, { x <- input$fb; viejo <- fb(); fb(x); if (is.null(viejo) || !identical(viejo$eid, x$eid) || is.null(datos())) cargar_firebase(x) })
  observeEvent(input$actualizar, { req(fb()); cargar_firebase(fb()) })
  observeEvent(input$archivo, {
    r <- tryCatch(leer_archivo(input$archivo$datapath), error = function(e) e)
    if (inherits(r, "error")) { mensaje(paste("No se pudo leer el archivo:", conditionMessage(r))); return() }
    fb(NULL); datos(list(largo = r, nombre = tools::file_path_sans_ext(input$archivo$name), fuente = "Archivo")); mensaje(NULL)
  })
  output$estado <- renderUI({
    d <- datos(); m <- mensaje()
    tagList(if (!is.null(m)) div(class = "alert alert-info small", m),
      if (is.null(d)) div(class = "small text-muted", if (isTRUE(input$hay_abridor)) "Esperando la sesión de la app…"
        else "Abrí este análisis desde la app (estudio de Sorting → “Análisis completo en R”) o subí un archivo abajo.")
      else div(h5(class = "mb-0", d$nombre), div(class = "small text-muted mb-2", d$fuente),
        if (!is.null(fb())) actionButton("actualizar", "Actualizar datos", icon = icon("rotate"), class = "btn-sm btn-outline-primary mb-2")))
  })
  output$selector_sesiones <- renderUI({ d <- datos(); req(d); s <- orden_nat(d$largo$Sesion); if (length(s) < 2) return(NULL)
    checkboxGroupInput("sesiones", "Sesiones", s, selected = s, inline = TRUE) })

  A <- reactive({
    d <- datos(); validate(need(d, "Todavía no hay datos: abrí el análisis desde la app o subí un archivo."))
    r <- tryCatch(armar(d$largo, input$sesiones), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r
  })
  observe({ n <- nrow(A()$G); updateSliderInput(session, "k", max = max(2, min(8, n - 1))) })
  M <- reactive({ r <- tryCatch(coordenadas(A()$G, input$metodo), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  dims <- reactive(as.integer(strsplit(input$dims, ",")[[1]]))
  dis <- reactive(distatis(A()$G))
  C <- reactive(coocurrencia(A()$G))
  cla <- reactive(clasificar(M()$F, min(input$k, nrow(A()$G) - 1)))

  # ---------------- Datos
  output$cajas <- renderUI({ a <- A(); ng <- apply(a$G, 2, function(x) length(unique(x)))
    layout_columns(fill = FALSE,
      value_box("Panelistas", ncol(a$G), showcase = icon("users"), theme = "primary"),
      value_box("Muestras", nrow(a$G), showcase = icon("wine-glass"), theme = "secondary"),
      value_box("Grupos por panelista", sprintf("%.1f", mean(ng)), p(sprintf("entre %d y %d", min(ng), max(ng))), showcase = icon("layer-group"), theme = "info"),
      value_box("Palabras distintas", if (is.null(a$pal)) 0 else ncol(a$pal), showcase = icon("comment-dots"), theme = "light")) })
  pan <- reactive({ x <- panelistas(A(), cla()$cl); x$`Peso DISTATIS` <- round(dis()$alfa[x$Panelista], 3); x })
  output$tabla_pan <- renderDT(tabla(pan()))
  output$controles <- renderUI({ a <- A(); ng <- apply(a$G, 2, function(x) length(unique(x)))
    tagList(
      if (length(a$excluidas)) div(class = "alert alert-warning small", "No entran (no agruparon todas las muestras): ", paste(a$excluidas, collapse = ", ")),
      if (ncol(a$G) < 15) div(class = "alert alert-warning small", "Pocos panelistas: Chollet y colaboradores recomiendan al menos 15–20 para un mapa estable."),
      if (any(ng == 2 & nrow(a$G) > 6)) div(class = "small text-muted mb-1", "Panelistas con solo 2 grupos: ", paste(names(ng)[ng == 2], collapse = ", "), " (suelen separar por un único criterio, por ejemplo color o gusto)."),
      div(class = "small text-muted", "Cada panelista es una partición de las muestras; el mapa resume en qué coinciden.")) })
  output$p_ngrupos <- renderPlot(g_ngrupos(A()$G), res = 100)

  # ---------------- Mapa
  boot_par <- eventReactive(input$calc_boot, list(G = A()$G, met = input$metodo, F = M()$F, B = input$nboot, d = dims()))
  bt <- reactive({ if (input$calc_boot == 0) return(NULL); e <- boot_par()
    if (!identical(e$met, input$metodo) || !identical(e$d, dims())) return(NULL)
    withProgress(message = "Simulando paneles…", bootstrap(e$G, e$met, e$F, e$B, e$d)) })
  g_m <- reactive({ m <- M(); validate(need(max(dims()) <= ncol(m$F), "No hay tantas dimensiones."))
    g_mapa(m$F, m$pct, dims(), input$metodo, if (input$ver_grupos) cla()$cl else NULL,
           if (input$ver_pal) palabras_en_mapa(m$F, A()$pal, dims(), input$minpal) else NULL, bt()) })
  output$p_mapa <- renderPlot(g_m(), res = 100)
  output$t_inercia <- renderTable({ p <- M()$pct; data.frame(`Dimensión` = seq_len(min(5, length(p))), `%` = round(p[seq_len(min(5, length(p)))], 1), check.names = FALSE) })
  output$t_coord <- renderDT(datatable(round(as.data.frame(M()$F[, seq_len(min(5, ncol(M()$F))), drop = FALSE]), 3), options = list(dom = "t", pageLength = 50)))

  # ---------------- Co-ocurrencias y clasificación
  g_co <- reactive(g_cooc(C(), ncol(A()$G), cla()$hc$labels[cla()$hc$order]))
  output$p_cooc <- renderPlot(g_co(), res = 100)
  plot_arbol <- function() { h <- cla(); plot(h$hc, main = "Clasificación de las muestras", xlab = "", sub = "", ylab = "Altura (Ward)"); stats::rect.hclust(h$hc, k = length(unique(h$cl)), border = col_n(length(unique(h$cl)))) }
  output$p_arbol <- renderPlot(plot_arbol(), res = 100)
  output$t_grupos <- renderTable({ cl <- cla()$cl; data.frame(Grupo = sort(unique(cl)), Muestras = vapply(sort(unique(cl)), function(k) paste(names(cl)[cl == k], collapse = ", "), "")) })

  # ---------------- Palabras
  hay_pal <- reactive(!is.null(A()$pal) && ncol(A()$pal) >= 1)
  fr <- reactive(frecuencia_palabras(A()$pal))
  g_te <- reactive({ validate(need(hay_pal(), "Los panelistas no escribieron palabras.")); g_terminos(fr()) })
  output$p_terminos <- renderPlot(g_te(), res = 100)
  descf <- reactive(palabras_por_muestra(A()$pal))
  output$t_descfreq <- renderDT({ x <- descf(); validate(need(!is.null(x) && nrow(x), "Ninguna palabra caracteriza significativamente a una muestra (o no hay palabras).")); tabla(x, 15) })
  pal_gr <- reactive({ validate(need(hay_pal(), "Sin palabras.")); cl <- cla()$cl; P <- A()$pal
    do.call(rbind, lapply(sort(unique(cl)), function(k) { f <- sort(colSums(P[names(cl)[cl == k], , drop = FALSE]), decreasing = TRUE); f <- f[f > 0]
      data.frame(Grupo = k, Muestras = paste(names(cl)[cl == k], collapse = ", "), Palabras = paste(sprintf("%s (%d)", names(f)[seq_len(min(10, length(f)))], f[seq_len(min(10, length(f)))]), collapse = ", ")) })) })
  output$t_pal_grupos <- renderDT(tabla(pal_gr()))
  output$t_pal <- renderDT({ validate(need(hay_pal(), "Sin palabras.")); P <- A()$pal; P <- P[, order(-colSums(P)), drop = FALSE]
    datatable(as.data.frame.matrix(t(P)), options = list(pageLength = 25, scrollX = TRUE)) })

  # ---------------- Panelistas
  g_al <- reactive(g_barras(dis()$alfa, "Peso en el compromiso DISTATIS", "Peso (α)", 1 / length(dis()$alfa), "%.3f"))
  output$p_alfa <- renderPlot(g_al(), res = 100)
  g_pa <- reactive({ if (input$tipo_pan == "distatis") { d <- dis(); g_panelistas(d$panelistas, d$rv_eig, "Panelistas (DISTATIS)", "Panelistas cerca = agruparon de forma parecida") }
    else { m <- mapa_mca(A()$G); g_panelistas(m$var$eta2, m$eig[, 2], "Panelistas (MCA)", "Razón de correlación de cada panelista con cada dimensión") } })
  output$p_panelistas <- renderPlot(g_pa(), res = 100)
  g_r <- reactive({ R <- dis()$RV; o <- stats::hclust(stats::as.dist(1 - R))$order; g_rv(R[o, o]) })
  output$p_rv <- renderPlot(g_r(), res = 100)

  # ---------------- Repetibilidad
  output$u_dup <- renderUI({ dt <- duplicados(A()$G)
    if (is.null(dt)) return(tagList(p("Este estudio no tiene duplicados."), ayuda("Se cargan al crear el estudio: una muestra servida dos veces, con otro código, sin que el panelista lo sepa.")))
    tagList(renderTable(dt), ayuda("Un panelista consistente pone el duplicado en el mismo grupo que su original.")) })
  output$u_ses <- renderUI({ R <- rv_sesiones(datos()$largo)
    if (is.null(R)) return(p("Hay una sola sesión."))
    tagList(p("RV entre las matrices de co-ocurrencia de cada sesión (1 = mismo mapa):"), renderTable(R, rownames = TRUE, digits = 3)) })

  # ---------------- Descargas
  png_de <- function(g, w = 10, h = 8) downloadHandler(filename = function() paste0("sorting_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
    content = function(file) ggsave(file, g(), width = w, height = h, dpi = 200, bg = "white"))
  output$d_mapa <- png_de(g_m); output$d_cooc <- png_de(g_co, 10, 9); output$d_terminos <- png_de(g_te); output$d_alfa <- png_de(g_al)
  output$d_panelistas <- png_de(g_pa); output$d_rv <- png_de(g_r, 11, 10)
  output$d_arbol <- downloadHandler("dendrograma_sorting.png", function(file) { png(file, 2000, 1400, res = 200); plot_arbol(); dev.off() })
  nombre_archivo <- reactive(gsub("[^A-Za-z0-9_-]+", "_", datos()$nombre))
  output$d_matriz <- downloadHandler(function() paste0(nombre_archivo(), "_sorting_matriz.csv"),
    function(file) utils::write.csv(data.frame(Producto = rownames(A()$G), A()$G, check.names = FALSE), file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$d_excel <- downloadHandler(function() paste0(nombre_archivo(), "_sorting.xlsx"), function(file) {
    a <- A(); m <- M()
    hojas <- list(
      Matriz_grupos = data.frame(Producto = rownames(a$G), a$G, check.names = FALSE),
      Coocurrencias = data.frame(Producto = rownames(C()), C(), check.names = FALSE),
      Coordenadas = data.frame(Producto = rownames(m$F), round(m$F, 4), check.names = FALSE),
      Inercia = data.frame(Dimension = seq_along(m$pct), Porcentaje = round(m$pct, 2)),
      Clasificacion = data.frame(Producto = names(cla()$cl), Grupo = cla()$cl),
      Panelistas = pan(),
      RV_panelistas = data.frame(Panelista = rownames(dis()$RV), round(dis()$RV, 3), check.names = FALSE),
      Formato_largo = a$d)
    if (hay_pal()) { hojas$Palabras <- data.frame(Producto = rownames(a$pal), unclass(a$pal), check.names = FALSE); hojas$Descfreq <- descf() %||% data.frame(Nota = "Sin palabras significativas") }
    if (!is.null(duplicados(a$G))) hojas$Duplicados <- duplicados(a$G)
    writexl::write_xlsx(hojas, file) })
  output$d_informe <- downloadHandler(
    filename = function() paste0(nombre_archivo(), "_informe_sorting.", if (input$formato == "word") "docx" else "html"),
    content = function(file) withProgress(message = "Armando el informe…", {
      dir <- tempfile(); dir.create(dir); file.copy(ruta("informe.Rmd"), dir); file.copy(ruta("R"), dir, recursive = TRUE)
      out <- rmarkdown::render(file.path(dir, "informe.Rmd"),
        output_format = if (input$formato == "word") rmarkdown::word_document() else rmarkdown::html_document(self_contained = TRUE, toc = TRUE),
        params = list(largo = datos()$largo, nombre = datos()$nombre, sesiones = input$sesiones, metodo = input$metodo, dims = dims(),
                      k = input$k, minpal = input$minpal, nboot = input$nboot),
        envir = new.env(parent = globalenv()), quiet = TRUE)
      file.copy(out, file) }))
  output$codigo_r <- renderText(paste0(
    'library(FactoMineR)\n',
    'd <- read.csv("', nombre_archivo(), '_sorting_matriz.csv", row.names = 1, check.names = FALSE, colClasses = "character")\n',
    'd[] <- lapply(names(d), function(v) factor(paste(v, d[[v]], sep = "_")))\n',
    'acm <- MCA(d)                                          # mapa (Cadoret, Lê y Pagès, 2009)\n',
    'plot(acm, invisible = c("var", "quali.sup"))           # solo las muestras\n',
    'HCPC(acm, nb.clust = ', input$k, ')                                # clasificación de las muestras\n',
    '# co-ocurrencias y MDS clásico\n',
    'co <- Reduce(`+`, lapply(d, function(g) outer(g, g, "==")))\n',
    'mds <- cmdscale(1 - co / ncol(d), eig = TRUE)\n',
    '# DISTATIS: install.packages("DistatisR"); DistatisR::distatis(...) con una matriz de distancias por panelista\n',
    'library(SensoMineR)\n',
    'res <- fast(d)                                          # FAST con elipses (usar la matriz con palabras de la app)\n'))
}

shinyApp(ui, server)
