# =====================================================================================
# Napping / mapeo proyectivo · Análisis completo
# App Shiny que acompaña a la app "LibreSense" (https://anibalacatania.github.io/libresense/).
# Se abre desde la administración de un estudio de Napping ("Análisis completo en R"),
# o se usa sola subiendo la matriz / el formato largo que descarga la app.
#
# Métodos según Dehlholm (2015), "Projective Mapping and Napping", cap. 9 de Varela y Ares (eds.),
# Novel Techniques in Sensory Characterization and Consumer Profiling; FactoMineR y SensoMineR.
# =====================================================================================
library(shiny)
library(bslib)
library(ggplot2)
library(FactoMineR)
library(SensoMineR)
library(DT)
library(httr2)
requireNamespace("jsonlite")   # (sin library(): su validate() taparía al de shiny)
library(writexl)
library(wordcloud)
library(gtools)
library(ggrepel)
library(rmarkdown)
library(htmltools)

# carpeta de esta app: "." si se publica sola; la fija la app combinada (shiny/app.R) si se publica todo junto
APP_DIR <- if (exists("APP_DIR", inherits = FALSE)) APP_DIR else "."
ruta <- function(...) file.path(APP_DIR, ...)
for (f in list.files(ruta("R"), pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8", local = TRUE)

origenes_extra <- strsplit(Sys.getenv("PF_ORIGENES_EXTRA", ""), ",")[[1]]
origenes_extra <- origenes_extra[nzchar(origenes_extra)]
tema_app <- bs_theme(version = 5, primary = "#059669", secondary = "#db2777", base_font = font_collection("Inter", "system-ui", "sans-serif"))
ayuda <- function(...) tags$p(class = "text-muted small mb-2", ...)
tarjeta <- function(titulo, ..., descargar = NULL) card(full_screen = TRUE,
  card_header(class = "d-flex justify-content-between align-items-center", titulo, descargar), card_body(...))
bajar <- function(id) downloadButton(id, "PNG", class = "btn-sm btn-outline-secondary")

ui <- page_navbar(
  title = tags$b("Napping"), window_title = "Napping · análisis completo", id = "nav", theme = tema_app, fillable = FALSE,
  header = tagList(
    tags$script(HTML(sprintf("window.PF_ORIGENES_EXTRA = %s;", jsonlite::toJSON(origenes_extra)))),
    tags$script(src = "conexion.js"),
    tags$style(HTML(".shiny-output-error-validation{color:#6b7280;font-size:1rem;padding:1rem}"))),
  sidebar = sidebar(width = 290,
    uiOutput("estado"),
    uiOutput("selector_tarea"),
    uiOutput("selector_sesiones"),
    selectInput("dims", "Dimensiones", c("1 y 2" = "1,2", "1 y 3" = "1,3", "2 y 3" = "2,3")),
    checkboxInput("parciales", "Mostrar a cada panelista sobre el mapa", FALSE),
    hr(),
    fileInput("archivo", "…o subí un archivo de la app", accept = c(".csv", ".txt"), buttonLabel = "Elegir", placeholder = "Matriz o formato largo (CSV)"),
    conditionalPanel("output.desde_archivo", selectInput("hoja_csv", "Hoja usada", c("Rectangular 60 × 40" = "rect", "Cuadrada 60 × 60" = "cuad", "Redonda 40 cm" = "redonda"))),
    ayuda("En la app: Resultados → “Matriz para MFA” o “Formato largo”.")),

  nav_panel("Datos", icon = icon("table"),
    uiOutput("cajas"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Panelistas", DTOutput("tabla_pan")),
      tarjeta("Controles", uiOutput("controles")))),

  nav_panel("Mapa de consenso", icon = icon("map"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Posición de las muestras", plotOutput("p_consenso", height = "620px"), descargar = bajar("d_consenso")),
      tarjeta("Cómo leerlo",
        p("Análisis factorial múltiple (MFA) con las coordenadas X, Y de cada panelista como un grupo, sin estandarizar",
          " (", code('type = "c"'), "), tal como propone el capítulo. Muestras cerca = el panel las percibió parecidas."),
        p("El Napping es rápido pero poco preciso: conviene interpretar las dos primeras dimensiones y los grupos de muestras."),
        hr(), h6("Inercia por dimensión"), tableOutput("t_inercia"))),
    tarjeta("Coordenadas de las muestras", DTOutput("t_coord"))),

  nav_panel("Elipses de confianza", icon = icon("bullseye"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Elipses de confianza al 95%", plotOutput("p_elipses", height = "620px"), descargar = bajar("d_elipses")),
      tarjeta("Método",
        radioButtons("met_elipses", NULL, c("Bootstrap de puntos parciales (Dehlholm et al., 2012; apéndice del cap. 9)" = "dehlholm",
                                            "Paneles virtuales (Cadoret y Husson, 2013; boot de SensoMineR)" = "cadoret")),
        sliderInput("nsim", "Simulaciones", 100, 1000, 300, step = 100),
        actionButton("calc_elipses", "Calcular elipses", class = "btn-primary w-100"),
        ayuda("Dos muestras cuyas elipses no se superponen fueron diferenciadas por el panel. Según el capítulo, en Napping las elipses suelen ser grandes.")))),

  nav_panel("Palabras", icon = icon("comment-dots"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Muestras y palabras", plotOutput("p_pal_mapa", height = "620px"), descargar = bajar("d_pal_mapa")),
      tarjeta("Filtro", sliderInput("minpal", "Mostrar palabras usadas al menos", 1, 10, 2, step = 1),
        ayuda("Cada palabra se ubica en el promedio de las muestras donde se usó (más grande = más usada). No modifica el mapa: en el MFA las palabras son un grupo suplementario."))),
    tarjeta("Palabras que caracterizan a cada muestra", ayuda("FactoMineR::descfreq: palabras usadas significativamente más (v.test > 0) o menos (v.test < 0) en esa muestra que en el resto (p < 0,05)."),
            DTOutput("t_descfreq")),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Palabras más usadas", plotOutput("p_terminos", height = "480px"), descargar = bajar("d_terminos")),
      tarjeta("Nube de palabras", plotOutput("p_nube", height = "480px")))),

  nav_panel("Panelistas", icon = icon("users"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Acuerdo con el consenso (RV)", ayuda("Coeficiente RV entre la hoja de cada panelista y el consenso (1 = misma configuración)."),
              plotOutput("p_rv_cons", height = "440px"), descargar = bajar("d_rv_cons")),
      tarjeta("Uso de la hoja", ayuda("Área que ocupan las muestras, en % de la hoja. Valores muy bajos = proyecciones “amontonadas” (Dehlholm): el panelista no usó el espacio."),
              plotOutput("p_uso", height = "440px"), descargar = bajar("d_uso"))),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Parecido entre panelistas", plotOutput("p_rv", height = "520px"), descargar = bajar("d_rv")),
      tarjeta("Panelistas sobre las dimensiones", plotOutput("p_grupos", height = "520px"), descargar = bajar("d_grupos")))),

  nav_panel("Hojas individuales", icon = icon("table-cells"),
    tarjeta("Hoja de cada panelista", ayuda("Tal como ubicó cada uno las muestras. Sirve para ver estrategias distintas: en línea, en L, dispersas, amontonadas, en grupos (Dehlholm, fig. 9.1). Círculo vacío = duplicado."),
            plotOutput("p_hojas", height = "820px"), descargar = bajar("d_hojas")),
    tarjeta("Superpuestas al consenso", ayuda("Cada hoja rotada y escalada (Procrustes) sobre el consenso, como pmfa de SensoMineR. Líneas cortas = el panelista coincide con el panel."),
            plotOutput("p_superp", height = "820px"), descargar = bajar("d_superp"))),

  nav_panel("Clasificación", icon = icon("sitemap"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Dendrograma de las muestras", plotOutput("p_arbol", height = "560px"), descargar = bajar("d_arbol")),
      tarjeta("Grupos", sliderInput("k", "Cantidad de grupos", 2, 6, 3, step = 1),
        ayuda("Clasificación jerárquica (Ward) sobre las coordenadas del MFA (HCPC)."), tableOutput("t_grupos")))),

  nav_panel("Repetibilidad", icon = icon("repeat"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Duplicados", uiOutput("u_dup")),
      tarjeta("Índice de desempeño por panelista", uiOutput("u_ppi"))),
    tarjeta("Sesiones (repeticiones)", uiOutput("u_ses"))),

  nav_panel("Descargas", icon = icon("download"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Tablas", p("Excel con coordenadas, consenso, RV, uso de la hoja, palabras, descfreq, duplicados y formato largo."),
        downloadButton("d_excel", "Descargar Excel", class = "btn-primary"), br(), br(), downloadButton("d_matriz", "Matriz (CSV, para R)")),
      tarjeta("Informe", p("Informe con los gráficos y tablas de lo elegido en el panel izquierdo."),
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
    if (!identical(r$estudio$tecnica, "napping")) { mensaje("Este estudio no es de Napping."); return() }
    datos(list(largo = r$largo, tareas = r$tareas, hoja = r$hoja, pal_tabla = NULL, nombre = r$estudio$nombre %||% "Estudio", fuente = "Firebase (en vivo)"))
    mensaje(NULL)
  }
  observeEvent(input$fb, { x <- input$fb; viejo <- fb(); fb(x); if (is.null(viejo) || !identical(viejo$eid, x$eid) || is.null(datos())) cargar_firebase(x) })
  observeEvent(input$actualizar, { req(fb()); cargar_firebase(fb()) })
  observeEvent(input$archivo, {
    r <- tryCatch(leer_archivo(input$archivo$datapath), error = function(e) e)
    if (inherits(r, "error")) { mensaje(paste("No se pudo leer el archivo:", conditionMessage(r))); return() }
    fb(NULL); datos(c(r, list(hoja = NULL, nombre = tools::file_path_sans_ext(input$archivo$name), fuente = "Archivo"))); mensaje(NULL)
  })
  output$desde_archivo <- reactive(!is.null(datos()) && is.null(datos()$hoja)); outputOptions(output, "desde_archivo", suspendWhenHidden = FALSE)
  hoja <- reactive(datos()$hoja %||% input$hoja_csv)

  output$estado <- renderUI({
    d <- datos(); m <- mensaje()
    tagList(if (!is.null(m)) div(class = "alert alert-info small", m),
      if (is.null(d)) div(class = "small text-muted", if (isTRUE(input$hay_abridor)) "Esperando la sesión de la app…"
        else "Abrí este análisis desde la app (estudio de Napping → “Análisis completo en R”) o subí un archivo abajo.")
      else div(h5(class = "mb-0", d$nombre), div(class = "small text-muted mb-2", d$fuente),
        if (!is.null(fb())) actionButton("actualizar", "Actualizar datos", icon = icon("rotate"), class = "btn-sm btn-outline-primary mb-2")))
  })
  output$selector_tarea <- renderUI({ d <- datos(); req(d); t <- d$tareas; if (length(t) < 2) return(NULL)
    selectInput("tarea", "Napping parcial", c("Todos los focos juntos" = "", stats::setNames(t, t))) })
  output$selector_sesiones <- renderUI({ d <- datos(); req(d); s <- orden_nat(d$largo$Sesion); if (length(s) < 2) return(NULL)
    checkboxGroupInput("sesiones", "Sesiones", s, selected = s, inline = TRUE) })

  A <- reactive({
    d <- datos(); validate(need(d, "Todavía no hay datos: abrí el análisis desde la app o subí un archivo."))
    r <- tryCatch(armar(d$largo, input$sesiones, input$tarea, d$pal_tabla), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r
  })
  mfa <- reactive(correr_mfa(A()))
  dims <- reactive(as.integer(strsplit(input$dims, ",")[[1]]))
  rvc <- reactive({ r <- mfa()$group$RV; r[A()$grupos, "MFA"] })
  uso <- reactive(uso_hoja(A(), hoja()))
  observe({ n <- nrow(A()$d); updateSliderInput(session, "k", max = max(2, min(8, n - 1))) })

  # ---------------- Datos
  output$cajas <- renderUI({ a <- A()
    layout_columns(fill = FALSE,
      value_box("Muestras", nrow(a$d), showcase = icon("wine-glass"), theme = "primary"),
      value_box("Hojas analizadas", length(a$grupos), p(paste(length(unique(a$largo$Valuador)), "panelistas")), showcase = icon("users"), theme = "secondary"),
      value_box("Palabras distintas", if (is.null(a$pal)) 0 else ncol(a$pal), showcase = icon("comment-dots"), theme = "info"),
      value_box("Hoja", (HOJAS[[hoja()]] %||% HOJAS$rect)$nombre, showcase = icon("note-sticky"), theme = "light")) })
  output$tabla_pan <- renderDT({ a <- A()
    np <- vapply(a$grupos, function(g) { w <- a$largo$Palabras[a$largo$Grupo == g]; sum(lengths(strsplit(w[nzchar(w)], ","))) }, 0)
    datatable(data.frame(Hoja = a$grupos, `RV con el consenso` = round(rvc(), 3), `Uso de la hoja (%)` = round(uso(), 1), Palabras = np, check.names = FALSE),
              rownames = FALSE, options = list(pageLength = 25, dom = "tp")) })
  output$controles <- renderUI({ a <- A()
    tagList(
      if (length(a$excluidas)) div(class = "alert alert-warning small", "No entran al análisis: ", paste(a$excluidas, collapse = ", ")) else div(class = "small text-muted mb-2", "Todas las hojas entran al análisis."),
      if (any(uso() < 10)) div(class = "alert alert-warning small", "Hojas con muestras muy amontonadas (<10% de la hoja): ", paste(names(uso())[uso() < 10], collapse = ", ")),
      div(class = "small text-muted", "Coordenadas en cm desde la esquina inferior izquierda de la hoja. Con varias sesiones o focos juntos, cada hoja es un grupo del MFA.")) })

  # ---------------- Consenso
  parciales <- reactive({ if (!isTRUE(input$parciales)) return(NULL)
    dd <- dims(); cp <- mfa()$ind$coord.partiel; v <- rownames(mfa()$ind$coord)
    do.call(rbind, lapply(A()$grupos, function(g) { r <- paste0(v, ".", g); data.frame(vino = v, panelista = g, x = cp[r, dd[1]], y = cp[r, dd[2]]) })) })
  g_cons <- reactive({ m <- mfa(); validate(need(max(dims()) <= ncol(m$ind$coord), "No hay tantas dimensiones.")); g_consenso(m$ind$coord, m$eig[, 2], dims(), parciales(), "Consenso (MFA)") })
  output$p_consenso <- renderPlot(g_cons(), res = 100)
  output$t_inercia <- renderTable({ e <- mfa()$eig; stats::setNames(data.frame(seq_len(nrow(e)), round(e[, 2], 1)), c("Dimensión", "%"))[seq_len(min(5, nrow(e))), ] })
  output$t_coord <- renderDT(datatable(round(as.data.frame(mfa()$ind$coord), 3), options = list(dom = "t", pageLength = 50)))

  # ---------------- Elipses
  dibujar_elipses <- function(a, m, met, nsim, dd) {
    if (met == "dehlholm") print(g_elipses(m$ind$coord, m$eig[, 2], elipses_dehlholm(m, dd, nsim), dd))
    else { set.seed(1); SensoMineR::boot(a$d, method = "napping", nbsim = nsim, axes = dd, new.plot = FALSE, title = "Elipses de confianza (95%)") }
    invisible(NULL)
  }
  el_par <- eventReactive(input$calc_elipses, list(a = A(), m = mfa(), met = input$met_elipses, nsim = input$nsim, dd = dims()))
  output$p_elipses <- renderPlot({ validate(need(input$calc_elipses > 0, "Elegí el método y tocá “Calcular elipses”."))
    e <- el_par(); withProgress(message = "Simulando…", dibujar_elipses(e$a, e$m, e$met, e$nsim, e$dd)) }, res = 100)
  output$d_elipses <- downloadHandler("elipses_napping.png", function(file) { png(file, 2000, 1600, res = 200); on.exit(dev.off()); dibujar_elipses(A(), mfa(), input$met_elipses, input$nsim, dims()) })

  # ---------------- Palabras
  hay_pal <- reactive(!is.null(A()$pal) && ncol(A()$pal) >= 2)
  g_palm <- reactive({ validate(need(hay_pal(), "No hay palabras (o hay menos de dos distintas).")); g_palabras_mapa(mfa(), dims(), input$minpal, A()$pal) })
  output$p_pal_mapa <- renderPlot(g_palm(), res = 100)
  descf <- reactive(if (hay_pal()) palabras_por_muestra(A()$pal) else NULL)
  output$t_descfreq <- renderDT({ x <- descf(); validate(need(!is.null(x) && nrow(x), "Ninguna palabra caracteriza significativamente a una muestra (o no hay palabras)."))
    datatable(x, rownames = FALSE, options = list(pageLength = 15)) })
  fr <- reactive(frecuencia_palabras(A()$pal))
  g_term <- reactive({ validate(need(nrow(fr()) > 0, "No hay palabras.")); g_terminos(fr()) })
  output$p_terminos <- renderPlot(g_term(), res = 100)
  output$p_nube <- renderPlot({ f <- fr(); validate(need(nrow(f) > 0, "No hay palabras.")); par(mar = c(0, 0, 0, 0)); set.seed(2)
    wordcloud::wordcloud(f$Termino, f$Panelistas, min.freq = 1, scale = c(4, 0.8), random.order = FALSE, colors = c("#6ee7b7", "#059669", "#db2777", "#be185d")) }, res = 100)

  # ---------------- Panelistas
  g_rvc <- reactive(g_barras(rvc(), "Acuerdo de cada hoja con el consenso", "RV", 0.5))
  output$p_rv_cons <- renderPlot(g_rvc(), res = 100)
  g_uso <- reactive(g_barras(uso(), "Uso de la hoja", "% del área de la hoja", 10))
  output$p_uso <- renderPlot(g_uso(), res = 100)
  g_rvm <- reactive({ r <- mfa()$group$RV; g_rv(r[A()$grupos, A()$grupos]) })
  output$p_rv <- renderPlot(g_rvm(), res = 100)
  g_grp <- reactive({ m <- mfa(); m$group$coord <- m$group$coord[A()$grupos, , drop = FALSE]; g_grupos(m, dims()) })
  output$p_grupos <- renderPlot(g_grp(), res = 100)

  # ---------------- Hojas
  g_hj <- reactive(g_hojas(A(), hoja())); output$p_hojas <- renderPlot(g_hj(), res = 100)
  g_sp <- reactive(g_superpuesto(superponer(A(), mfa(), dims()))); output$p_superp <- renderPlot(g_sp(), res = 100)

  # ---------------- Clasificación
  hc <- reactive(correr_hcpc(mfa(), input$k))
  plot_arbol <- function() plot(hc(), choice = "tree", title = "Clasificación jerárquica de las muestras")
  output$p_arbol <- renderPlot(plot_arbol(), res = 100)
  output$t_grupos <- renderTable({ cl <- hc()$data.clust$clust; v <- rownames(hc()$data.clust)
    data.frame(Grupo = sort(unique(cl)), Muestras = vapply(sort(unique(cl)), function(k) paste(v[cl == k], collapse = ", "), "")) })

  # ---------------- Repetibilidad
  output$u_dup <- renderUI({ dt <- duplicados(mfa())
    if (is.null(dt)) return(tagList(p("Este estudio no tiene duplicados."), ayuda("El capítulo recomienda incluir una o más muestras repetidas a ciegas: se cargan al crear el estudio (“Duplicados”).")))
    tagList(renderTable(dt), p(class = "small", "El percentil indica qué porcentaje de las distancias entre muestras distintas es menor o igual a la del duplicado: más bajo = el panel lo ubicó cerca de su original.")) })
  output$u_ppi <- renderUI({ v <- indice_desempeno(A())
    if (is.null(v)) return(p("Necesita duplicados."))
    tagList(renderPlot(g_barras(v, "Índice de desempeño (más bajo = mejor)", "dist(duplicado, original) / dist. máxima", invertir = TRUE), res = 100, height = 420),
      ayuda("Hopfer y Heymann (2013): distancia entre el duplicado y su muestra dividida por la distancia entre las dos muestras más alejadas de ese panelista.")) })
  output$u_ses <- renderUI({ R <- rv_sesiones(datos()$largo, input$tarea)
    if (is.null(R)) return(p("Hay una sola sesión. El capítulo señala que las repeticiones individuales rara vez coinciden, aunque el promedio del panel sí; para una evaluación espontánea se recomienda una sola."))
    tagList(p("RV entre los mapas de consenso de cada sesión:"), renderTable(R, rownames = TRUE, digits = 3)) })

  # ---------------- Descargas
  png_de <- function(g, w = 10, h = 8) downloadHandler(filename = function() paste0("napping_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
    content = function(file) ggsave(file, g(), width = w, height = h, dpi = 200, bg = "white"))
  output$d_consenso <- png_de(g_cons); output$d_pal_mapa <- png_de(g_palm); output$d_terminos <- png_de(g_term)
  output$d_rv_cons <- png_de(g_rvc); output$d_uso <- png_de(g_uso); output$d_rv <- png_de(g_rvm); output$d_grupos <- png_de(g_grp)
  output$d_hojas <- png_de(g_hj, 12, 10); output$d_superp <- png_de(g_sp, 12, 10)
  output$d_arbol <- downloadHandler("dendrograma_napping.png", function(file) { png(file, 2000, 1400, res = 200); plot_arbol(); dev.off() })

  nombre_archivo <- reactive(gsub("[^A-Za-z0-9_-]+", "_", datos()$nombre))
  matriz <- reactive({ a <- A(); m <- a$d; if (!is.null(a$pal)) m <- cbind(m, as.data.frame(a$pal, check.names = FALSE)); data.frame(Producto = rownames(m), m, check.names = FALSE) })
  output$d_matriz <- downloadHandler(function() paste0(nombre_archivo(), "_matriz.csv"), function(file) utils::write.csv(matriz(), file, row.names = FALSE))
  output$d_excel <- downloadHandler(function() paste0(nombre_archivo(), "_napping.xlsx"), function(file) {
    m <- mfa(); a <- A()
    writexl::write_xlsx(list(
      Matriz = matriz(),
      Consenso_MFA = data.frame(Muestra = rownames(m$ind$coord), round(m$ind$coord, 4), check.names = FALSE),
      Inercia = data.frame(Dimension = seq_len(nrow(m$eig)), round(m$eig, 3), check.names = FALSE),
      RV = data.frame(Hoja = rownames(m$group$RV), round(m$group$RV, 3), check.names = FALSE),
      Uso_hoja = data.frame(Hoja = names(uso()), Porcentaje = round(uso(), 2)),
      Palabras = fr(), Descfreq = descf() %||% data.frame(Nota = "Sin palabras significativas"),
      Duplicados = duplicados(m) %||% data.frame(Nota = "Sin duplicados"),
      Formato_largo = a$largo), file) })
  output$d_informe <- downloadHandler(
    filename = function() paste0(nombre_archivo(), "_informe_napping.", if (input$formato == "word") "docx" else "html"),
    content = function(file) withProgress(message = "Armando el informe…", {
      dir <- tempfile(); dir.create(dir); file.copy(ruta("informe.Rmd"), dir); file.copy(ruta("R"), dir, recursive = TRUE)
      out <- rmarkdown::render(file.path(dir, "informe.Rmd"),
        output_format = if (input$formato == "word") rmarkdown::word_document() else rmarkdown::html_document(self_contained = TRUE, toc = TRUE),
        params = list(largo = datos()$largo, pal_tabla = datos()$pal_tabla, nombre = datos()$nombre, sesiones = input$sesiones, tarea = input$tarea %||% "",
                      hoja = hoja(), dims = dims(), nsim = input$nsim, k = input$k),
        envir = new.env(parent = globalenv()), quiet = TRUE)
      file.copy(out, file) }))
  output$codigo_r <- renderText({ a <- A(); k <- length(a$grupos); np <- if (is.null(a$pal)) 0 else ncol(a$pal)
    paste0('library(FactoMineR); library(SensoMineR)\n',
      'd <- read.csv("', nombre_archivo(), '_matriz.csv", row.names = 1, check.names = FALSE)\n',
      'k <- ', k, '   # hojas (X, Y de cada panelista', if (length(unique(a$largo$Tarea)) > 1) ' y foco', ')\n',
      if (np >= 2) paste0('afm <- MFA(d, group = c(rep(2, k), ', np, '), type = c(rep("c", k), "f"), num.group.sup = k + 1)\n')
      else 'afm <- MFA(d[, 1:(2 * k)], group = rep(2, k), type = rep("c", k))\n',
      'afm$group$RV                                  # acuerdo de cada hoja con el consenso\n',
      'coord <- d[, 1:(2 * k)]\n',
      'pmfa(coord)                                   # cada panelista superpuesto al consenso\n',
      'boot(coord, method = "napping")               # elipses (Cadoret y Husson)\n',
      if (np >= 2) 'descfreq(d[, -(1:(2 * k))])                  # palabras que caracterizan a cada muestra\n' else '',
      'HCPC(afm, nb.clust = ', input$k, ')                     # clasificación de las muestras\n') })
}

shinyApp(ui, server)
