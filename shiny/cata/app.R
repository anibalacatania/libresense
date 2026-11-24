# =====================================================================================
# CATA con aceptabilidad · Análisis completo
# App Shiny que acompaña a la app "LibreSense" (https://anibalacatania.github.io/libresense/).
# Se abre desde la administración de un estudio CATA ("Análisis completo en R"),
# o se usa sola subiendo el CSV de resultados que descarga la app.
#
# Métodos: Meyners y Castura (2014), cap. 11 de Varela y Ares (eds.), Novel Techniques in Sensory
# Characterization and Consumer Profiling; Ares y Jaeger (2015), cap. 11 de Delarue et al. (eds.), Rapid Sensory Profiling Techniques; Meyners, Castura y Carr (2013); Ares et al. (2014).
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

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

origenes_extra <- strsplit(Sys.getenv("PF_ORIGENES_EXTRA", ""), ",")[[1]]
origenes_extra <- origenes_extra[nzchar(origenes_extra)]
tema_app <- bs_theme(version = 5, primary = "#0284c7", secondary = "#db2777", base_font = font_collection("Inter", "system-ui", "sans-serif"))
ayuda <- function(...) tags$p(class = "text-muted small mb-2", ...)
tarjeta <- function(titulo, ..., descargar = NULL) card(full_screen = TRUE,
  card_header(class = "d-flex justify-content-between align-items-center", titulo, descargar), card_body(...))
bajar <- function(id) downloadButton(id, "PNG", class = "btn-sm btn-outline-secondary")
tabla <- function(x, n = 25, ...) datatable(x, rownames = FALSE, options = list(pageLength = n, dom = if (nrow(x) > n) "tp" else "t", scrollX = TRUE), ...)

ui <- page_navbar(
  title = tags$b("CATA"), window_title = "CATA · análisis completo", id = "nav", theme = tema_app, fillable = FALSE,
  header = tagList(
    tags$script(HTML(sprintf("window.PF_ORIGENES_EXTRA = %s;", jsonlite::toJSON(origenes_extra)))),
    tags$script(src = "conexion.js"),
    tags$style(HTML(".shiny-output-error-validation{color:#6b7280;font-size:1rem;padding:1rem}"))),
  sidebar = sidebar(width = 290,
    uiOutput("estado"),
    uiOutput("selector_sesiones"),
    selectInput("ajuste", "Comparaciones de a pares (McNemar)", c("Sin ajuste" = "none", "Holm" = "holm", "Bonferroni" = "bonferroni")),
    sliderInput("umbral", "Umbral de penalidades (% de evaluaciones)", 5, 50, 20, step = 5),
    hr(),
    fileInput("archivo", "…o subí el CSV de la app", accept = c(".csv", ".txt"), buttonLabel = "Elegir", placeholder = "Resultados (CSV)"),
    ayuda("En la app: Resultados → “Descargar CSV”.")),

  nav_panel("Datos", icon = icon("table"),
    uiOutput("cajas"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Consumidores", DTOutput("tabla_cons")),
      tarjeta("Controles", uiOutput("controles")))),

  nav_panel("Aceptabilidad", icon = icon("heart"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Aceptabilidad media", plotOutput("p_agrado", height = "460px"), descargar = bajar("d_agrado")),
      tarjeta("ANOVA (producto + consumidor)", verbatimTextOutput("t_anova"),
        ayuda("Comparaciones de a pares: prueba de Tukey. La escala hedónica se analiza como numérica, lo habitual en estudios de consumidores."))),
    tarjeta("Detalle por producto", DTOutput("t_agrado")),
    tarjeta("Distribución de las respuestas", plotOutput("p_dist", height = "420px"), descargar = bajar("d_dist"))),

  nav_panel("Términos", icon = icon("list-check"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Frecuencia de citación", plotOutput("p_frec", height = "640px"), descargar = bajar("d_frec")),
      tarjeta("Cómo leerlo",
        p("Cada celda es el % de evaluaciones de ese producto en que se marcó el término."),
        p(tags$b("Q de Cochran"), ": ¿el término se marca más en unos productos que en otros? Es la prueba adecuada para datos 0/1 de los mismos consumidores (Meyners, Castura y Carr, 2013)."),
        p(tags$b("Letras"), ": en los términos significativos, productos sin letra en común difieren (McNemar exacto de a pares; el ajuste se elige a la izquierda)."),
        checkboxInput("solo_sig_frec", "Mostrar solo los términos que discriminan (p < 0,05)", FALSE))),
    tarjeta("Tabla", DTOutput("t_frec"))),

  nav_panel("Mapa", icon = icon("map"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Productos y términos", plotOutput("p_ca", height = "640px"), descargar = bajar("d_ca")),
      tarjeta("Opciones",
        selectInput("dims", "Dimensiones", c("1 y 2" = "1,2", "1 y 3" = "1,3", "2 y 3" = "2,3")),
        checkboxInput("solo_sig_ca", "Solo los términos que discriminan (Cochran p < 0,05)", TRUE),
        checkboxInput("ca_ideal", "Mostrar el producto ideal (fila suplementaria)", TRUE),
        checkboxInput("ca_agrado", "Mostrar la dirección de la aceptabilidad (variable suplementaria)", TRUE),
        ayuda("Análisis de correspondencias (FactoMineR::CA) sobre la tabla de frecuencias, como recomiendan Meyners y Castura. El ideal y la aceptabilidad no modifican el mapa: se proyectan."),
        uiOutput("u_todos"),
        hr(), h6("Inercia por dimensión"), tableOutput("t_inercia"), uiOutput("u_chi")))),

  nav_panel("Penalidades", icon = icon("arrow-trend-down"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Impacto de cada término en la aceptabilidad", plotOutput("p_impacto", height = "520px"), descargar = bajar("d_impacto")),
      tarjeta("Opciones", selectInput("prod_imp", "Productos", c("Todos juntos" = "")),
        ayuda("Para cada término: aceptabilidad media de las evaluaciones en que se marcó menos la de las que no (t de Welch).",
              "Arriba y a la derecha: atributos frecuentes que suben el agrado (impulsores); abajo: defectos."),
        DTOutput("t_impacto"))),
    uiOutput("u_ideal")),

  nav_panel("Consumidores", icon = icon("users"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Mapa de preferencias interno", plotOutput("p_pref", height = "560px"), descargar = bajar("d_pref")),
      tarjeta("Segmentos", sliderInput("k", "Cantidad de segmentos", 2, 6, 2, step = 1),
        ayuda("Clasificación jerárquica (Ward) de los consumidores según la forma de su preferencia (agrado centrado por consumidor). Solo entran los que evaluaron todos los productos."),
        tableOutput("t_seg"))),
    tarjeta("Aceptabilidad por segmento", plotOutput("p_seg", height = "420px"), descargar = bajar("d_seg")),
    tarjeta("Términos que diferencian a cada segmento", ayuda("Impacto de cada término en el agrado dentro de cada segmento: los segmentos suelen valorar cosas distintas."), DTOutput("t_seg_imp"))),

  nav_panel("Descargas", icon = icon("download"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Tablas", p("Excel con frecuencias, Cochran y McNemar, aceptabilidad, Tukey, correspondencias, penalidades, ideal, segmentos y los datos."),
        downloadButton("d_excel", "Descargar Excel", class = "btn-primary"), br(), br(), downloadButton("d_csv", "Datos (CSV, para R)")),
      tarjeta("Informe", p("Informe con los gráficos y tablas, con las opciones elegidas en el panel izquierdo."),
        radioButtons("formato", NULL, c("Word (.docx)" = "word", "Página web (.html)" = "html"), inline = TRUE),
        downloadButton("d_informe", "Generar informe", class = "btn-primary"))),
    tarjeta("Código R equivalente", verbatimTextOutput("codigo_r"))),

  nav_panel("Ayuda", icon = icon("circle-question"), card(card_body(includeHTML("ayuda.html"))))
)

server <- function(input, output, session) {
  datos <- reactiveVal(NULL); fb <- reactiveVal(NULL); mensaje <- reactiveVal(NULL)

  cargar_firebase <- function(x) {
    mensaje("Leyendo los datos del estudio…")
    r <- tryCatch(fs_leer_estudio(x$proyecto, x$eid, x$token), error = function(e) e)
    if (inherits(r, "error")) { mensaje(paste("No se pudieron leer los datos:", conditionMessage(r))); return() }
    if (!identical(r$estudio$tecnica, "cata")) { mensaje("Este estudio no es de CATA."); return() }
    datos(list(ancho = r$ancho, terminos = r$terminos, gusto = r$gusto, nombre = r$estudio$nombre %||% "Estudio", fuente = "Firebase (en vivo)"))
    mensaje(NULL)
  }
  observeEvent(input$fb, { x <- input$fb; viejo <- fb(); fb(x); if (is.null(viejo) || !identical(viejo$eid, x$eid) || is.null(datos())) cargar_firebase(x) })
  observeEvent(input$actualizar, { req(fb()); cargar_firebase(fb()) })
  observeEvent(input$archivo, {
    r <- tryCatch(leer_archivo(input$archivo$datapath), error = function(e) e)
    if (inherits(r, "error")) { mensaje(paste("No se pudo leer el archivo:", conditionMessage(r))); return() }
    fb(NULL); datos(c(r, list(nombre = tools::file_path_sans_ext(input$archivo$name), fuente = "Archivo"))); mensaje(NULL)
  })

  output$estado <- renderUI({
    d <- datos(); m <- mensaje()
    tagList(if (!is.null(m)) div(class = "alert alert-info small", m),
      if (is.null(d)) div(class = "small text-muted", if (isTRUE(input$hay_abridor)) "Esperando la sesión de la app…"
        else "Abrí este análisis desde la app (estudio CATA → “Análisis completo en R”) o subí el CSV abajo.")
      else div(h5(class = "mb-0", d$nombre), div(class = "small text-muted mb-2", d$fuente),
        if (!is.null(fb())) actionButton("actualizar", "Actualizar datos", icon = icon("rotate"), class = "btn-sm btn-outline-primary mb-2")))
  })
  output$selector_sesiones <- renderUI({ d <- datos(); req(d); s <- orden_nat(d$ancho$Sesion[d$ancho$NombreProducto != "IDEAL"]); if (length(s) < 2) return(NULL)
    checkboxGroupInput("sesiones", "Sesiones", s, selected = s, inline = TRUE) })

  A <- reactive({
    d <- datos(); validate(need(d, "Todavía no hay datos: abrí el análisis desde la app o subí el CSV."))
    r <- tryCatch(armar(d$ancho, d$terminos, d$gusto, input$sesiones), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r
  })
  co <- reactive(cochran(A(), input$ajuste))
  ag <- reactive({ validate(need(A()$gusto, "Este estudio no tiene la pregunta de aceptabilidad.")); agrado(A()) })
  dims <- reactive(as.integer(strsplit(input$dims, ",")[[1]]))
  observe({ updateSelectInput(session, "prod_imp", choices = c("Todos juntos" = "", stats::setNames(A()$productos, A()$productos))) })

  # ---------------- Datos
  output$cajas <- renderUI({ a <- A()
    layout_columns(fill = FALSE,
      value_box("Consumidores", length(a$consumidores), p(paste(nrow(a$ev), "evaluaciones")), showcase = icon("users"), theme = "primary"),
      value_box("Productos", length(a$productos), showcase = icon("wine-glass"), theme = "secondary"),
      value_box("Términos", length(a$terminos), p(paste(length(a$usados), "marcados alguna vez")), showcase = icon("list-check"), theme = "info"),
      value_box("Ideal", if (is.null(a$ideal)) "No" else nrow(a$ideal), p(if (is.null(a$ideal)) "sin pregunta de ideal" else "consumidores"), showcase = icon("star"), theme = "light")) })
  output$tabla_cons <- renderDT({ a <- A(); ev <- a$ev
    x <- do.call(rbind, lapply(a$consumidores, function(v) { e <- ev[ev$Valuador == v, ]
      data.frame(Consumidor = v, Evaluaciones = nrow(e), `Aceptabilidad media` = if (a$gusto) round(mean(e$Aceptabilidad, na.rm = TRUE), 2) else NA,
                 `Términos por muestra` = round(mean(rowSums(e[, a$terminos, drop = FALSE])), 1), Ideal = if (!is.null(a$ideal) && v %in% a$ideal$Valuador) "Sí" else "", check.names = FALSE) }))
    tabla(x) })
  output$controles <- renderUI({ a <- A(); ev <- a$ev
    tpm <- rowSums(ev[, a$terminos, drop = FALSE]); n <- table(ev$Producto)
    planos <- if (a$gusto) { s <- tapply(ev$Aceptabilidad, ev$Valuador, function(x) length(unique(x[!is.na(x)]))); names(s)[s == 1 & table(ev$Valuador)[names(s)] > 1] } else character()
    tagList(
      div(class = "small mb-2", sprintf("Términos marcados por evaluación: %.1f en promedio (mín. %d, máx. %d).", mean(tpm), min(tpm), max(tpm))),
      if (any(tpm == 0)) div(class = "alert alert-warning small", sum(tpm == 0), " evaluaciones sin ningún término marcado."),
      if (min(n) < 50) div(class = "alert alert-warning small", "Hay productos con menos de 50 evaluaciones: Ares y Jaeger (2015) recomiendan al menos 50–100 consumidores para resultados estables."),
      if (length(planos)) div(class = "alert alert-warning small", "Consumidores que dieron el mismo puntaje a todo: ", paste(planos, collapse = ", ")),
      if (length(setdiff(a$terminos, a$usados))) div(class = "small text-muted", "Nunca marcados: ", paste(setdiff(a$terminos, a$usados), collapse = ", ")),
      div(class = "small text-muted", "Bloques completos para Cochran (consumidor × repetición con todos los productos): ", co()$bloques)) })

  # ---------------- Aceptabilidad
  g_ag <- reactive(g_agrado(ag())); output$p_agrado <- renderPlot(g_ag(), res = 100)
  output$t_anova <- renderPrint(ag()$anova)
  output$t_agrado <- renderDT(tabla(ag()$tabla[order(-ag()$tabla$Media), ]))
  g_di <- reactive(g_distribucion(ag())); output$p_dist <- renderPlot(g_di(), res = 100)

  # ---------------- Términos
  g_fr <- reactive({ r <- tryCatch(g_frecuencias(A(), co(), input$solo_sig_frec), error = function(e) e); validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  output$p_frec <- renderPlot(g_fr(), res = 100)
  tt <- reactive(tabla_terminos(A(), co()))
  output$t_frec <- renderDT(tabla(tt(), 50))

  # ---------------- Mapa
  ca <- reactive({ r <- tryCatch(correr_ca(A(), input$solo_sig_ca, co(), input$ca_ideal, input$ca_agrado), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  g_c <- reactive({ m <- ca(); validate(need(max(dims()) <= ncol(m$row$coord), "No hay tantas dimensiones.")); g_ca(m, dims()) })
  output$p_ca <- renderPlot(g_c(), res = 100)
  output$u_todos <- renderUI(if (isTRUE(attr(ca(), "todos"))) div(class = "alert alert-warning small mt-2",
    "Todavía ningún término discrimina entre productos (Cochran p < 0,05; suele pasar con pocos consumidores): el mapa usa todos los términos marcados."))
  output$t_inercia <- renderTable({ e <- ca()$eig; stats::setNames(data.frame(seq_len(nrow(e)), round(e[, 2], 1)), c("Dimensión", "%"))[seq_len(min(5, nrow(e))), ] })
  output$u_chi <- renderUI({ m <- ca(); x <- frecuencias(A())[, rownames(m$col$coord), drop = FALSE]
    ch <- suppressWarnings(stats::chisq.test(as.matrix(x)))
    ayuda(sprintf("Prueba χ² de independencia: χ² = %.1f, gl = %d, p = %s.", ch$statistic, ch$parameter, format.pval(ch$p.value, digits = 3))) })

  # ---------------- Penalidades
  im <- reactive({ validate(need(A()$gusto, "Este estudio no tiene la pregunta de aceptabilidad.")); impacto(A(), input$prod_imp %||% "") })
  g_im <- reactive(g_impacto(im())); output$p_impacto <- renderPlot(g_im(), res = 100)
  output$t_impacto <- renderDT(tabla(im()[order(-im()$Diferencia), c("Termino", "% marcado", "Diferencia", "p")], 12))
  pe <- reactive(if (A()$gusto) penal_ideal(A(), input$prod_imp %||% "", input$umbral) else NULL)
  g_pe <- reactive(g_penal(pe(), input$umbral))
  g_br <- reactive(g_brecha(brecha_ideal(A())))
  output$u_ideal <- renderUI({
    if (is.null(A()$ideal)) return(tarjeta("Penalidades respecto del ideal", p("Este estudio no tiene la pregunta del producto ideal. Se activa al crear el estudio (“vino ideal”) y permite ver qué le falta o le sobra a cada producto.")))
    tagList(
      layout_columns(col_widths = c(7, 5),
        tarjeta("Penalidades respecto del ideal", plotOutput("p_penal", height = "520px"), descargar = bajar("d_penal")),
        tarjeta("Cómo leerlo",
          p("Para cada término se compara la aceptabilidad cuando el producto coincide con el ideal del consumidor y cuando no (Ares et al., 2014):"),
          tags$ul(tags$li(tags$b("Falta"), ": el consumidor lo quiere (está en su ideal) y el producto no lo tiene."),
                  tags$li(tags$b("Sobra"), ": el producto lo tiene y el consumidor no lo quiere.")),
          p("Los términos en la zona roja (frecuentes, con caída significativa) son las prioridades para reformular. Con un producto elegido arriba, se analiza solo ese producto."))),
      tarjeta("Tabla de penalidades", DTOutput("t_penal")),
      tarjeta("Diferencia de cada producto con el ideal", plotOutput("p_brecha", height = "480px"), descargar = bajar("d_brecha")))
  })
  output$p_penal <- renderPlot({ validate(need(!is.null(pe()), "Sin datos suficientes.")); g_pe() }, res = 100)
  output$t_penal <- renderDT({ x <- pe(); validate(need(!is.null(x), "Sin datos.")); tabla(x[order(x$Conclusion == "", -pmax(x$`Caída si falta`, x$`Caída si sobra`, na.rm = TRUE)), ], 50) })
  output$p_brecha <- renderPlot(g_br(), res = 100)

  # ---------------- Consumidores
  sg <- reactive({ validate(need(A()$gusto, "Este estudio no tiene la pregunta de aceptabilidad."))
    r <- tryCatch(segmentos(A(), input$k), error = function(e) e); validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  pref <- reactive({ validate(need(A()$gusto, "Este estudio no tiene la pregunta de aceptabilidad."))
    r <- tryCatch(mapa_pref(A()), error = function(e) e); validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  g_pr <- reactive({ cl <- tryCatch(sg()$cl, error = function(e) NULL); g_pref(pref(), cl) }); output$p_pref <- renderPlot(g_pr(), res = 100)
  g_sg <- reactive(g_segmentos(sg())); output$p_seg <- renderPlot(g_sg(), res = 100)
  output$t_seg <- renderTable({ s <- sg(); data.frame(Segmento = rownames(s$medias), round(s$medias, 2), check.names = FALSE) })
  seg_imp <- reactive({ s <- sg(); a <- A()
    do.call(cbind, c(list(Termino = a$terminos), lapply(sort(unique(s$cl)), function(g) {
      b <- a; b$ev <- a$ev[a$ev$Valuador %in% names(s$cl)[s$cl == g], ]; x <- impacto(b)
      stats::setNames(data.frame(sprintf("%+.2f%s", x$Diferencia, ifelse(!is.na(x$p) & x$p < 0.05, " *", ""))), paste("Segmento", g)) }))) })
  output$t_seg_imp <- renderDT(tabla(seg_imp(), 50))

  # ---------------- Descargas
  png_de <- function(g, w = 10, h = 8) downloadHandler(filename = function() paste0("cata_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
    content = function(file) ggsave(file, g(), width = w, height = h, dpi = 200, bg = "white"))
  output$d_agrado <- png_de(g_ag, 9, 6); output$d_dist <- png_de(g_di, 11, 6); output$d_frec <- png_de(g_fr, 10, 9); output$d_ca <- png_de(g_c)
  output$d_impacto <- png_de(g_im); output$d_penal <- png_de(g_pe); output$d_brecha <- png_de(g_br, 10, 8); output$d_pref <- png_de(g_pr); output$d_seg <- png_de(g_sg, 10, 6)

  nombre_archivo <- reactive(gsub("[^A-Za-z0-9_-]+", "_", datos()$nombre))
  output$d_csv <- downloadHandler(function() paste0(nombre_archivo(), "_cata.csv"), function(file) utils::write.csv(datos()$ancho, file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$d_excel <- downloadHandler(function() paste0(nombre_archivo(), "_cata.xlsx"), function(file) {
    a <- A(); F <- frecuencias(a)
    hojas <- list(
      Frecuencias = data.frame(Producto = rownames(F), F, check.names = FALSE),
      Cochran_McNemar = tt(),
      Datos = datos()$ancho)
    if (a$gusto) {
      g <- agrado(a); hojas$Aceptabilidad <- g$tabla
      hojas$ANOVA <- data.frame(Fuente = trimws(rownames(g$anova)), round(as.data.frame(g$anova), 4), check.names = FALSE)
      if (!is.null(g$tukey)) hojas$Tukey <- data.frame(Comparacion = rownames(g$tukey), round(g$tukey, 4), check.names = FALSE)
      hojas$Impacto <- impacto(a)
      if (!is.null(a$ideal)) { hojas$Penalidades_ideal <- penal_ideal(a, "", input$umbral); B <- brecha_ideal(a); hojas$Diferencia_ideal <- data.frame(Producto = rownames(B), B, check.names = FALSE) }
      s <- tryCatch(segmentos(a, input$k), error = function(e) NULL)
      if (!is.null(s)) hojas$Segmentos <- data.frame(Consumidor = names(s$cl), Segmento = s$cl)
    }
    m <- tryCatch(correr_ca(a, input$solo_sig_ca, co(), input$ca_ideal, FALSE), error = function(e) NULL)
    if (!is.null(m)) { hojas$CA_productos <- data.frame(Producto = rownames(m$row$coord), round(m$row$coord, 4), check.names = FALSE)
      hojas$CA_terminos <- data.frame(Termino = rownames(m$col$coord), round(m$col$coord, 4), check.names = FALSE) }
    writexl::write_xlsx(hojas, file) })
  output$d_informe <- downloadHandler(
    filename = function() paste0(nombre_archivo(), "_informe_cata.", if (input$formato == "word") "docx" else "html"),
    content = function(file) withProgress(message = "Armando el informe…", {
      dir <- tempfile(); dir.create(dir); file.copy("informe.Rmd", dir); file.copy("R", dir, recursive = TRUE)
      out <- rmarkdown::render(file.path(dir, "informe.Rmd"),
        output_format = if (input$formato == "word") rmarkdown::word_document() else rmarkdown::html_document(self_contained = TRUE, toc = TRUE),
        params = list(ancho = datos()$ancho, terminos = datos()$terminos, gusto = datos()$gusto, nombre = datos()$nombre, sesiones = input$sesiones,
                      ajuste = input$ajuste, umbral = input$umbral, solo_sig = input$solo_sig_ca, k = input$k),
        envir = new.env(parent = globalenv()), quiet = TRUE)
      file.copy(out, file) }))
  output$codigo_r <- renderText({ a <- A()
    paste0('library(FactoMineR)\n',
      'd <- read.csv("', nombre_archivo(), '_cata.csv", check.names = FALSE)\n',
      'ideal <- d[d$NombreProducto == "IDEAL", ]; d <- d[d$NombreProducto != "IDEAL", ]\n',
      'terminos <- c(', paste0('"', a$terminos, '"', collapse = ", "), ')\n',
      'frec <- aggregate(d[terminos], list(Producto = d$NombreProducto), sum)\n',
      'rownames(frec) <- frec$Producto; frec$Producto <- NULL\n',
      '# Q de Cochran por término (bloques consumidor × repetición)\n',
      'for (t in terminos) { m <- xtabs(d[[t]] ~ paste(d$Valuador, d$Rep) + d$NombreProducto)\n',
      '  k <- ncol(m); C <- colSums(m); R <- rowSums(m); N <- sum(m)\n',
      '  Q <- (k - 1) * (k * sum(C^2) - N^2) / (k * N - sum(R^2)); cat(t, Q, pchisq(Q, k - 1, lower.tail = FALSE), "\\n") }\n',
      'ca <- CA(frec)                                            # mapa de productos y términos\n',
      if (a$gusto) paste0(
      'd$Producto <- factor(d$NombreProducto); d$Consumidor <- factor(d$Valuador)\n',
      'modelo <- aov(Aceptabilidad ~ Producto + Consumidor, data = d); summary(modelo)\n',
      'TukeyHSD(modelo, "Producto")\n',
      '# impacto de un término en el agrado (con vs. sin)\n',
      'for (t in terminos) print(t.test(d$Aceptabilidad[d[[t]] == 1], d$Aceptabilidad[d[[t]] == 0]))\n') else '') })
}

shinyApp(ui, server)
