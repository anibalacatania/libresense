# =====================================================================================
# RATA (Rate-All-That-Apply) · Análisis completo
# App Shiny que acompaña a la app "LibreSense" (https://anibalacatania.github.io/libresense/).
# Se abre desde la administración de un estudio RATA ("Análisis completo en R"),
# o se usa sola subiendo el CSV de resultados que descarga la app. Sirve también para el ADC (escala 0–10).
#
# Métodos: Meyners, Jaeger y Ares (2016) (no marcado = 0 en la misma escala); ANOVA con panelista aleatorio;
# Tukey; Q de Cochran y McNemar sobre la citación; PCA (FactoMineR) con elipses por remuestreo de panelistas;
# desempeño del panel (discriminación, acuerdo, repetibilidad).
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
tema_app <- bs_theme(version = 5, primary = "#db2777", secondary = "#7c3aed", base_font = font_collection("Inter", "system-ui", "sans-serif"))
ayuda <- function(...) tags$p(class = "text-muted small mb-2", ...)
tarjeta <- function(titulo, ..., descargar = NULL) card(full_screen = TRUE,
  card_header(class = "d-flex justify-content-between align-items-center", titulo, descargar), card_body(...))
bajar <- function(id) downloadButton(id, "PNG", class = "btn-sm btn-outline-secondary")
tabla <- function(x, n = 25) datatable(x, rownames = FALSE, options = list(pageLength = n, dom = if (nrow(x) > n) "tp" else "t", scrollX = TRUE))
`%||%` <- function(a, b) if (is.null(a)) b else a

ui <- page_navbar(
  title = tags$b("RATA"), window_title = "RATA · análisis completo", id = "nav", theme = tema_app, fillable = FALSE,
  header = tagList(
    tags$script(HTML(sprintf("window.PF_ORIGENES_EXTRA = %s;", jsonlite::toJSON(origenes_extra)))),
    tags$script(src = "conexion.js"),
    tags$style(HTML(".shiny-output-error-validation{color:#6b7280;font-size:1rem;padding:1rem}"))),
  sidebar = sidebar(width = 290,
    uiOutput("estado"),
    uiOutput("selector_sesiones"),
    uiOutput("selector_modalidades"),
    selectInput("ajuste", "Comparaciones de citación (McNemar)", c("Sin ajuste" = "none", "Holm" = "holm", "Bonferroni" = "bonferroni")),
    hr(),
    fileInput("archivo", "…o subí el CSV de la app", accept = c(".csv", ".txt"), buttonLabel = "Elegir", placeholder = "Resultados (CSV)"),
    ayuda("En la app: Resultados → “Descargar CSV”.")),

  nav_panel("Datos", icon = icon("table"),
    uiOutput("cajas"),
    layout_columns(col_widths = c(7, 5),
      tarjeta("Panelistas", DTOutput("tabla_pan"), ayuda("Resumen del desempeño de cada panelista (detalle en la pestaña Panel).")),
      tarjeta("Controles", uiOutput("controles")))),

  nav_panel("Descriptores", icon = icon("list-check"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Intensidad media", plotOutput("p_medias", height = "620px"), descargar = bajar("d_medias")),
      tarjeta("Cómo leerlo",
        p("Medias con el ", tags$b("0 de “no marcado” incluido"), ", como recomiendan Meyners, Jaeger y Ares (2016): así una media alta puede venir de muchos que lo marcan bajo o de pocos que lo marcan alto."),
        uiOutput("u_modelo"),
        p(tags$b("Letras"), ": productos sin letra en común difieren (Tukey, p < 0,05), con el mismo término de error del ANOVA."),
        checkboxInput("solo_sig_med", "Mostrar solo los descriptores que discriminan (p < 0,05)", FALSE))),
    tarjeta("Tabla", radioButtons("que_tabla", NULL, c("Media (con letras)" = "medias", "% de panelistas que lo marcó" = "cita", "Intensidad media cuando se marcó" = "cond"), inline = TRUE),
      DTOutput("t_medias")),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Un descriptor", selectInput("atr", NULL, choices = NULL), plotOutput("p_atr", height = "380px"), descargar = bajar("d_atr")),
      tarjeta("Producto × panelista", plotOutput("p_inter", height = "440px"), descargar = bajar("d_inter")))),

  nav_panel("Citación", icon = icon("check-double"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Frecuencia de citación", plotOutput("p_citas", height = "620px"), descargar = bajar("d_citas")),
      tarjeta("Cómo leerlo",
        p("Solo presencia/ausencia: qué % de las evaluaciones marcó cada descriptor, sin importar la intensidad. Es el análisis CATA de los mismos datos."),
        p(tags$b("Q de Cochran"), ": ¿un descriptor se marca más en unos productos que en otros? ", tags$b("McNemar"), ": comparación de a pares."),
        p("Comparar con la pestaña Descriptores muestra si la diferencia está en cuántos lo perciben o en cuán intenso lo perciben."),
        checkboxInput("solo_sig_cit", "Solo los que se marcan distinto (p < 0,05)", FALSE)))),

  nav_panel("Mapa", icon = icon("map"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Productos y descriptores", plotOutput("p_pca", height = "640px"), descargar = bajar("d_pca")),
      tarjeta("Opciones",
        selectInput("dims", "Dimensiones", c("1 y 2" = "1,2", "1 y 3" = "1,3", "2 y 3" = "2,3")),
        checkboxInput("solo_sig_pca", "Solo descriptores que discriminan", TRUE),
        selectInput("umbral", "Umbral", c("p < 0,05" = "0.05", "p < 0,10" = "0.1", "p < 0,20" = "0.2"), selected = "0.05"),
        sliderInput("k", "Grupos de productos", 2, 6, 3, step = 1),
        hr(),
        sliderInput("nboot", "Elipses: paneles simulados", 100, 1000, 300, step = 100),
        actionButton("calc_boot", "Calcular elipses de confianza", class = "btn-primary w-100"),
        ayuda("Remuestreo de panelistas (como SensoMineR::panellipse). Productos con elipses que no se superponen fueron diferenciados por el panel."),
        hr(), h6("Inercia por dimensión"), tableOutput("t_inercia"))),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Correlación de cada descriptor con las dimensiones", DTOutput("t_cor")),
      tarjeta("Clasificación de los productos", plotOutput("p_arbol", height = "360px"), tableOutput("t_grupos")))),

  nav_panel("Perfiles", icon = icon("chart-simple"),
    layout_columns(col_widths = c(8, 4),
      tarjeta("Perfil sensorial", plotOutput("p_radar", height = "640px"), descargar = bajar("d_radar")),
      tarjeta("Elegir", uiOutput("sel_radar"),
        ayuda("Gráfico de araña con la intensidad media de cada descriptor. Conviene comparar pocos productos a la vez.")))),

  nav_panel("Panel", icon = icon("users"),
    tarjeta("Resumen por panelista", DTOutput("t_panel"),
      ayuda("Acuerdo: correlación entre las medias del panelista y las del panel (1 = ordena los productos igual que el panel; cerca de 0 o negativo = no coincide).",
            " Discrimina: descriptores en los que el panelista diferencia los productos (ANOVA propio, p < 0,05; necesita repeticiones).",
            " Repetibilidad: desvío entre sus repeticiones (más bajo = más repetible).")),
    tarjeta("Acuerdo con el panel (correlación)", plotOutput("p_acu", height = "520px"), descargar = bajar("d_acu")),
    uiOutput("u_rep")),

  nav_panel("Descargas", icon = icon("download"),
    layout_columns(col_widths = c(6, 6),
      tarjeta("Tablas", p("Excel con medias y letras, ANOVA, citación (Cochran), PCA, desempeño del panel y los datos."),
        downloadButton("d_excel", "Descargar Excel", class = "btn-primary"), br(), br(), downloadButton("d_csv", "Datos (CSV, para R)")),
      tarjeta("Informe", p("Informe con los gráficos y tablas, con las opciones elegidas."),
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
    if (!r$estudio$tecnica %in% c("rata", "descriptiva")) { mensaje("Este estudio no es de RATA ni de ADC."); return() }
    datos(list(ancho = r$ancho, atributos = r$atributos, grupos = r$grupos, escala = r$escala,
               tecnica = r$estudio$tecnica, nombre = r$estudio$nombre %||% "Estudio", fuente = "Firebase (en vivo)"))
    mensaje(NULL)
  }
  observeEvent(input$fb, { x <- input$fb; viejo <- fb(); fb(x); if (is.null(viejo) || !identical(viejo$eid, x$eid) || is.null(datos())) cargar_firebase(x) })
  observeEvent(input$actualizar, { req(fb()); cargar_firebase(fb()) })
  observeEvent(input$archivo, {
    r <- tryCatch(leer_archivo(input$archivo$datapath), error = function(e) e)
    if (inherits(r, "error")) { mensaje(paste("No se pudo leer el archivo:", conditionMessage(r))); return() }
    fb(NULL); datos(c(r, list(tecnica = if (r$escala > 5) "descriptiva" else "rata", nombre = tools::file_path_sans_ext(input$archivo$name), fuente = "Archivo"))); mensaje(NULL)
  })
  output$estado <- renderUI({
    d <- datos(); m <- mensaje()
    tagList(if (!is.null(m)) div(class = "alert alert-info small", m),
      if (is.null(d)) div(class = "small text-muted", if (isTRUE(input$hay_abridor)) "Esperando la sesión de la app…"
        else "Abrí este análisis desde la app (estudio RATA → “Análisis completo en R”) o subí el CSV abajo.")
      else div(h5(class = "mb-0", d$nombre), div(class = "small text-muted mb-2", d$fuente),
        if (!is.null(fb())) actionButton("actualizar", "Actualizar datos", icon = icon("rotate"), class = "btn-sm btn-outline-primary mb-2")))
  })
  output$selector_sesiones <- renderUI({ d <- datos(); req(d); s <- orden_nat(d$ancho$Sesion); if (length(s) < 2) return(NULL)
    checkboxGroupInput("sesiones", "Sesiones", s, selected = s, inline = TRUE) })
  output$selector_modalidades <- renderUI({ d <- datos(); req(d); g <- unique(d$grupos[nzchar(d$grupos)]); if (length(g) < 2) return(NULL)
    checkboxGroupInput("modalidades", "Modalidades", g, selected = g) })

  A <- reactive({
    d <- datos(); validate(need(d, "Todavía no hay datos: abrí el análisis desde la app o subí el CSV."))
    mods <- if (length(unique(d$grupos[nzchar(d$grupos)])) >= 2) input$modalidades else NULL
    r <- tryCatch(armar(d$ancho, d$atributos, d$grupos, input$sesiones, mods), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r
  })
  esc <- reactive(datos()$escala)
  R <- reactive(analisis_medias(A()))
  co <- reactive(cochran(A(), input$ajuste))
  D <- reactive(desempeno(A(), R()))
  observe({ a <- A(); updateSelectInput(session, "atr", choices = a$atributos, selected = if (isolate(input$atr) %in% a$atributos) isolate(input$atr) else a$atributos[1]) })

  # ---------------- Datos
  output$cajas <- renderUI({ a <- A(); nrep <- length(unique(a$d$Rep))
    layout_columns(fill = FALSE,
      value_box("Panelistas", length(a$panelistas), p(paste(nrow(a$d), "evaluaciones")), showcase = icon("users"), theme = "primary"),
      value_box("Productos", length(a$productos), p(if (a$conrep) paste(nrep, "repeticiones") else "sin repeticiones"), showcase = icon("wine-glass"), theme = "secondary"),
      value_box("Descriptores", length(a$atributos), p(sprintf("%d discriminan (p < 0,05)", sum(vapply(a$atributos, function(x) isTRUE(R()[[x]]$an$p < 0.05), TRUE)))), showcase = icon("list-check"), theme = "info"),
      value_box("Escala", if (esc() == 10) "0–10 (ADC)" else sprintf("0–%d", esc()), p("0 = no marcado"), showcase = icon("sliders"), theme = "light")) })
  output$tabla_pan <- renderDT(tabla(D()$resumen))
  output$controles <- renderUI({ a <- A(); dd <- D()$resumen
    malos <- dd$Panelista[!is.na(dd$`Acuerdo medio (r)`) & dd$`Acuerdo medio (r)` < 0.3]
    nunca <- setdiff(a$atributos, a$usados)
    tagList(
      if (length(malos)) div(class = "alert alert-warning small", "Panelistas con poco acuerdo con el panel (r < 0,3): ", paste(malos, collapse = ", "), ". Mirá la pestaña Panel antes de sacar conclusiones."),
      if (!a$conrep) div(class = "alert alert-info small", "Sin repeticiones: el ANOVA usa el residuo como error y no se puede medir la repetibilidad de cada panelista."),
      if (length(nunca)) div(class = "small text-muted mb-1", "Nunca marcados: ", paste(nunca, collapse = ", ")),
      div(class = "small text-muted", sprintf("Descriptores marcados por evaluación: %.1f en promedio.", mean(rowSums(a$d[, a$atributos, drop = FALSE] > 0))))) })

  # ---------------- Descriptores
  output$u_modelo <- renderUI(if (A()$conrep) p(tags$b("ANOVA"), ": producto + panelista + producto × panelista, con el panelista como efecto aleatorio: la F del producto se prueba contra la interacción (más exigente y más general).")
    else p(tags$b("ANOVA"), ": producto + panelista (sin repeticiones, la interacción no se puede separar del error)."))
  g_me <- reactive({ r <- tryCatch(g_medias(A(), R(), esc(), input$solo_sig_med), error = function(e) e); validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  output$p_medias <- renderPlot(g_me(), res = 100)
  output$t_medias <- renderDT(tabla(tabla_medias(A(), R(), input$que_tabla), 50))
  g_at <- reactive({ req(input$atr %in% A()$atributos); g_descriptor(A(), R(), input$atr, esc()) })
  output$p_atr <- renderPlot(g_at(), res = 100)
  g_in <- reactive({ req(input$atr %in% A()$atributos); g_interaccion(A(), input$atr) })
  output$p_inter <- renderPlot(g_in(), res = 100)

  # ---------------- Citación
  g_ci <- reactive({ r <- tryCatch(g_citas(A(), co(), input$solo_sig_cit), error = function(e) e); validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  output$p_citas <- renderPlot(g_ci(), res = 100)

  # ---------------- Mapa
  pca <- reactive({ r <- tryCatch(correr_pca(A(), R(), input$solo_sig_pca, as.numeric(input$umbral)), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  dims <- reactive(as.integer(strsplit(input$dims, ",")[[1]]))
  cla <- reactive(clasificar(pca(), input$k))
  observe({ n <- length(A()$productos); updateSliderInput(session, "k", max = max(2, min(6, n - 1))) })
  boot_par <- eventReactive(input$calc_boot, list(A = A(), pca = pca(), B = input$nboot, d = dims()))
  bt <- reactive({ if (input$calc_boot == 0) return(NULL); e <- boot_par()
    if (!identical(rownames(e$pca$var$coord), rownames(pca()$var$coord)) || !identical(e$d, dims())) return(NULL)
    withProgress(message = "Simulando paneles…", elipses(e$A, e$pca, e$B, e$d)) })
  g_pc <- reactive({ m <- pca(); validate(need(max(dims()) <= ncol(m$ind$coord), "No hay tantas dimensiones.")); g_pca(m, dims(), cla()$cl, bt()) })
  output$p_pca <- renderPlot(g_pc(), res = 100)
  output$t_inercia <- renderTable({ e <- pca()$eig; data.frame(`Dimensión` = seq_len(min(5, nrow(e))), `%` = round(e[seq_len(min(5, nrow(e))), 2], 1), check.names = FALSE) })
  output$t_cor <- renderDT(datatable(round(as.data.frame(pca()$var$cor[, seq_len(min(3, ncol(pca()$var$cor))), drop = FALSE]), 3), options = list(dom = "t", pageLength = 50)))
  plot_arbol <- function() { h <- cla(); plot(h$hc, main = "", xlab = "", sub = "", ylab = "Altura (Ward)"); stats::rect.hclust(h$hc, k = length(unique(h$cl)), border = col_n(length(unique(h$cl)))) }
  output$p_arbol <- renderPlot(plot_arbol(), res = 100)
  output$t_grupos <- renderTable({ cl <- cla()$cl; data.frame(Grupo = sort(unique(cl)), Productos = vapply(sort(unique(cl)), function(k) paste(names(cl)[cl == k], collapse = ", "), "")) })

  # ---------------- Perfiles
  output$sel_radar <- renderUI({ a <- A(); sig <- a$atributos[vapply(a$atributos, function(x) isTRUE(R()[[x]]$an$p < 0.05), TRUE)]
    tagList(checkboxGroupInput("rad_p", "Productos", a$productos, selected = a$productos[seq_len(min(3, length(a$productos)))]),
            selectizeInput("rad_a", "Descriptores", a$atributos, selected = if (length(sig) >= 3) sig else a$atributos, multiple = TRUE)) })
  g_ra <- reactive({ req(input$rad_p, input$rad_a); r <- tryCatch(g_radar(A(), R(), input$rad_p, input$rad_a, esc()), error = function(e) e)
    validate(need(!inherits(r, "error"), if (inherits(r, "error")) conditionMessage(r) else "")); r })
  output$p_radar <- renderPlot(g_ra(), res = 100)

  # ---------------- Panel
  output$t_panel <- renderDT(tabla(D()$resumen))
  g_ac <- reactive(g_calor(D()$acu, "Acuerdo de cada panelista con el panel", "Correlación de sus medias con las del panel, por descriptor (verde = coincide)", "r"))
  output$p_acu <- renderPlot(g_ac(), res = 100)
  output$u_rep <- renderUI({ if (!A()$conrep) return(tarjeta("Discriminación y repetibilidad", p("Se necesitan repeticiones (cada panelista evalúa cada producto más de una vez).")))
    tagList(tarjeta("Discriminación (p del ANOVA de cada panelista)", plotOutput("p_disc", height = "520px"), descargar = bajar("d_disc")),
            tarjeta("Repetibilidad (desvío entre repeticiones)", plotOutput("p_rep", height = "520px"), descargar = bajar("d_rep"))) })
  g_di <- reactive(g_calor(D()$disc, "Discriminación", "p < 0,05 (verde) = el panelista diferencia los productos en ese descriptor", "p"))
  g_re <- reactive(g_calor(D()$rep, "Repetibilidad", "Desvío estándar entre repeticiones del mismo producto (más bajo = más repetible)", "sd"))
  output$p_disc <- renderPlot(g_di(), res = 100); output$p_rep <- renderPlot(g_re(), res = 100)

  # ---------------- Descargas
  png_de <- function(g, w = 10, h = 8) downloadHandler(filename = function() paste0("rata_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
    content = function(file) ggsave(file, g(), width = w, height = h, dpi = 200, bg = "white"))
  output$d_medias <- png_de(g_me, 10, 8); output$d_atr <- png_de(g_at, 9, 5); output$d_inter <- png_de(g_in, 10, 6); output$d_citas <- png_de(g_ci, 10, 8)
  output$d_pca <- png_de(g_pc); output$d_radar <- png_de(g_ra, 9, 8); output$d_acu <- png_de(g_ac, 11, 7); output$d_disc <- png_de(g_di, 11, 7); output$d_rep <- png_de(g_re, 11, 7)
  nombre_archivo <- reactive(gsub("[^A-Za-z0-9_-]+", "_", datos()$nombre))
  output$d_csv <- downloadHandler(function() paste0(nombre_archivo(), "_rata.csv"), function(file) utils::write.csv(datos()$ancho, file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$d_excel <- downloadHandler(function() paste0(nombre_archivo(), "_rata.xlsx"), function(file) {
    a <- A(); r <- R(); dd <- D()
    hojas <- list(Medias = tabla_medias(a, r, "medias"), Citacion = tabla_medias(a, r, "cita"), Intensidad_si_marcado = tabla_medias(a, r, "cond"),
      Cochran = data.frame(Descriptor = a$atributos, Q = round(vapply(a$atributos, function(x) co()$res[[x]]$Q, 0), 3), p = signif(vapply(a$atributos, function(x) co()$res[[x]]$p, 0), 3)),
      Panel = dd$resumen, Acuerdo = data.frame(Panelista = rownames(dd$acu), round(dd$acu, 3), check.names = FALSE), Datos = datos()$ancho)
    if (a$conrep) { hojas$Discriminacion <- data.frame(Panelista = rownames(dd$disc), signif(dd$disc, 3), check.names = FALSE); hojas$Repetibilidad <- data.frame(Panelista = rownames(dd$rep), round(dd$rep, 3), check.names = FALSE) }
    m <- tryCatch(pca(), error = function(e) NULL)
    if (!is.null(m)) { hojas$PCA_productos <- data.frame(Producto = rownames(m$ind$coord), round(m$ind$coord, 4), check.names = FALSE)
      hojas$PCA_descriptores <- data.frame(Descriptor = rownames(m$var$cor), round(m$var$cor, 4), check.names = FALSE) }
    writexl::write_xlsx(hojas, file) })
  output$d_informe <- downloadHandler(
    filename = function() paste0(nombre_archivo(), "_informe_rata.", if (input$formato == "word") "docx" else "html"),
    content = function(file) withProgress(message = "Armando el informe…", {
      dir <- tempfile(); dir.create(dir); file.copy("informe.Rmd", dir); file.copy("R", dir, recursive = TRUE)
      d <- datos(); mods <- if (length(unique(d$grupos[nzchar(d$grupos)])) >= 2) input$modalidades else NULL
      out <- rmarkdown::render(file.path(dir, "informe.Rmd"),
        output_format = if (input$formato == "word") rmarkdown::word_document() else rmarkdown::html_document(self_contained = TRUE, toc = TRUE),
        params = list(ancho = d$ancho, atributos = d$atributos, grupos = d$grupos, escala = d$escala, nombre = d$nombre, sesiones = input$sesiones,
                      modalidades = mods, ajuste = input$ajuste, solo_sig = input$solo_sig_pca, umbral = as.numeric(input$umbral), k = input$k, nboot = input$nboot),
        envir = new.env(parent = globalenv()), quiet = TRUE)
      file.copy(out, file) }))
  output$codigo_r <- renderText({ a <- A()
    paste0('library(FactoMineR); library(SensoMineR)\n',
      'd <- read.csv("', nombre_archivo(), '_rata.csv", check.names = FALSE)\n',
      'd$Producto <- factor(d$NombreProducto); d$Panelista <- factor(d$Valuador)\n',
      'atr <- c(', paste0('"', a$atributos, '"', collapse = ", "), ')\n',
      '# ANOVA por descriptor', if (a$conrep) ' (panelista aleatorio: F del producto contra la interacción)', '\n',
      'for (x in atr) {\n',
      if (a$conrep) '  t <- anova(lm(d[[x]] ~ Panelista * Producto, d))\n  F <- t["Producto", "Mean Sq"] / t["Panelista:Producto", "Mean Sq"]\n  cat(x, F, pf(F, t["Producto", "Df"], t["Panelista:Producto", "Df"], lower.tail = FALSE), "\\n")\n'
      else '  print(anova(lm(d[[x]] ~ Panelista + Producto, d)))\n',
      '}\n',
      'res <- decat(d[, c("Producto", "Panelista", atr)], formul = "~Producto+Panelista", firstvar = 3)   # SensoMineR\n',
      'medias <- aggregate(d[atr], list(Producto = d$Producto), mean); rownames(medias) <- medias$Producto\n',
      'pca <- PCA(medias[, -1], scale.unit = TRUE)\n',
      'panellipse(d[, c("Producto", "Panelista", atr)], col.p = 1, col.j = 2, firstvar = 3)   # elipses\n',
      if (a$conrep) 'paneliperf(d[, c("Producto", "Panelista", "Rep", atr)], formul = "~Producto+Panelista+Rep+Producto:Panelista", formul.j = "~Producto+Rep", col.j = 2, firstvar = 4)   # desempeño\n' else '') })
}

shinyApp(ui, server)
