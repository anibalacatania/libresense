# Lectura de un estudio RATA (o ADC) desde Firestore (API REST), con el token de la sesión del administrador
# que la app "LibreSense" le pasa a Shiny. Las reglas de seguridad de Firebase se aplican igual que en
# la app: sin un token de administrador (dueño del estudio o administrador total) no se puede leer nada privado.

fs_base <- function(proyecto) {
  b <- Sys.getenv("PF_FIRESTORE_BASE", "https://firestore.googleapis.com/v1")  # (solo para pruebas)
  sprintf("%s/projects/%s/databases/(default)/documents", b, proyecto)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

fs_valor <- function(v) {
  if (is.null(v)) return(NULL)
  switch(names(v)[1],
    stringValue = v$stringValue,
    integerValue = as.numeric(v$integerValue),
    doubleValue = as.numeric(v$doubleValue),
    booleanValue = isTRUE(v$booleanValue),
    nullValue = NA,
    timestampValue = v$timestampValue,
    mapValue = lapply(v$mapValue$fields %||% list(), fs_valor),
    arrayValue = lapply(v$arrayValue$values %||% list(), fs_valor),
    NA)
}
fs_doc <- function(d) lapply(d$fields %||% list(), fs_valor)

fs_pedir <- function(url, token, query = list()) {
  req <- httr2::request(url) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_timeout(60) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  st <- httr2::resp_status(resp)
  if (st == 401 || st == 403) stop("Firebase no dio permiso (la sesión venció o no es un administrador de este estudio). Volvé a abrir el análisis desde la app.")
  if (st == 404) stop("No se encontró el estudio en Firebase.")
  if (st >= 400) stop(sprintf("Error %s al leer Firebase.", st))
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

# Lee estudio + diseño privado + respuestas y arma la tabla ancha (una fila por evaluación)
fs_leer_estudio <- function(proyecto, eid, token) {
  base <- fs_base(proyecto)
  est <- fs_doc(fs_pedir(sprintf("%s/estudios/%s", base, eid), token))
  priv <- fs_doc(fs_pedir(sprintf("%s/estudios/%s/privado/diseno", base, eid), token))
  docs <- list(); tok <- NULL
  repeat {
    q <- list(pageSize = 300); if (!is.null(tok)) q$pageToken <- tok
    r <- fs_pedir(sprintf("%s/estudios/%s/respuestas", base, eid), token, q)
    docs <- c(docs, r$documents %||% list())
    tok <- r$nextPageToken
    if (is.null(tok)) break
  }
  atributos <- unlist(est$atributos %||% list())
  list(estudio = est, atributos = atributos, grupos = grupos_de(est$estructura, atributos),
       escala = if (identical(est$tecnica, "descriptiva")) 10 else { e <- suppressWarnings(as.numeric(est$escala)); if (length(e) != 1 || is.na(e)) 3 else e },
       ancho = armar_ancho(priv, lapply(docs, fs_doc), atributos))
}

# modalidad (# Aroma, # Boca…) de cada descriptor, según la lista del estudio
grupos_de <- function(estructura, atributos) {
  g <- stats::setNames(rep("", length(atributos)), atributos)
  if (!is.list(estructura) || !length(estructura)) return(g)   # sin grupos: Firebase la devuelve vacía (NA)
  g1 <- ""
  for (it in estructura) {
    if (!is.list(it)) next
    if (identical(it$t, "g1")) g1 <- it$n %||% ""
    else if (identical(it$t, "a") && !is.null(it$k) && it$k %in% atributos) g[[it$k]] <- g1
  }
  g
}

armar_ancho <- function(priv, resp, atributos) {
  if (!length(resp)) stop("El estudio todavía no tiene respuestas.")
  filas <- strsplit(unlist(priv$filas %||% list()), "|", fixed = TRUE)
  info <- if (length(filas)) {
    m <- do.call(rbind, lapply(filas, function(x) x[1:6]))
    data.frame(Sesion = m[, 1], Valuador = m[, 2], Orden = as.numeric(m[, 3]), CodigoProducto = m[, 4],
               NombreProducto = m[, 5], Rep = m[, 6], stringsAsFactors = FALSE)
  } else data.frame(Sesion = character(), Valuador = character(), Orden = numeric(), CodigoProducto = character(), NombreProducto = character(), Rep = character())
  d <- do.call(rbind, lapply(resp, function(r) {
    v <- r$valores %||% list()
    x <- data.frame(Sesion = r$Sesion, Valuador = r$Valuador, Orden = as.numeric(r$Orden), CodigoProducto = r$CodigoProducto, stringsAsFactors = FALSE)
    for (a in atributos) x[[a]] <- as.numeric(v[[a]] %||% 0)
    x
  }))
  d <- merge(d, info, by = c("Sesion", "Valuador", "Orden", "CodigoProducto"), all.x = TRUE)
  d$NombreProducto[is.na(d$NombreProducto)] <- d$CodigoProducto[is.na(d$NombreProducto)]
  d$Rep[is.na(d$Rep)] <- "1"
  d[order(d$Sesion, d$Valuador, d$Orden), c("Sesion", "Valuador", "Orden", "NombreProducto", "Rep", "CodigoProducto", atributos)]
}

# CSV descargado de la app (Resultados): para R (coma) o para Excel (punto y coma, coma decimal)
leer_archivo <- function(ruta) {
  l1 <- readLines(ruta, n = 1, encoding = "UTF-8", warn = FALSE)
  d <- if (grepl(";", l1)) utils::read.csv2(ruta, check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
       else utils::read.csv(ruta, check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
  names(d) <- sub("^﻿", "", names(d))
  if (!all(c("Valuador", "NombreProducto") %in% names(d))) stop("No parece el CSV de resultados de un estudio RATA (faltan las columnas Valuador y NombreProducto).")
  fijas <- c("Estudio", "Sesion", "Valuador", "Orden", "NombreProducto", "Rep", "CodigoProducto", "timestamp")
  atributos <- setdiff(names(d), fijas)
  if (length(atributos) < 2) stop("No se encontraron columnas de descriptores.")
  for (c in c("Sesion", "Rep", "CodigoProducto")) if (!c %in% names(d)) d[[c]] <- if (c == "Sesion") "S1" else if (c == "Rep") "1" else d$NombreProducto
  if (!"Orden" %in% names(d)) d$Orden <- seq_len(nrow(d))
  for (a in atributos) { x <- suppressWarnings(as.numeric(d[[a]])); x[is.na(x)] <- 0; d[[a]] <- x }
  d$Sesion <- as.character(d$Sesion); d$Rep <- as.character(d$Rep); d$Valuador <- as.character(d$Valuador)
  mx <- max(unlist(d[atributos]), na.rm = TRUE)
  list(atributos = atributos, grupos = stats::setNames(rep("", length(atributos)), atributos),
       escala = if (mx > 5) 10 else if (mx > 3) 5 else 3,
       ancho = d[, c("Sesion", "Valuador", "Orden", "NombreProducto", "Rep", "CodigoProducto", atributos)])
}
