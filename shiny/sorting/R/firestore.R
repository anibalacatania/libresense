# Lectura de un estudio de Sorting desde Firestore (API REST), con el token de la sesión del administrador
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

# Lee estudio + diseño privado + respuestas y arma el formato largo (una fila por panelista × muestra)
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
  list(estudio = est, largo = armar_largo(priv, lapply(docs, fs_doc)))
}

armar_largo <- function(priv, resp) {
  if (!length(resp)) stop("El estudio todavía no tiene respuestas.")
  filas <- strsplit(unlist(priv$filas %||% list()), "|", fixed = TRUE)
  info <- if (length(filas)) {
    m <- do.call(rbind, lapply(filas, function(x) x[1:6]))
    data.frame(Sesion = m[, 1], Valuador = m[, 2], Orden = as.numeric(m[, 3]), CodigoProducto = m[, 4],
               NombreProducto = m[, 5], Rep = m[, 6], stringsAsFactors = FALSE)
  } else data.frame(Sesion = character(), Valuador = character(), Orden = numeric(), CodigoProducto = character(), NombreProducto = character(), Rep = character())
  d <- do.call(rbind, lapply(resp, function(r) {
    data.frame(Sesion = r$Sesion, Valuador = r$Valuador, Orden = as.numeric(r$Orden), CodigoProducto = r$CodigoProducto,
               Grupo = as.numeric((r$valores %||% list())$Grupo %||% NA),
               Palabras = gsub(";\\s*", ", ", as.character((r$def %||% list())$Palabras %||% "")), stringsAsFactors = FALSE)
  }))
  d <- merge(d, info, by = c("Sesion", "Valuador", "Orden", "CodigoProducto"), all.x = TRUE)
  d$NombreProducto[is.na(d$NombreProducto)] <- d$CodigoProducto[is.na(d$NombreProducto)]
  d[order(d$Sesion, d$Valuador, d$Grupo, d$Orden), c("Sesion", "Valuador", "Orden", "NombreProducto", "CodigoProducto", "Grupo", "Palabras")]
}

# Archivo de la app: formato largo (Valuador, NombreProducto, Grupo, Palabras) o matriz de grupos (Producto + una columna por panelista)
leer_archivo <- function(ruta) {
  l1 <- readLines(ruta, n = 1, encoding = "UTF-8", warn = FALSE)
  d <- if (grepl(";", l1) && !grepl(",", l1)) utils::read.csv2(ruta, check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
       else utils::read.csv(ruta, check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
  names(d) <- sub("^﻿", "", names(d))
  if (all(c("Valuador", "NombreProducto", "Grupo") %in% names(d))) {
    if (!"Sesion" %in% names(d)) d$Sesion <- "S1"
    if (!"Palabras" %in% names(d)) d$Palabras <- ""
    if (!"Orden" %in% names(d)) d$Orden <- seq_len(nrow(d))
    if (!"CodigoProducto" %in% names(d)) d$CodigoProducto <- d$NombreProducto
    d$Grupo <- suppressWarnings(as.numeric(gsub("\\D", "", d$Grupo))); d$Orden <- as.numeric(d$Orden)
    d$Palabras[is.na(d$Palabras)] <- ""
    return(d[, c("Sesion", "Valuador", "Orden", "NombreProducto", "CodigoProducto", "Grupo", "Palabras")])
  }
  # matriz: primera columna = producto; el resto, un panelista por columna (G1, G2… o palabras)
  prod <- d[[1]]; pans <- names(d)[-1]
  if (!length(pans)) stop("No se reconoce el archivo: subí el formato largo o la matriz de grupos de la app.")
  do.call(rbind, lapply(pans, function(v) {
    x <- d[[v]]; niveles <- unique(x[nzchar(x)])
    data.frame(Sesion = "S1", Valuador = v, Orden = seq_along(prod), NombreProducto = prod, CodigoProducto = prod,
               Grupo = match(x, niveles), Palabras = ifelse(grepl("^G\\d+$", x), "", gsub("\\s+", ", ", x)), stringsAsFactors = FALSE)
  }))
}
