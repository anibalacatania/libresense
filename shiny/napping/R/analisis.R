# Análisis de Napping / mapeo proyectivo (Dehlholm, cap. 9 de "Novel Techniques in Sensory
# Characterization and Consumer Profiling"): coordenadas X, Y de cada panelista + palabras (Ultra Flash Profile).

`%||%` <- function(a, b) if (is.null(a)) b else a
orden_nat <- function(x) gtools::mixedsort(unique(x))
es_dup <- function(x) grepl(" \\(dup\\)$", x)

HOJAS <- list(
  rect = list(nombre = "Rectangular 60 × 40 cm", ancho = 60, alto = 40, redonda = FALSE),
  cuad = list(nombre = "Cuadrada 60 × 60 cm", ancho = 60, alto = 60, redonda = FALSE),
  redonda = list(nombre = "Redonda, 40 cm de diámetro", ancho = 40, alto = 40, redonda = TRUE))
area_hoja <- function(h) { H <- HOJAS[[h]] %||% HOJAS$rect; if (H$redonda) pi * (H$ancho / 2)^2 else H$ancho * H$alto }

# ------------------------------------------------------------------ lectura de archivos
# Acepta la "Matriz para MFA" o el "Formato largo" que descarga la app.
leer_archivo <- function(path) {
  lee <- function(sep, dec) utils::read.csv(path, sep = sep, dec = dec, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  x <- tryCatch(lee(",", "."), error = function(e) NULL)
  if (is.null(x) || ncol(x) < 3) x <- lee(";", ",")
  num <- function(v) as.numeric(gsub(",", ".", v))
  if (all(c("Valuador", "X", "Y") %in% names(x))) {
    x$X <- num(x$X); x$Y <- num(x$Y)
    if (!"NombreProducto" %in% names(x)) x$NombreProducto <- x$CodigoProducto
    if (!"Sesion" %in% names(x)) x$Sesion <- "S1"
    if (!"Tarea" %in% names(x)) x$Tarea <- "Global"
    if (!"Palabras" %in% names(x)) x$Palabras <- ""
    x$Palabras[is.na(x$Palabras)] <- ""
    return(list(largo = x, tareas = orden_nat(x$Tarea), pal_tabla = NULL))
  }
  # matriz: Producto, <grupo>_X, <grupo>_Y, …, y después columnas de palabras (frecuencias)
  prod <- x[[1]]; m <- x[-1]
  cx <- grep("_X$", names(m), value = TRUE); gr <- sub("_X$", "", cx)
  gr <- gr[paste0(gr, "_Y") %in% names(m)]
  if (!length(gr)) stop("No encontré columnas <panelista>_X y <panelista>_Y.")
  largo <- do.call(rbind, lapply(gr, function(g) data.frame(Sesion = "S1", Valuador = g, Tarea = "Global", NombreProducto = prod,
    CodigoProducto = prod, X = num(m[[paste0(g, "_X")]]), Y = num(m[[paste0(g, "_Y")]]), Palabras = "", stringsAsFactors = FALSE)))
  resto <- setdiff(names(m), c(paste0(gr, "_X"), paste0(gr, "_Y")))
  pal <- if (length(resto)) { t <- as.matrix(as.data.frame(lapply(m[resto], num))); dimnames(t) <- list(prod, resto); t } else NULL
  list(largo = largo, tareas = "Global", pal_tabla = pal)
}

# ------------------------------------------------------------------ matrices de trabajo
# coordenadas: filas = muestras; dos columnas (X, Y) por grupo = panelista (+ sesión, + foco si se analizan juntos)
armar <- function(largo, sesiones = NULL, tarea = NULL, pal_tabla = NULL) {
  if (length(sesiones)) largo <- largo[largo$Sesion %in% sesiones, ]
  tareas <- orden_nat(largo$Tarea)
  if (length(tarea) && nzchar(tarea)) largo <- largo[largo$Tarea == tarea, ]
  if (!nrow(largo)) stop("No hay datos para lo elegido.")
  varias_s <- length(unique(largo$Sesion)) > 1; varias_t <- length(unique(largo$Tarea)) > 1
  largo$Grupo <- paste0(largo$Valuador, if (varias_s) paste0("_", largo$Sesion) else "", if (varias_t) paste0("_", largo$Tarea) else "")
  prods <- orden_nat(largo$NombreProducto)
  grupos <- gtools::mixedsort(unique(largo$Grupo)); cols <- list(); ok <- c(); excl <- c()
  for (g in grupos) {
    a <- largo[largo$Grupo == g, ]
    i <- match(prods, a$NombreProducto)
    if (anyNA(i)) { excl <- c(excl, paste(g, "(faltan muestras)")); next }
    if (stats::sd(a$X[i]) + stats::sd(a$Y[i]) == 0) { excl <- c(excl, paste(g, "(todas en el mismo lugar)")); next }
    cols[[paste0(g, "_X")]] <- a$X[i]; cols[[paste0(g, "_Y")]] <- a$Y[i]; ok <- c(ok, g)
  }
  if (length(ok) < 2) stop("Se necesitan al menos 2 panelistas con datos completos.")
  if (length(prods) < 3) stop("Se necesitan al menos 3 muestras.")
  d <- as.data.frame(cols, check.names = FALSE); rownames(d) <- prods
  # palabras: tabla de frecuencia muestra × palabra (con el foco adelante si se analizan varios focos juntos)
  if (is.null(pal_tabla)) {
    filas <- largo[largo$Grupo %in% ok & nzchar(largo$Palabras), ]
    pal <- if (nrow(filas)) {
      ws <- strsplit(filas$Palabras, ",")
      df <- data.frame(p = rep(filas$NombreProducto, lengths(ws)), t = rep(filas$Tarea, lengths(ws)), w = trimws(tolower(unlist(ws))))
      df <- df[nzchar(df$w), ]
      if (varias_t) df$w <- paste0(df$t, ": ", df$w)
      tb <- table(factor(df$p, levels = prods), df$w); m <- matrix(as.numeric(tb), nrow(tb), dimnames = dimnames(tb)); m
    } else NULL
  } else pal <- pal_tabla[prods, , drop = FALSE]
  if (!is.null(pal)) pal <- pal[, colSums(pal) > 0, drop = FALSE]
  if (!is.null(pal) && !ncol(pal)) pal <- NULL
  list(d = d, grupos = ok, excluidas = excl, pal = pal, largo = largo[largo$Grupo %in% ok, ], tareas = tareas)
}

# ------------------------------------------------------------------ análisis
correr_mfa <- function(A) {
  k <- length(A$grupos)
  X <- A$d; grp <- rep(2, k); tipo <- rep("c", k); nom <- A$grupos; sup <- NULL
  if (!is.null(A$pal) && ncol(A$pal) >= 2) {
    P <- as.data.frame(A$pal); names(P) <- paste0("w", seq_len(ncol(P)))   # nombres seguros; se recuperan abajo
    X <- cbind(X, P); grp <- c(grp, ncol(P)); tipo <- c(tipo, "f"); nom <- c(nom, "Palabras"); sup <- k + 1
  }
  m <- FactoMineR::MFA(X, group = grp, type = tipo, name.group = nom, num.group.sup = sup, ncp = min(5, nrow(X) - 1), graph = FALSE)
  if (!is.null(m$freq.sup)) rownames(m$freq.sup$coord) <- colnames(A$pal)
  m
}
correr_hcpc <- function(mfa, k) FactoMineR::HCPC(mfa, nb.clust = k, consol = FALSE, graph = FALSE)

# Elipses según el apéndice del cap. 9 (Dehlholm et al. 2012): bootstrap de los puntos parciales del MFA
elipses_dehlholm <- function(mfa, dims = 1:2, nsim = 500) {
  nm <- rownames(mfa$ind$coord.partiel)
  grupos <- rownames(mfa$group$coord)
  prod <- rep(rownames(mfa$ind$coord), each = length(grupos))
  P <- data.frame(names = factor(prod), mfa$ind$coord.partiel[, dims])
  set.seed(1)
  B <- FactoMineR::simule(P, nb.simul = nsim)
  E <- FactoMineR::coord.ellipse(B$simul, level.conf = 0.95, bary = FALSE)$res   # como en el apéndice del cap. 9
  names(E) <- c("vino", "x", "y"); E
}

# palabras que caracterizan a cada muestra (FactoMineR::descfreq), con los nombres originales
palabras_por_muestra <- function(pal, proba = 0.05) {
  if (is.null(pal) || ncol(pal) < 2) return(NULL)
  P <- as.data.frame(pal); orig <- colnames(pal); names(P) <- paste0("w", seq_along(orig))
  r <- tryCatch(FactoMineR::descfreq(P, proba = proba), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  do.call(rbind, lapply(names(r), function(p) {
    x <- r[[p]]; if (is.null(x) || !nrow(x)) return(NULL)
    data.frame(Muestra = p, Palabra = orig[as.integer(sub("^w", "", rownames(x)))], `% en la muestra` = round(x[, "Intern %"], 1),
               `% total` = round(x[, "glob %"], 1), p = signif(x[, "p.value"], 3), v.test = round(x[, "v.test"], 2), check.names = FALSE)
  }))
}

# Procrustes (rotación + escala, como pmfa) de la hoja de cada panelista sobre el consenso
superponer <- function(A, mfa, dims = 1:2) {
  Y <- scale(mfa$ind$coord[, dims], scale = FALSE)
  do.call(rbind, lapply(A$grupos, function(g) {
    X <- scale(as.matrix(A$d[, paste0(g, c("_X", "_Y"))]), scale = FALSE)
    s <- svd(t(X) %*% Y); R <- s$u %*% t(s$v); k <- sum(s$d) / sum(X^2)
    Z <- k * X %*% R
    data.frame(grupo = g, vino = rownames(A$d), x = Z[, 1], y = Z[, 2], cx = Y[, 1], cy = Y[, 2])
  }))
}

# uso de la hoja: área del polígono que encierra las muestras / área de la hoja (Dehlholm: proyecciones "amontonadas")
uso_hoja <- function(A, hoja) {
  sapply(A$grupos, function(g) {
    x <- A$d[[paste0(g, "_X")]]; y <- A$d[[paste0(g, "_Y")]]; h <- grDevices::chull(x, y)
    a <- abs(sum(x[h] * c(y[h][-1], y[h][1]) - c(x[h][-1], x[h][1]) * y[h])) / 2
    100 * a / area_hoja(hoja)
  })
}

# índice de desempeño (Hopfer y Heymann, 2013): dist(duplicado, original) / dist. máxima entre muestras del panelista
indice_desempeno <- function(A) {
  p <- rownames(A$d); dups <- p[es_dup(p) & sub(" \\(dup\\)$", "", p) %in% p]
  if (!length(dups)) return(NULL)
  out <- sapply(A$grupos, function(g) {
    Z <- as.matrix(A$d[, paste0(g, c("_X", "_Y"))]); D <- as.matrix(stats::dist(Z)); dimnames(D) <- list(p, p)
    mean(vapply(dups, function(dd) D[dd, sub(" \\(dup\\)$", "", dd)] / max(D), 0))
  })
  out
}

# duplicados en el consenso: distancia al original vs. distancias entre muestras distintas
duplicados <- function(mfa) {
  X <- mfa$ind$coord; p <- rownames(X); dups <- p[es_dup(p)]
  if (!length(dups)) return(NULL)
  orig <- p[!es_dup(p)]; D <- as.matrix(stats::dist(X)); entre <- D[orig, orig][upper.tri(D[orig, orig])]
  do.call(rbind, lapply(dups, function(dd) { o <- sub(" \\(dup\\)$", "", dd); if (!o %in% p) return(NULL)
    stats::setNames(data.frame(dd, o, round(D[dd, o], 3), round(100 * mean(entre <= D[dd, o]), 1)),
                    c("Duplicado", "Muestra", "Distancia en el consenso", "Percentil entre muestras distintas")) }))
}

rv_sesiones <- function(largo, tarea) {
  ses <- orden_nat(largo$Sesion); if (length(ses) < 2) return(NULL)
  conf <- lapply(ses, function(s) tryCatch(correr_mfa(armar(largo, s, tarea))$ind$coord, error = function(e) NULL))
  names(conf) <- ses; conf <- conf[!vapply(conf, is.null, TRUE)]; if (length(conf) < 2) return(NULL)
  com <- Reduce(intersect, lapply(conf, rownames))
  R <- outer(names(conf), names(conf), Vectorize(function(a, b) FactoMineR::coeffRV(conf[[a]][com, , drop = FALSE], conf[[b]][com, , drop = FALSE])$rv))
  dimnames(R) <- list(names(conf), names(conf)); round(R, 3)
}

frecuencia_palabras <- function(pal) {
  if (is.null(pal)) return(data.frame(Termino = character(0), Panelistas = integer(0)))
  f <- sort(colSums(pal), decreasing = TRUE); data.frame(Termino = names(f), Panelistas = as.integer(f), stringsAsFactors = FALSE)
}
