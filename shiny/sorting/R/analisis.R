# Análisis de Sorting (agrupamiento libre).
# Referencias: Chollet, Valentin y Abdi (2014), cap. "Free sorting task" de Varela y Ares (eds.), Novel Techniques in
# Sensory Characterization and Consumer Profiling; Cadoret, Lê y Pagès (2009) MCA/FAST; Abdi, Valentin, Chollet y
# Chrea (2007) DISTATIS; Cadoret y Husson (2013) elipses por bootstrap.

orden_nat <- function(x) { x <- unique(as.character(x)); x[order(as.numeric(gsub("\\D", "", x)), x, na.last = TRUE)] }

# formato largo → matriz productos × hojas (panelista × sesión) con el grupo, y palabras por producto
armar <- function(largo, sesiones = NULL) {
  d <- largo[!is.na(largo$Grupo), ]
  if (length(sesiones)) d <- d[d$Sesion %in% sesiones, ]
  if (!nrow(d)) stop("No hay respuestas en las sesiones elegidas.")
  varias <- length(unique(d$Sesion)) > 1
  d$Hoja <- if (varias) paste(d$Valuador, d$Sesion, sep = "_") else d$Valuador
  productos <- sort(unique(d$NombreProducto))
  productos <- productos[order(gsub("\\d+", "", productos), suppressWarnings(as.numeric(gsub("\\D", "", productos))), productos)]
  hojas <- orden_nat(d$Hoja)
  G <- matrix(NA_character_, length(productos), length(hojas), dimnames = list(productos, hojas))
  G[cbind(match(d$NombreProducto, productos), match(d$Hoja, hojas))] <- paste0("G", d$Grupo)
  completas <- colSums(is.na(G)) == 0
  excluidas <- hojas[!completas]
  G <- G[, completas, drop = FALSE]
  if (ncol(G) < 2) stop("Se necesitan al menos 2 panelistas que hayan agrupado todas las muestras.")
  # palabras: veces que cada palabra se usó para cada producto
  w <- strsplit(tolower(d$Palabras), ",\\s*"); w <- lapply(w, function(x) unique(trimws(x[nzchar(trimws(x))])))
  pal <- NULL
  if (sum(lengths(w))) {
    pal_l <- data.frame(Producto = rep(d$NombreProducto, lengths(w)), Palabra = unlist(w))
    pal <- as.matrix(table(factor(pal_l$Producto, levels = productos), pal_l$Palabra)); class(pal) <- "matrix"
  }
  list(G = G, d = d, productos = productos, hojas = colnames(G), excluidas = excluidas, pal = pal)
}

# co-ocurrencias: cuántas hojas ponen cada par de productos en el mismo grupo
coocurrencia <- function(G) {
  n <- nrow(G); C <- matrix(0, n, n, dimnames = list(rownames(G), rownames(G)))
  for (j in seq_len(ncol(G))) C <- C + outer(G[, j], G[, j], "==")
  C
}

# ---------------------------------------------------------------- mapas
# MCA (Cadoret, Lê y Pagès, 2009): cada panelista es una variable categórica (sus grupos)
mapa_mca <- function(G, ncp = 5) {
  df <- as.data.frame(lapply(seq_len(ncol(G)), function(j) factor(paste0(colnames(G)[j], "_", G[, j]))), check.names = FALSE)
  names(df) <- colnames(G); rownames(df) <- rownames(G)
  FactoMineR::MCA(df, graph = FALSE, ncp = min(ncp, nrow(G) - 1))
}

# DISTATIS (Abdi et al., 2007): una matriz de distancias por panelista → compromiso ponderado por el acuerdo (RV)
distatis <- function(G) {
  n <- nrow(G); Xi <- diag(n) - matrix(1 / n, n, n)
  S <- lapply(seq_len(ncol(G)), function(j) {
    D <- 1 - outer(G[, j], G[, j], "==")
    s <- -0.5 * Xi %*% D %*% Xi
    s / eigen(s, symmetric = TRUE, only.values = TRUE)$values[1]
  })
  names(S) <- colnames(G)
  k <- length(S)
  tr <- function(a, b) sum(a * b)
  RV <- matrix(1, k, k, dimnames = list(names(S), names(S)))
  for (a in seq_len(k)) for (b in seq_len(k)) if (a < b) RV[a, b] <- RV[b, a] <- tr(S[[a]], S[[b]]) / sqrt(tr(S[[a]], S[[a]]) * tr(S[[b]], S[[b]]))
  e1 <- eigen(RV, symmetric = TRUE)
  p1 <- abs(e1$vectors[, 1]); alfa <- p1 / sum(p1); names(alfa) <- names(S)
  Sp <- Reduce(`+`, Map(`*`, S, alfa))
  e <- eigen(Sp, symmetric = TRUE); pos <- e$values > 1e-10
  lam <- e$values[pos]; U <- e$vectors[, pos, drop = FALSE]
  F <- U %*% diag(sqrt(lam), length(lam)); dimnames(F) <- list(rownames(G), paste0("Dim.", seq_along(lam)))
  proy <- U %*% diag(1 / sqrt(lam), length(lam))
  parciales <- lapply(S, function(s) { x <- s %*% proy; dimnames(x) <- dimnames(F); x })
  # mapa de los panelistas: primeras dos dimensiones de la matriz RV
  pc <- e1$vectors[, 1:2, drop = FALSE] %*% diag(sqrt(pmax(0, e1$values[1:2])), 2); rownames(pc) <- names(S)
  list(F = F, eig = lam, pct = 100 * lam / sum(lam), RV = RV, alfa = alfa, parciales = parciales, panelistas = pc, rv_eig = 100 * e1$values / sum(e1$values))
}

# MDS clásico sobre 1 − proporción de co-ocurrencias
mapa_mds <- function(G) {
  C <- coocurrencia(G); D <- 1 - C / ncol(G)
  m <- stats::cmdscale(D, k = min(5, nrow(G) - 1), eig = TRUE)
  pos <- m$eig[m$eig > 1e-10]
  F <- m$points; colnames(F) <- paste0("Dim.", seq_len(ncol(F)))
  list(F = F, pct = 100 * pos / sum(pos))
}

# coordenadas y % de inercia según el método elegido
coordenadas <- function(G, metodo) {
  switch(metodo,
    mca = { m <- mapa_mca(G); list(F = m$ind$coord, pct = m$eig[, 2], obj = m) },
    distatis = { m <- distatis(G); list(F = m$F, pct = m$pct, obj = m) },
    mds = { m <- mapa_mds(G); list(F = m$F, pct = m$pct, obj = m) })
}

# rotación (y reflexión) de Procrustes de X sobre Y, sin cambiar la escala
procrustes <- function(X, Y) { s <- svd(t(X) %*% Y); X %*% (s$u %*% t(s$v)) }

# bootstrap de panelistas (Cadoret y Husson, 2013): mapas de paneles virtuales llevados al mapa de referencia
bootstrap <- function(G, metodo, ref, B = 200, dims = 1:2) {
  set.seed(123); k <- max(dims)
  Y <- ref[, seq_len(k), drop = FALSE]
  out <- lapply(seq_len(B), function(b) {
    j <- sample(ncol(G), replace = TRUE)
    if (length(unique(j)) < 2) return(NULL)
    Gb <- G[, j, drop = FALSE]; colnames(Gb) <- paste0("b", seq_along(j))
    F <- tryCatch(coordenadas(Gb, metodo)$F, error = function(e) NULL)
    if (is.null(F) || ncol(F) < k) return(NULL)
    X <- procrustes(F[, seq_len(k), drop = FALSE], Y)
    data.frame(Producto = rownames(G), x = X[, dims[1]], y = X[, dims[2]], b = b)
  })
  do.call(rbind, out)
}

# palabras ubicadas en el promedio (ponderado) de los productos donde se usaron
palabras_en_mapa <- function(F, pal, dims = 1:2, minimo = 2) {
  if (is.null(pal)) return(NULL)
  pal <- pal[rownames(F), colSums(pal) >= minimo, drop = FALSE]
  if (!ncol(pal)) return(NULL)
  w <- sweep(pal, 2, colSums(pal), "/")
  data.frame(Palabra = colnames(pal), x = colSums(w * F[, dims[1]]), y = colSums(w * F[, dims[2]]), n = colSums(pal))
}

palabras_por_muestra <- function(pal, proba = 0.05) {
  if (is.null(pal) || ncol(pal) < 2) return(NULL)
  P <- as.data.frame(unclass(pal)); orig <- colnames(pal); names(P) <- paste0("w", seq_along(orig))
  r <- tryCatch(FactoMineR::descfreq(P, proba = proba), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  do.call(rbind, lapply(names(r), function(p) {
    x <- r[[p]]; if (is.null(x) || !nrow(x)) return(NULL)
    data.frame(Muestra = p, Palabra = orig[as.integer(sub("^w", "", rownames(x)))], `% en la muestra` = round(x[, "Intern %"], 1),
               `% total` = round(x[, "glob %"], 1), p = signif(x[, "p.value"], 3), v.test = round(x[, "v.test"], 2), check.names = FALSE)
  }))
}

frecuencia_palabras <- function(pal) {
  if (is.null(pal)) return(data.frame(Termino = character(0), Veces = integer(0)))
  f <- sort(colSums(pal), decreasing = TRUE); data.frame(Termino = names(f), Veces = as.integer(f), stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------- clasificación y panelistas
clasificar <- function(F, k, ndim = 3) {
  X <- F[, seq_len(min(ndim, ncol(F))), drop = FALSE]
  hc <- stats::hclust(stats::dist(X), method = "ward.D2")
  list(hc = hc, cl = stats::cutree(hc, k))
}

# índice de Rand ajustado (Hubert y Arabie) entre dos particiones
rand_ajustado <- function(a, b) {
  t <- table(a, b); n <- sum(t); s <- function(x) sum(choose(x, 2))
  ij <- s(t); ai <- s(rowSums(t)); bj <- s(colSums(t)); esp <- ai * bj / choose(n, 2); mx <- (ai + bj) / 2
  if (mx == esp) return(1)
  (ij - esp) / (mx - esp)
}

panelistas <- function(A, cl) {
  G <- A$G
  data.frame(Panelista = colnames(G), Grupos = apply(G, 2, function(x) length(unique(x))),
             `Acuerdo con la clasificación (Rand ajustado)` = round(apply(G, 2, function(x) rand_ajustado(x, cl)), 3),
             Palabras = vapply(colnames(G), function(h) { x <- A$d$Palabras[(if ("Hoja" %in% names(A$d)) A$d$Hoja else A$d$Valuador) == h]; length(unique(unlist(strsplit(tolower(x[nzchar(x)]), ",\\s*")))) }, 0),
             check.names = FALSE)
}

# duplicados: % de panelistas que pusieron el duplicado junto a su original
duplicados <- function(G) {
  dup <- grep("\\(dup\\)$", rownames(G), value = TRUE)
  if (!length(dup)) return(NULL)
  do.call(rbind, lapply(dup, function(d) { o <- sub("\\s*\\(dup\\)$", "", d); if (!o %in% rownames(G)) return(NULL)
    data.frame(Muestra = o, `Panelistas que lo juntaron con su duplicado` = sum(G[d, ] == G[o, ]), `%` = round(100 * mean(G[d, ] == G[o, ]), 1), check.names = FALSE) }))
}

# acuerdo entre sesiones: RV entre las co-ocurrencias de cada sesión
rv_sesiones <- function(largo) {
  s <- orden_nat(largo$Sesion); if (length(s) < 2) return(NULL)
  C <- lapply(s, function(x) { A <- tryCatch(armar(largo, x), error = function(e) NULL); if (is.null(A)) NULL else coocurrencia(A$G) / ncol(A$G) })
  ok <- !vapply(C, is.null, TRUE); C <- C[ok]; s <- s[ok]; if (length(C) < 2) return(NULL)
  pr <- Reduce(intersect, lapply(C, rownames)); C <- lapply(C, function(x) { x <- x[pr, pr]; n <- nrow(x); Xi <- diag(n) - 1 / n; Xi %*% x %*% Xi })
  R <- outer(seq_along(C), seq_along(C), Vectorize(function(a, b) sum(C[[a]] * C[[b]]) / sqrt(sum(C[[a]]^2) * sum(C[[b]]^2))))
  dimnames(R) <- list(s, s); R
}
