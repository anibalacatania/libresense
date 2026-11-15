# Análisis de CATA con aceptabilidad.
# Referencias: Meyners y Castura (2014), cap. 11 de Varela y Ares (eds.), Novel Techniques in Sensory
# Characterization and Consumer Profiling; Ares y Jaeger (2015), cap. 11 de Rapid Sensory Profiling Techniques; Meyners, Castura y Carr (2013); Ares, Dauber, Fernández, Giménez y
# Varela (2014) penalidades con producto ideal; Meyners y Castura (2014) Cochran y McNemar.

HEDONICA <- c("Me disgusta muchísimo", "Me disgusta mucho", "Me disgusta moderadamente", "Me disgusta levemente",
              "Ni me gusta ni me disgusta", "Me gusta levemente", "Me gusta moderadamente", "Me gusta mucho", "Me gusta muchísimo")

orden_nat <- function(x) { x <- unique(as.character(x)); x[order(as.numeric(gsub("\\D", "", x)), x, na.last = TRUE)] }

# Prepara los datos: evaluaciones de productos, ideal de cada consumidor, filtros por sesión
armar <- function(ancho, terminos, gusto, sesiones = NULL) {
  d <- ancho
  if (length(sesiones)) d <- d[d$Sesion %in% sesiones | d$NombreProducto == "IDEAL", ]
  ideal <- d[d$NombreProducto == "IDEAL", ]
  ideal <- ideal[!duplicated(ideal$Valuador), ]
  ev <- d[d$NombreProducto != "IDEAL", ]
  if (!nrow(ev)) stop("No hay evaluaciones en las sesiones elegidas.")
  productos <- sort(unique(ev$NombreProducto))
  productos <- productos[order(gsub("\\d+", "", productos), as.numeric(gsub("\\D", "", productos)), productos)]
  ev$Producto <- factor(ev$NombreProducto, levels = productos)
  ev$Consumidor <- factor(ev$Valuador, levels = orden_nat(ev$Valuador))
  ev$Bloque <- paste(ev$Valuador, ev$Rep, sep = "_")
  usados <- terminos[colSums(ev[, terminos, drop = FALSE]) > 0]
  gusto <- gusto && sum(!is.na(ev$Aceptabilidad)) > 0
  list(ev = ev, ideal = if (nrow(ideal)) ideal else NULL, terminos = terminos, usados = usados, gusto = gusto,
       productos = productos, consumidores = levels(ev$Consumidor))
}

# ---------------------------------------------------------------- letras (compact letter display)
# sig: matriz lógica simétrica (TRUE = difieren); medias: para ordenar (la letra "a" para el más alto)
letras <- function(sig, medias) {
  nm <- names(medias); sig <- sig[nm, nm, drop = FALSE]; sig[is.na(sig)] <- FALSE
  cols <- list(nm)
  if (length(nm) > 1) for (i in seq_along(nm)) for (j in seq_along(nm)) if (j > i && sig[i, j]) {
    nuevas <- list()
    for (cc in cols) {
      if (nm[i] %in% cc && nm[j] %in% cc) nuevas <- c(nuevas, list(setdiff(cc, nm[i]), setdiff(cc, nm[j]))) else nuevas <- c(nuevas, list(cc))
    }
    nuevas <- unique(lapply(nuevas, sort))
    quedan <- vapply(seq_along(nuevas), function(k) !any(vapply(seq_along(nuevas), function(l) l != k && all(nuevas[[k]] %in% nuevas[[l]]) && length(nuevas[[l]]) > length(nuevas[[k]]), TRUE)), TRUE)
    cols <- nuevas[quedan]
  }
  # ordenar las columnas por la media más alta que contienen
  cols <- cols[order(-vapply(cols, function(cc) max(medias[cc]), 0))]
  out <- vapply(nm, function(v) paste(letters[which(vapply(cols, function(cc) v %in% cc, TRUE))], collapse = ""), "")
  out
}

# ---------------------------------------------------------------- términos
frecuencias <- function(A) {
  t(sapply(A$productos, function(p) colSums(A$ev[A$ev$Producto == p, A$terminos, drop = FALSE])))
}

# Q de Cochran por término (bloques completos consumidor × repetición) y McNemar exacto de a pares
cochran <- function(A, ajuste = "none") {
  ev <- A$ev; P <- A$productos; k <- length(P)
  bl <- split(ev, ev$Bloque)
  completos <- bl[vapply(bl, function(b) all(P %in% b$NombreProducto), TRUE)]
  res <- lapply(A$terminos, function(t) {
    if (k < 2 || length(completos) < 2) return(list(Q = NA, p = NA, letras = stats::setNames(rep("", k), P)))
    M <- t(vapply(completos, function(b) vapply(P, function(p) as.numeric(b[[t]][match(p, b$NombreProducto)] > 0), 0), numeric(k)))
    C <- colSums(M); R <- rowSums(M); N <- sum(M)
    den <- k * N - sum(R^2)
    Q <- if (den > 0) (k - 1) * (k * sum(C^2) - N^2) / den else 0
    p <- if (den > 0) stats::pchisq(Q, k - 1, lower.tail = FALSE) else 1
    # McNemar exacto (binomial sobre los pares discordantes) entre productos
    pv <- matrix(1, k, k, dimnames = list(P, P))
    for (i in 1:(k - 1)) for (j in (i + 1):k) {
      b <- sum(M[, i] == 1 & M[, j] == 0); c <- sum(M[, i] == 0 & M[, j] == 1)
      pv[i, j] <- pv[j, i] <- if (b + c > 0) stats::binom.test(b, b + c)$p.value else 1
    }
    sup <- upper.tri(pv); pv[sup] <- stats::p.adjust(pv[sup], ajuste); pv[lower.tri(pv)] <- t(pv)[lower.tri(pv)]
    list(Q = Q, p = p, letras = letras(pv < 0.05, stats::setNames(C, P)))
  })
  names(res) <- A$terminos
  list(res = res, bloques = length(completos))
}

tabla_terminos <- function(A, coch) {
  F <- frecuencias(A); n <- table(A$ev$Producto)[A$productos]
  out <- data.frame(Termino = A$terminos, check.names = FALSE)
  for (p in A$productos) out[[p]] <- sprintf("%d (%.0f%%)%s", F[p, A$terminos], 100 * F[p, A$terminos] / n[[p]],
                                            vapply(A$terminos, function(t) { l <- coch$res[[t]]$letras[[p]]; if (nzchar(l) && !is.na(coch$res[[t]]$p) && coch$res[[t]]$p < 0.05) paste0(" ", l) else "" }, ""))
  if (!is.null(A$ideal)) out[["Ideal"]] <- sprintf("%d (%.0f%%)", colSums(A$ideal[, A$terminos, drop = FALSE]), 100 * colMeans(A$ideal[, A$terminos, drop = FALSE]))
  out$Q <- round(vapply(A$terminos, function(t) coch$res[[t]]$Q, 0), 2)
  out$p <- signif(vapply(A$terminos, function(t) coch$res[[t]]$p, 0), 3)
  tot <- colSums(F[, A$terminos, drop = FALSE])
  out[order(out$p, -tot, na.last = TRUE), ]
}

# ---------------------------------------------------------------- mapa (análisis de correspondencias)
correr_ca <- function(A, solo_sig = FALSE, coch = NULL, ideal = TRUE, agrado = TRUE) {
  F <- frecuencias(A)
  ts <- A$usados
  if (solo_sig && !is.null(coch)) ts <- ts[vapply(ts, function(t) isTRUE(coch$res[[t]]$p < 0.05), TRUE)]
  if (length(ts) < 2 || length(A$productos) < 3) stop("El mapa necesita al menos 3 productos y 2 términos marcados.")
  X <- as.data.frame(F[, ts, drop = FALSE], check.names = FALSE)
  sup_row <- NULL; sup_q <- NULL
  if (ideal && !is.null(A$ideal)) { X <- rbind(X, Ideal = colSums(A$ideal[, ts, drop = FALSE])); sup_row <- nrow(X) }
  if (agrado && A$gusto) {
    m <- tapply(A$ev$Aceptabilidad, A$ev$Producto, mean, na.rm = TRUE)
    X$Aceptabilidad <- c(m[A$productos], if (!is.null(sup_row)) NA)
    sup_q <- ncol(X)
    if (!is.null(sup_row)) X$Aceptabilidad[sup_row] <- mean(m, na.rm = TRUE)
  }
  FactoMineR::CA(X, row.sup = sup_row, quanti.sup = sup_q, graph = FALSE, ncp = 5)
}

# ---------------------------------------------------------------- aceptabilidad
agrado <- function(A) {
  ev <- A$ev[!is.na(A$ev$Aceptabilidad), ]
  if (!nrow(ev)) return(NULL)
  m <- tapply(ev$Aceptabilidad, ev$Producto, mean); s <- tapply(ev$Aceptabilidad, ev$Producto, stats::sd); n <- tapply(ev$Aceptabilidad, ev$Producto, length)
  tab <- data.frame(Producto = A$productos, n = as.integer(n[A$productos]), Media = round(m[A$productos], 2), DE = round(s[A$productos], 2),
                    EE = round(s[A$productos] / sqrt(n[A$productos]), 3),
                    `Le gusta (6-9) %` = round(100 * tapply(ev$Aceptabilidad >= 6, ev$Producto, mean)[A$productos], 1),
                    `Indiferente (5) %` = round(100 * tapply(ev$Aceptabilidad == 5, ev$Producto, mean)[A$productos], 1),
                    `Le disgusta (1-4) %` = round(100 * tapply(ev$Aceptabilidad <= 4, ev$Producto, mean)[A$productos], 1), check.names = FALSE)
  multi <- length(unique(ev$Consumidor)) > 1
  modelo <- if (multi) stats::aov(Aceptabilidad ~ Producto + Consumidor, data = ev) else stats::aov(Aceptabilidad ~ Producto, data = ev)
  an <- summary(modelo)[[1]]
  tk <- tryCatch(stats::TukeyHSD(modelo, "Producto")$Producto, error = function(e) NULL)
  # TukeyHSD ordena los pares como combn(): 2-1, 3-1, …, 3-2, …
  P <- A$productos; sig <- matrix(FALSE, length(P), length(P), dimnames = list(P, P))
  if (!is.null(tk) && nrow(tk) == choose(length(P), 2)) { pares <- utils::combn(P, 2)
    for (q in seq_len(ncol(pares))) sig[pares[1, q], pares[2, q]] <- sig[pares[2, q], pares[1, q]] <- tk[q, "p adj"] < 0.05 }
  tab$Grupo <- letras(sig, stats::setNames(tab$Media, tab$Producto))
  list(tabla = tab, anova = an, tukey = tk, datos = ev)
}

# ---------------------------------------------------------------- penalidades
welch <- function(a, b) if (length(a) >= 2 && length(b) >= 2 && (stats::var(a) + stats::var(b)) > 0) stats::t.test(a, b)$p.value else NA

# impacto de cada término en el agrado (con − sin), todas las evaluaciones o un producto
impacto <- function(A, producto = "") {
  ev <- A$ev[!is.na(A$ev$Aceptabilidad), ]; if (nzchar(producto)) ev <- ev[ev$NombreProducto == producto, ]
  do.call(rbind, lapply(A$terminos, function(t) {
    con <- ev$Aceptabilidad[ev[[t]] > 0]; sin <- ev$Aceptabilidad[!(ev[[t]] > 0)]
    data.frame(Termino = t, `% marcado` = round(100 * length(con) / max(1, nrow(ev)), 1),
               `Agrado con` = if (length(con)) round(mean(con), 2) else NA, `Agrado sin` = if (length(sin)) round(mean(sin), 2) else NA,
               Diferencia = if (length(con) && length(sin)) round(mean(con) - mean(sin), 2) else NA, p = signif(welch(con, sin), 3), check.names = FALSE)
  }))
}

# penalidades respecto del ideal (Ares et al., 2014)
penal_ideal <- function(A, producto = "", umbral = 20) {
  if (is.null(A$ideal)) return(NULL)
  ev <- A$ev[!is.na(A$ev$Aceptabilidad) & A$ev$Valuador %in% A$ideal$Valuador, ]
  if (nzchar(producto)) ev <- ev[ev$NombreProducto == producto, ]
  if (!nrow(ev)) return(NULL)
  id <- A$ideal[match(ev$Valuador, A$ideal$Valuador), ]
  do.call(rbind, lapply(A$terminos, function(t) {
    i <- id[[t]] > 0; x <- ev[[t]] > 0; y <- ev$Aceptabilidad
    g11 <- y[i & x]; g10 <- y[i & !x]; g00 <- y[!i & !x]; g01 <- y[!i & x]
    cf <- if (length(g11) && length(g10)) mean(g11) - mean(g10) else NA
    cs <- if (length(g00) && length(g01)) mean(g00) - mean(g01) else NA
    pf <- welch(g11, g10); ps <- welch(g00, g01)
    pctf <- 100 * length(g10) / nrow(ev); pcts <- 100 * length(g01) / nrow(ev)
    concl <- c(if (isTRUE(pctf >= umbral & pf < 0.05 & cf > 0)) "Debe estar (su falta penaliza)",
               if (isTRUE(pcts >= umbral & ps < 0.05 & cs > 0)) "No debe estar (penaliza)")
    data.frame(Termino = t, `En el ideal %` = round(100 * mean(A$ideal[[t]] > 0), 1),
               `Falta %` = round(pctf, 1), `Caída si falta` = round(cf, 2), `p falta` = signif(pf, 3),
               `Sobra %` = round(pcts, 1), `Caída si sobra` = round(cs, 2), `p sobra` = signif(ps, 3),
               Conclusion = paste(concl, collapse = " · "), check.names = FALSE)
  }))
}

# diferencia de cada producto con el ideal: % de consumidores que marcó el término en el ideal − en el producto
brecha_ideal <- function(A) {
  if (is.null(A$ideal)) return(NULL)
  pi <- colMeans(A$ideal[, A$terminos, drop = FALSE] > 0)
  F <- frecuencias(A); n <- as.numeric(table(A$ev$Producto)[A$productos])
  round(100 * sweep(-(F[, A$terminos, drop = FALSE] / n), 2, pi, "+"), 1)
}

# ---------------------------------------------------------------- consumidores
# matriz consumidores × productos (promedio de repeticiones) con todos los productos evaluados
matriz_agrado <- function(A) {
  ev <- A$ev[!is.na(A$ev$Aceptabilidad), ]
  M <- tapply(ev$Aceptabilidad, list(as.character(ev$Consumidor), as.character(ev$Producto)), mean)
  M <- M[stats::complete.cases(M), A$productos[A$productos %in% colnames(M)], drop = FALSE]
  M
}
segmentos <- function(A, k = 3) {
  M <- matriz_agrado(A)
  if (nrow(M) < max(4, k + 1) || ncol(M) < 2) stop("Se necesitan más consumidores que evaluaron todos los productos.")
  C <- sweep(M, 1, rowMeans(M))                         # centrado por consumidor: forma de la preferencia
  hc <- stats::hclust(stats::dist(C), method = "ward.D2")
  cl <- stats::cutree(hc, k)
  medias <- t(sapply(sort(unique(cl)), function(g) colMeans(M[cl == g, , drop = FALSE])))
  rownames(medias) <- paste0("Segmento ", sort(unique(cl)), " (n=", as.numeric(table(cl)), ")")
  list(M = M, hc = hc, cl = cl, medias = medias)
}
# mapa de preferencias interno: PCA productos × consumidores (agrado estandarizado por consumidor)
mapa_pref <- function(A) {
  M <- matriz_agrado(A)
  M <- M[apply(M, 1, stats::sd) > 0, , drop = FALSE]
  if (nrow(M) < 3 || ncol(M) < 3) stop("Se necesitan al menos 3 productos y 3 consumidores con agrado variable.")
  FactoMineR::PCA(t(M), scale.unit = TRUE, graph = FALSE)
}
