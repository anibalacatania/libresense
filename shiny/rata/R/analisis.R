# Análisis de RATA (Rate-All-That-Apply). También sirve para el ADC (escala 0–10).
# Referencias: Ares, Bruzzone, Vidal y otros (2014) RATA; Meyners, Jaeger y Ares (2016) "On the analysis of RATA data"
# (los descriptores no marcados se analizan como 0 en la misma escala); Meyners, Castura y Carr (2013) Cochran/McNemar;
# Lê y Husson (2008) SensoMineR (panellipse); Næs, Brockhoff y Tomic (2010) desempeño del panel.

orden_nat <- function(x) { x <- unique(as.character(x)); x[order(suppressWarnings(as.numeric(gsub("\\D", "", x))), x, na.last = TRUE)] }

# Prepara los datos: filtro por sesión y por modalidad (# Aroma, # Boca…)
armar <- function(ancho, atributos, grupos, sesiones = NULL, modalidades = NULL) {
  d <- ancho
  if (length(sesiones)) d <- d[d$Sesion %in% sesiones, ]
  if (!nrow(d)) stop("No hay evaluaciones en las sesiones elegidas.")
  usar <- atributos
  if (length(modalidades) && any(nzchar(grupos))) usar <- atributos[grupos[atributos] %in% modalidades]
  if (!length(usar)) stop("No hay descriptores en las modalidades elegidas.")
  productos <- orden_nat(d$NombreProducto)
  d$Producto <- factor(d$NombreProducto, levels = productos)
  d$Panelista <- factor(d$Valuador, levels = orden_nat(d$Valuador))
  d$Rep <- factor(d$Rep)
  d$Bloque <- paste(d$Valuador, d$Rep, sep = "_")
  usados <- usar[vapply(usar, function(a) stats::var(d[[a]]) > 0, TRUE)]
  conrep <- any(table(d$Panelista, d$Producto) > 1)
  list(d = d, atributos = usar, usados = usados, grupos = grupos[usar], productos = productos,
       panelistas = levels(d$Panelista), conrep = conrep)
}

# ---------------------------------------------------------------- letras (compact letter display)
letras <- function(sig, medias) {
  nm <- names(medias); sig <- sig[nm, nm, drop = FALSE]; sig[is.na(sig)] <- FALSE
  cols <- list(nm)
  if (length(nm) > 1) for (i in seq_along(nm)) for (j in seq_along(nm)) if (j > i && sig[i, j]) {
    nuevas <- list()
    for (cc in cols) nuevas <- if (nm[i] %in% cc && nm[j] %in% cc) c(nuevas, list(setdiff(cc, nm[i]), setdiff(cc, nm[j]))) else c(nuevas, list(cc))
    nuevas <- unique(lapply(nuevas, sort))
    quedan <- vapply(seq_along(nuevas), function(k) !any(vapply(seq_along(nuevas), function(l) l != k && all(nuevas[[k]] %in% nuevas[[l]]) && length(nuevas[[l]]) > length(nuevas[[k]]), TRUE)), TRUE)
    cols <- nuevas[quedan]
  }
  cols <- cols[order(-vapply(cols, function(cc) max(medias[cc]), 0))]
  vapply(nm, function(v) paste(letters[which(vapply(cols, function(cc) v %in% cc, TRUE))], collapse = ""), "")
}

# ---------------------------------------------------------------- ANOVA por descriptor
# Con repeticiones: modelo mixto clásico del análisis descriptivo (panelista aleatorio):
#   y ~ Panelista + Producto + Producto:Panelista, con F(Producto) = CM(Producto) / CM(Producto:Panelista).
# Sin repeticiones: y ~ Panelista + Producto, F contra el residuo.
anova_atributo <- function(A, a) {
  d <- A$d; y <- d[[a]]
  if (stats::var(y) == 0) return(list(F = NA, p = NA, p_pan = NA, p_int = NA, cme = NA, gl = NA))
  if (A$conrep) {
    t <- summary(stats::aov(y ~ Panelista + Producto + Producto:Panelista, data = d))[[1]]
    rn <- trimws(rownames(t)); f <- function(x) which(rn == x)
    cm_p <- t[f("Producto"), "Mean Sq"]; cm_i <- t[f("Panelista:Producto"), "Mean Sq"]; gl_i <- t[f("Panelista:Producto"), "Df"]
    cm_j <- t[f("Panelista"), "Mean Sq"]; cm_r <- t[f("Residuals"), "Mean Sq"]; gl_r <- t[f("Residuals"), "Df"]
    if (!length(cm_i) || gl_i < 1 || cm_i <= 0) { cm_i <- cm_r; gl_i <- gl_r }
    Fp <- cm_p / cm_i
    list(F = Fp, p = stats::pf(Fp, t[f("Producto"), "Df"], gl_i, lower.tail = FALSE),
         p_pan = stats::pf(cm_j / cm_i, t[f("Panelista"), "Df"], gl_i, lower.tail = FALSE),
         p_int = if (length(cm_r) && cm_r > 0) stats::pf(cm_i / cm_r, gl_i, gl_r, lower.tail = FALSE) else NA,
         cme = cm_i, gl = gl_i)
  } else {
    t <- summary(stats::aov(y ~ Panelista + Producto, data = d))[[1]]
    rn <- trimws(rownames(t)); f <- function(x) which(rn == x)
    list(F = t[f("Producto"), "F value"], p = t[f("Producto"), "Pr(>F)"], p_pan = t[f("Panelista"), "Pr(>F)"], p_int = NA,
         cme = t[f("Residuals"), "Mean Sq"], gl = t[f("Residuals"), "Df"])
  }
}

# Tukey (HSD) con el término de error del ANOVA
tukey_letras <- function(medias, n, cme, gl) {
  P <- names(medias); k <- length(P)
  sig <- matrix(FALSE, k, k, dimnames = list(P, P))
  if (is.na(cme) || cme <= 0 || k < 2) return(stats::setNames(rep("", k), P))
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    q <- abs(medias[i] - medias[j]) / sqrt(cme / 2 * (1 / n[i] + 1 / n[j]))
    sig[i, j] <- sig[j, i] <- stats::ptukey(q, k, gl, lower.tail = FALSE) < 0.05
  }
  letras(sig, medias)
}

analisis_medias <- function(A) {
  d <- A$d; P <- A$productos
  n <- as.numeric(table(d$Producto)[P])
  res <- lapply(A$atributos, function(a) {
    m <- tapply(d[[a]], d$Producto, mean)[P]
    av <- anova_atributo(A, a)
    list(medias = m, an = av, letras = if (!is.na(av$p) && av$p < 0.05) tukey_letras(m, n, av$cme, av$gl) else stats::setNames(rep("", length(P)), P),
         cita = tapply(d[[a]] > 0, d$Producto, mean)[P],
         cond = tapply(d[[a]], d$Producto, function(x) if (any(x > 0)) mean(x[x > 0]) else NA)[P])
  })
  names(res) <- A$atributos
  res
}

tabla_medias <- function(A, R, que = "medias") {
  out <- data.frame(Descriptor = A$atributos, check.names = FALSE)
  if (any(nzchar(A$grupos))) out$Modalidad <- unname(A$grupos)
  for (p in A$productos) out[[p]] <- vapply(A$atributos, function(a) {
    r <- R[[a]]
    if (que == "cita") sprintf("%.0f%%", 100 * r$cita[[p]])
    else if (que == "cond") ifelse(is.na(r$cond[[p]]), "—", sprintf("%.2f", r$cond[[p]]))
    else paste0(sprintf("%.2f", r$medias[[p]]), if (nzchar(r$letras[[p]])) paste0(" ", r$letras[[p]]) else "")
  }, "")
  out$F <- round(vapply(A$atributos, function(a) R[[a]]$an$F, 0), 2)
  out$p <- signif(vapply(A$atributos, function(a) R[[a]]$an$p, 0), 3)
  if (A$conrep) out$`p Prod×Panel` <- signif(vapply(A$atributos, function(a) R[[a]]$an$p_int, 0), 3)
  out[order(out$p, na.last = TRUE), ]
}

matriz_medias <- function(A, R, atrs = A$atributos) {
  M <- sapply(atrs, function(a) R[[a]]$medias); if (is.null(dim(M))) M <- matrix(M, nrow = length(A$productos))
  dimnames(M) <- list(A$productos, atrs); M
}

# ---------------------------------------------------------------- citación (presencia/ausencia)
cochran <- function(A, ajuste = "none") {
  d <- A$d; P <- A$productos; k <- length(P)
  bl <- split(d, d$Bloque)
  completos <- bl[vapply(bl, function(b) all(P %in% b$NombreProducto), TRUE)]
  res <- lapply(A$atributos, function(a) {
    if (k < 2 || length(completos) < 2) return(list(Q = NA, p = NA, letras = stats::setNames(rep("", k), P)))
    M <- t(vapply(completos, function(b) vapply(P, function(p) as.numeric(b[[a]][match(p, b$NombreProducto)] > 0), 0), numeric(k)))
    C <- colSums(M); R <- rowSums(M); N <- sum(M); den <- k * N - sum(R^2)
    Q <- if (den > 0) (k - 1) * (k * sum(C^2) - N^2) / den else 0
    p <- if (den > 0) stats::pchisq(Q, k - 1, lower.tail = FALSE) else 1
    pv <- matrix(1, k, k, dimnames = list(P, P))
    for (i in 1:(k - 1)) for (j in (i + 1):k) {
      b <- sum(M[, i] == 1 & M[, j] == 0); c <- sum(M[, i] == 0 & M[, j] == 1)
      pv[i, j] <- pv[j, i] <- if (b + c > 0) stats::binom.test(b, b + c)$p.value else 1
    }
    sup <- upper.tri(pv); pv[sup] <- stats::p.adjust(pv[sup], ajuste); pv[lower.tri(pv)] <- t(pv)[lower.tri(pv)]
    list(Q = Q, p = p, letras = letras(pv < 0.05, stats::setNames(C, P)))
  })
  names(res) <- A$atributos
  list(res = res, bloques = length(completos))
}

# ---------------------------------------------------------------- mapas
correr_pca <- function(A, R, solo_sig = TRUE, umbral = 0.05) {
  atrs <- A$usados; todos <- FALSE
  if (solo_sig) {
    sig <- atrs[vapply(atrs, function(a) isTRUE(R[[a]]$an$p < umbral), TRUE)]
    # con pocos panelistas puede que ninguno discrimine todavía: se usan todos y se avisa
    if (length(sig) >= 2) atrs <- sig else todos <- TRUE
  }
  if (length(A$productos) < 3) stop("El mapa necesita al menos 3 productos con respuestas.")
  if (length(atrs) < 2) stop("El mapa necesita al menos 2 descriptores marcados en algún producto.")
  r <- FactoMineR::PCA(as.data.frame(matriz_medias(A, R, atrs), check.names = FALSE), scale.unit = TRUE, graph = FALSE, ncp = 5)
  attr(r, "todos") <- todos
  r
}

# elipses de confianza por remuestreo de panelistas (como SensoMineR::panellipse): se sortean panelistas con
# reposición, se recalculan las medias y se proyectan sobre el PCA original como individuos suplementarios.
elipses <- function(A, pca, nsim = 300, dims = 1:2) {
  set.seed(123)
  atrs <- rownames(pca$var$coord); pans <- A$panelistas; d <- A$d
  out <- lapply(seq_len(nsim), function(b) {
    js <- sample(pans, replace = TRUE)
    db <- do.call(rbind, lapply(seq_along(js), function(i) d[d$Panelista == js[i], c("Producto", atrs)]))
    M <- sapply(atrs, function(a) tapply(db[[a]], db$Producto, mean)[A$productos])
    if (is.null(dim(M)) || anyNA(M)) return(NULL)
    rownames(M) <- A$productos
    X <- stats::predict(pca, newdata = as.data.frame(M, check.names = FALSE))$coord
    data.frame(Producto = A$productos, x = X[, dims[1]], y = X[, dims[2]])
  })
  do.call(rbind, out)
}

clasificar <- function(pca, k) {
  X <- pca$ind$coord[, seq_len(min(3, ncol(pca$ind$coord))), drop = FALSE]
  hc <- stats::hclust(stats::dist(X), method = "ward.D2")
  list(hc = hc, cl = stats::cutree(hc, min(k, nrow(X) - 1)))
}

# ---------------------------------------------------------------- desempeño del panel
# Por panelista y descriptor: discriminación (p de su ANOVA de un factor, si hay repeticiones),
# acuerdo con el panel (correlación de sus medias con las del panel) y repetibilidad (desvío residual).
desempeno <- function(A, R) {
  d <- A$d; P <- A$productos
  pans <- A$panelistas; atrs <- A$usados
  mk <- function() matrix(NA_real_, length(pans), length(atrs), dimnames = list(pans, atrs))
  disc <- mk(); acu <- mk(); rep_ <- mk()
  for (j in pans) for (a in atrs) {
    x <- d[d$Panelista == j, ]
    mj <- tapply(x[[a]], x$Producto, mean)[P]
    mp <- R[[a]]$medias
    ok <- !is.na(mj)
    if (sum(ok) >= 3 && stats::sd(mj[ok]) > 0 && stats::sd(mp[ok]) > 0) acu[j, a] <- stats::cor(mj[ok], mp[ok])
    if (A$conrep && length(unique(x$Producto)) > 1 && any(table(x$Producto) > 1)) {
      if (stats::var(x[[a]]) == 0) { disc[j, a] <- 1; rep_[j, a] <- 0 }
      else { t <- summary(stats::aov(x[[a]] ~ x$Producto))[[1]]; disc[j, a] <- t[1, "Pr(>F)"]; rep_[j, a] <- sqrt(t[2, "Mean Sq"]) }
    }
  }
  resumen <- data.frame(Panelista = pans,
    `Acuerdo medio (r)` = round(rowMeans(acu, na.rm = TRUE), 2),
    `Descriptores con acuerdo < 0` = rowSums(acu < 0, na.rm = TRUE),
    check.names = FALSE)
  if (A$conrep) {
    resumen$`Descriptores que discrimina (p < 0,05)` <- rowSums(disc < 0.05, na.rm = TRUE)
    resumen$`Repetibilidad (desvío medio)` <- round(rowMeans(rep_, na.rm = TRUE), 2)
  }
  resumen$`Citas por muestra` <- round(vapply(pans, function(j) mean(rowSums(d[d$Panelista == j, atrs, drop = FALSE] > 0)), 0), 1)
  list(disc = disc, acu = acu, rep = rep_, resumen = resumen)
}
