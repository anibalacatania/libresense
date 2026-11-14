# Gráficos (ggplot2) del Perfil Flash
library(ggplot2)

PALETA <- c("#4f46e5", "#db2777", "#059669", "#d97706", "#0891b2", "#7c3aed", "#dc2626", "#65a30d",
            "#0d9488", "#c026d3", "#ea580c", "#2563eb", "#9333ea", "#16a34a", "#e11d48", "#0284c7")
col_pan <- function(n) rep(PALETA, length.out = n)

tema <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
          legend.position = "bottom", plot.title.position = "plot")
}
ejes <- function() list(geom_hline(yintercept = 0, linetype = 2, colour = "grey70"),
                        geom_vline(xintercept = 0, linetype = 2, colour = "grey70"))
etq <- function(dim, pct) sprintf("Dim %d (%.1f%%)", dim, pct)

# mapa de vinos (consenso), con puntos parciales de cada panelista opcionales
g_consenso <- function(coord, pct, dims = 1:2, parciales = NULL, titulo = "") {
  df <- data.frame(vino = rownames(coord), x = coord[, dims[1]], y = coord[, dims[2]])
  df$tipo <- ifelse(es_dup(df$vino), "Duplicado", "Vino")
  p <- ggplot(df, aes(x, y)) + ejes()
  if (!is.null(parciales)) {
    pa <- merge(parciales, df[, c("vino", "x", "y")], by = "vino", suffixes = c("", ".c"))
    p <- p + geom_segment(data = pa, aes(x = x.c, y = y.c, xend = x, yend = y, colour = panelista), alpha = 0.5) +
      geom_point(data = pa, aes(colour = panelista), size = 1.6, alpha = 0.8) +
      scale_colour_manual(values = col_pan(length(unique(pa$panelista))), name = "Panelista")
  }
  p + geom_point(aes(shape = tipo), size = 3.8, colour = "#1e1b4b") +
    ggrepel::geom_text_repel(aes(label = vino), fontface = "bold", size = 4.4, colour = "#1e1b4b", max.overlaps = 50) +
    scale_shape_manual(values = c(Vino = 16, Duplicado = 1), name = NULL) +
    coord_equal() + labs(x = etq(dims[1], pct[dims[1]]), y = etq(dims[2], pct[dims[2]]), title = titulo) + tema()
}

# representación de los panelistas (grupos del MFA)
g_grupos <- function(mfa, dims = 1:2) {
  gc <- mfa$group$coord
  df <- data.frame(p = rownames(gc), x = gc[, dims[1]], y = gc[, dims[2]])
  pct <- mfa$eig[, 2]
  ggplot(df, aes(x, y)) + geom_point(size = 3, colour = "#db2777") +
    ggrepel::geom_text_repel(aes(label = p), size = 4) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = etq(dims[1], pct[dims[1]]), y = etq(dims[2], pct[dims[2]]), title = "Panelistas (representación de los grupos)",
         subtitle = "Cerca del 1 en una dimensión = ese panelista la percibió con fuerza") + tema()
}

g_barras <- function(valores, titulo, xlab, umbral = NULL, invertir = FALSE) {
  df <- data.frame(p = names(valores), v = as.numeric(valores))
  df$p <- factor(df$p, levels = df$p[order(df$v, decreasing = invertir)])
  g <- ggplot(df, aes(v, p)) + geom_col(fill = "#7c3aed") + geom_text(aes(label = sprintf("%.2f", v)), hjust = -0.15, size = 3.6) +
    labs(x = xlab, y = NULL, title = titulo) + tema() + scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  if (!is.null(umbral)) g <- g + geom_vline(xintercept = umbral, linetype = 2, colour = "#dc2626")
  g
}

g_rv <- function(R) {
  df <- as.data.frame(as.table(R)); names(df) <- c("a", "b", "rv"); df$rv <- pmin(1, pmax(0, df$rv))
  ggplot(df, aes(a, b, fill = rv)) + geom_tile(colour = "white") + geom_text(aes(label = sprintf("%.2f", rv)), size = 3) +
    scale_fill_gradient(low = "#fef3c7", high = "#7c3aed", limits = c(0, 1), name = "RV") +
    labs(x = NULL, y = NULL, title = "Parecido entre panelistas (coeficiente RV)") + tema() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

g_terminos <- function(fr, n = 25, xlab = "Veces que se usó") {
  fr <- utils::head(fr, n); fr$Termino <- factor(fr$Termino, levels = rev(fr$Termino))
  ggplot(fr, aes(Panelistas, Termino)) + geom_col(fill = "#d97706") +
    labs(x = xlab, y = NULL, title = "Palabras más usadas") + tema()
}


# elipses de confianza (Dehlholm) sobre el consenso
g_elipses <- function(coord, pct, E, dims = 1:2) {
  df <- data.frame(vino = rownames(coord), x = coord[, dims[1]], y = coord[, dims[2]])
  cols <- col_pan(nrow(df)); names(cols) <- df$vino
  ggplot(df, aes(x, y, colour = vino)) + ejes() +
    geom_path(data = E, aes(group = vino), linewidth = 0.8) + geom_point(size = 3.5) +
    ggrepel::geom_text_repel(aes(label = vino), fontface = "bold", size = 4.4, show.legend = FALSE) +
    scale_colour_manual(values = cols, guide = "none") + coord_equal() +
    labs(x = etq(dims[1], pct[dims[1]]), y = etq(dims[2], pct[dims[2]]), title = "Elipses de confianza (95%)",
         subtitle = "Bootstrap de los puntos parciales del MFA (Dehlholm et al., 2012)") + tema()
}

# muestras + palabras (grupo suplementario de frecuencias del MFA)
g_palabras_mapa <- function(mfa, dims = 1:2, minimo = 2, pal = NULL) {
  ind <- data.frame(vino = rownames(mfa$ind$coord), x = mfa$ind$coord[, dims[1]], y = mfa$ind$coord[, dims[2]])
  # cada palabra en el promedio (ponderado por frecuencia) de las muestras en las que se usó
  pal <- pal[rownames(mfa$ind$coord), , drop = FALSE]
  tot <- colSums(pal)
  wd <- data.frame(p = colnames(pal), x = as.vector(t(pal) %*% ind$x) / tot, y = as.vector(t(pal) %*% ind$y) / tot, n = tot)
  wd <- wd[wd$n >= minimo, ]
  pct <- mfa$eig[, 2]
  ggplot(ind, aes(x, y)) + ejes() +
    ggrepel::geom_text_repel(data = wd, aes(label = p, size = n), colour = "#be185d", max.overlaps = 40, show.legend = FALSE) +
    scale_size_continuous(range = c(3, 5.5)) +
    geom_point(size = 3.6, colour = "#1e1b4b") + ggrepel::geom_text_repel(aes(label = vino), fontface = "bold", size = 4.6, colour = "#1e1b4b") +
    labs(x = etq(dims[1], pct[dims[1]]), y = etq(dims[2], pct[dims[2]]), title = "Muestras y palabras",
         subtitle = sprintf("Palabras usadas al menos %d veces, ubicadas en el promedio de las muestras donde se usaron (no influyen en el mapa)", minimo)) + tema()
}

# hojas originales de cada panelista
g_hojas <- function(A, hoja) {
  H <- HOJAS[[hoja]] %||% HOJAS$rect
  df <- do.call(rbind, lapply(A$grupos, function(g) data.frame(grupo = g, vino = rownames(A$d), x = A$d[[paste0(g, "_X")]], y = A$d[[paste0(g, "_Y")]])))
  df$grupo <- factor(df$grupo, levels = A$grupos)
  marco <- if (H$redonda) { t <- seq(0, 2 * pi, length.out = 120); geom_path(data = data.frame(x = H$ancho / 2 * (1 + cos(t)), y = H$alto / 2 * (1 + sin(t))), aes(x, y), inherit.aes = FALSE, colour = "grey60") }
           else geom_rect(aes(xmin = 0, xmax = H$ancho, ymin = 0, ymax = H$alto), fill = NA, colour = "grey60", inherit.aes = FALSE)
  ggplot(df, aes(x, y)) + marco + geom_point(aes(shape = es_dup(vino)), colour = "#4f46e5", size = 2) +
    geom_text(aes(label = vino), size = 2.6, vjust = -0.9, colour = "#1e1b4b") +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
    facet_wrap(~grupo) + coord_equal(xlim = c(0, H$ancho), ylim = c(0, H$alto)) +
    labs(x = "X (cm)", y = "Y (cm)", title = "Hoja de cada panelista") + theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.text = element_text(size = 7), strip.text = element_text(face = "bold"))
}

# cada panelista superpuesto al consenso (rotado y escalado, como pmfa de SensoMineR)
g_superpuesto <- function(S) {
  S$grupo <- factor(S$grupo, levels = unique(S$grupo))
  ggplot(S) + geom_segment(aes(x = cx, y = cy, xend = x, yend = y), colour = "grey65") +
    geom_point(aes(cx, cy), colour = "#1e1b4b", size = 2) + geom_point(aes(x, y), colour = "#db2777", size = 1.8) +
    geom_text(aes(cx, cy, label = vino), size = 2.5, vjust = -0.9) + facet_wrap(~grupo) + coord_equal() +
    labs(x = "Dim 1", y = "Dim 2", title = "Cada panelista (rosa) superpuesto al consenso (azul)") + theme_minimal(base_size = 11) +
    theme(axis.text = element_text(size = 7), strip.text = element_text(face = "bold"))
}
