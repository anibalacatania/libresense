# Gráficos (ggplot2) del Sorting
library(ggplot2)

PALETA <- c("#7c3aed", "#db2777", "#0891b2", "#d97706", "#059669", "#4f46e5", "#dc2626", "#65a30d",
            "#0d9488", "#c026d3", "#ea580c", "#2563eb", "#9333ea", "#16a34a", "#e11d48", "#0284c7")
col_n <- function(n) rep(PALETA, length.out = n)
tema <- function() theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom", plot.title.position = "plot")
ejes <- function() list(geom_hline(yintercept = 0, linetype = 2, colour = "grey70"), geom_vline(xintercept = 0, linetype = 2, colour = "grey70"))
etq <- function(dim, pct) sprintf("Dim %d (%.1f%%)", dim, pct)
NOMBRES <- c(mca = "Análisis de correspondencias múltiples", distatis = "DISTATIS", mds = "MDS de co-ocurrencias")

# mapa de productos, coloreados por grupo (clasificación), con palabras y elipses opcionales
g_mapa <- function(F, pct, dims = 1:2, metodo = "mca", cl = NULL, palabras = NULL, boot = NULL) {
  df <- data.frame(Producto = rownames(F), x = F[, dims[1]], y = F[, dims[2]])
  df$Grupo <- if (!is.null(cl)) factor(paste("Grupo", cl[df$Producto])) else factor("Producto")
  p <- ggplot(df, aes(x, y)) + ejes()
  if (!is.null(boot)) {
    boot$Grupo <- df$Grupo[match(boot$Producto, df$Producto)]
    p <- p + stat_ellipse(data = boot, aes(group = Producto, colour = Grupo), level = 0.95, linewidth = 0.7, alpha = 0.8)
  }
  if (!is.null(palabras) && nrow(palabras)) p <- p +
    ggrepel::geom_text_repel(data = palabras, aes(x, y, label = Palabra, size = n), colour = "#be185d", alpha = 0.85, max.overlaps = 80, show.legend = FALSE) +
    scale_size_continuous(range = c(3, 6), guide = "none")
  p + geom_point(aes(colour = Grupo), size = 4) +
    ggrepel::geom_label_repel(aes(label = Producto, colour = Grupo), fontface = "bold", size = 4.3, label.size = 0, fill = alpha("white", 0.85), max.overlaps = 80, show.legend = FALSE) +
    scale_colour_manual(values = col_n(nlevels(df$Grupo)), name = NULL) + coord_equal() +
    labs(x = etq(dims[1], pct[dims[1]]), y = etq(dims[2], pct[dims[2]]), title = NOMBRES[[metodo]],
         subtitle = paste0("Muestras cerca = muchos panelistas las pusieron en el mismo grupo",
                           if (!is.null(boot)) "\nElipses de confianza al 95% (bootstrap de panelistas)" else "")) + tema()
}

# co-ocurrencias (% de panelistas), ordenadas según el dendrograma
g_cooc <- function(C, nh, orden) {
  P <- C / nh * 100; P <- P[orden, orden]
  df <- as.data.frame(as.table(P)); names(df) <- c("a", "b", "v")
  df$v[df$a == df$b] <- NA
  ggplot(df, aes(a, b, fill = v)) + geom_tile(colour = "white") + geom_text(aes(label = ifelse(is.na(v), "", round(v))), size = 3.3) +
    scale_fill_gradient(low = "#f5f3ff", high = "#6d28d9", na.value = "grey95", limits = c(0, 100), name = "% juntos") +
    scale_y_discrete(limits = rev) + labs(x = NULL, y = NULL, title = "Co-ocurrencias", subtitle = "% de panelistas que pusieron cada par en el mismo grupo") +
    tema() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right", panel.grid = element_blank())
}

g_barras <- function(valores, titulo, xlab, umbral = NULL, fmt = "%.2f", color = "#7c3aed") {
  df <- data.frame(p = names(valores), v = as.numeric(valores))
  df$p <- factor(df$p, levels = df$p[order(df$v)])
  g <- ggplot(df, aes(v, p)) + geom_col(fill = color) + geom_text(aes(label = sprintf(fmt, v)), hjust = -0.15, size = 3.6) +
    labs(x = xlab, y = NULL, title = titulo) + tema() + scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  if (!is.null(umbral)) g <- g + geom_vline(xintercept = umbral, linetype = 2, colour = "#dc2626")
  g
}

g_ngrupos <- function(G) {
  df <- data.frame(n = apply(G, 2, function(x) length(unique(x))))
  ggplot(df, aes(factor(n, levels = 2:max(nrow(G) - 1, df$n)))) + geom_bar(fill = "#7c3aed") + scale_x_discrete(drop = FALSE) +
    labs(x = "Cantidad de grupos", y = "Panelistas", title = "¿Cuántos grupos hizo cada panelista?") + tema()
}

g_rv <- function(R) {
  df <- as.data.frame(as.table(R)); names(df) <- c("a", "b", "rv"); df$rv <- pmin(1, pmax(0, df$rv))
  ggplot(df, aes(a, b, fill = rv)) + geom_tile(colour = "white") + geom_text(aes(label = sprintf("%.2f", rv)), size = 2.8) +
    scale_fill_gradient(low = "#fef3c7", high = "#7c3aed", limits = c(0, 1), name = "RV") +
    labs(x = NULL, y = NULL, title = "Parecido entre panelistas (RV, DISTATIS)") + tema() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")
}

# panelistas: DISTATIS (matriz RV) o MCA (razones de correlación)
g_panelistas <- function(X, pct, titulo, sub) {
  df <- data.frame(p = rownames(X), x = X[, 1], y = X[, 2])
  ggplot(df, aes(x, y)) + geom_point(size = 3, colour = "#db2777") + ggrepel::geom_text_repel(aes(label = p), size = 3.6, max.overlaps = 80) +
    labs(x = etq(1, pct[1]), y = etq(2, pct[2]), title = titulo, subtitle = sub) + tema()
}

g_terminos <- function(fr, n = 25) {
  fr <- utils::head(fr, n); fr$Termino <- factor(fr$Termino, levels = rev(fr$Termino))
  ggplot(fr, aes(Veces, Termino)) + geom_col(fill = "#db2777") + labs(x = "Veces que se usó", y = NULL, title = "Palabras más usadas") + tema()
}
