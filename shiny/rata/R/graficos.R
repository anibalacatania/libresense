# Gráficos (ggplot2) de RATA
library(ggplot2)

PALETA <- c("#4f46e5", "#db2777", "#0891b2", "#d97706", "#059669", "#7c3aed", "#dc2626", "#65a30d",
            "#0d9488", "#c026d3", "#ea580c", "#2563eb", "#9333ea", "#16a34a", "#e11d48", "#0284c7")
col_n <- function(n) rep(PALETA, length.out = n)
tema <- function() theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom", plot.title.position = "plot")
ejes <- function() list(geom_hline(yintercept = 0, linetype = 2, colour = "grey70"), geom_vline(xintercept = 0, linetype = 2, colour = "grey70"))
etq <- function(dim, pct) sprintf("Dim %d (%.1f%%)", dim, pct)
estrellas <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, " ***", ifelse(p < 0.01, " **", ifelse(p < 0.05, " *", ""))))

# mapa de calor de medias, con letras de Tukey en los descriptores que discriminan
g_medias <- function(A, R, escala, solo_sig = FALSE) {
  atrs <- A$atributos; if (solo_sig) atrs <- atrs[vapply(atrs, function(a) isTRUE(R[[a]]$an$p < 0.05), TRUE)]
  if (!length(atrs)) stop("Ningún descriptor discrimina entre productos (p < 0,05).")
  atrs <- atrs[order(vapply(atrs, function(a) max(R[[a]]$medias), 0))]
  df <- expand.grid(Producto = A$productos, Descriptor = atrs, stringsAsFactors = FALSE)
  df$m <- mapply(function(p, a) R[[a]]$medias[[p]], df$Producto, df$Descriptor)
  df$l <- mapply(function(p, a) R[[a]]$letras[[p]], df$Producto, df$Descriptor)
  lab <- vapply(atrs, function(a) paste0(a, estrellas(R[[a]]$an$p)), "")
  df$Descriptor <- factor(df$Descriptor, levels = atrs, labels = lab)
  df$Producto <- factor(df$Producto, levels = A$productos)
  ggplot(df, aes(Producto, Descriptor, fill = m)) + geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f%s", m, ifelse(nzchar(l), paste0(" ", l), "")), colour = m > escala * 0.55), size = 3.5) +
    scale_colour_manual(values = c(`FALSE` = "#0f172a", `TRUE` = "white"), guide = "none") +
    scale_fill_gradient(low = "#fdf2f8", high = "#9d174d", limits = c(0, escala), name = "Media") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Intensidad media por producto",
         subtitle = sprintf("0 = no marcado; 1–%d = intensidad\nANOVA: *** p<0,001 ** p<0,01 * p<0,05 · letras distintas = Tukey (p<0,05)", escala)) +
    tema() + theme(legend.position = "right", panel.grid = element_blank())
}

# perfiles (radar) de los productos elegidos
g_radar <- function(A, R, productos, atrs, escala) {
  if (length(atrs) < 3) stop("Elegí al menos 3 descriptores para el gráfico de perfiles.")
  df <- do.call(rbind, lapply(productos, function(p) data.frame(Producto = p, Descriptor = atrs, m = vapply(atrs, function(a) R[[a]]$medias[[p]], 0))))
  df$Descriptor <- factor(df$Descriptor, levels = atrs)
  cierre <- do.call(rbind, lapply(split(df, df$Producto), function(x) rbind(x, x[1, ])))
  ggplot(cierre, aes(Descriptor, m, group = Producto, colour = Producto)) +
    geom_polygon(aes(fill = Producto), alpha = 0.06, linewidth = 0.9) + geom_point(size = 2) +
    coord_polar() + scale_y_continuous(limits = c(0, escala)) +
    scale_colour_manual(values = col_n(length(productos))) + scale_fill_manual(values = col_n(length(productos))) +
    labs(x = NULL, y = NULL, title = "Perfil sensorial", colour = NULL, fill = NULL) + tema() +
    theme(axis.text.x = element_text(size = 10, face = "bold"), panel.grid.major = element_line(colour = "grey85"))
}

# barras con letras para un descriptor
g_descriptor <- function(A, R, a, escala) {
  r <- R[[a]]
  se <- tapply(A$d[[a]], A$d$Producto, function(x) stats::sd(x) / sqrt(length(x)))[A$productos]
  df <- data.frame(Producto = factor(A$productos, levels = A$productos[order(r$medias)]), m = r$medias, se = se, l = r$letras, cita = r$cita)
  ggplot(df, aes(m, Producto)) + geom_col(fill = "#f472b6", width = 0.65) +
    geom_errorbarh(aes(xmin = pmax(0, m - se), xmax = m + se), height = 0.2, colour = "#831843") +
    geom_text(aes(x = m + se + escala * 0.02, label = sprintf("%.2f %s  (%.0f%% lo marcó)", m, l, 100 * cita)), hjust = 0, size = 3.6) +
    scale_x_continuous(limits = c(0, escala * 1.35), breaks = 0:escala) +
    labs(x = "Media ± error estándar", y = NULL, title = a,
         subtitle = sprintf("F = %.2f, p = %s", r$an$F, format.pval(r$an$p, digits = 3))) + tema()
}

# interacción producto × panelista para un descriptor
g_interaccion <- function(A, a) {
  m <- stats::aggregate(A$d[[a]], list(Producto = A$d$Producto, Panelista = A$d$Panelista), mean)
  panel <- stats::aggregate(A$d[[a]], list(Producto = A$d$Producto), mean)
  ggplot(m, aes(Producto, x, group = Panelista, colour = Panelista)) + geom_line(alpha = 0.6) + geom_point(size = 1.5, alpha = 0.7) +
    geom_line(data = panel, aes(Producto, x, group = 1), inherit.aes = FALSE, colour = "black", linewidth = 1.4) +
    scale_colour_manual(values = col_n(length(unique(m$Panelista)))) +
    labs(x = NULL, y = "Media del panelista", title = paste("Producto × panelista:", a),
         subtitle = "Línea negra = media del panel. Líneas que se cruzan = panelistas que ordenan distinto los productos") +
    tema() + theme(legend.position = "right", axis.text.x = element_text(angle = 30, hjust = 1))
}

# biplot del PCA
g_pca <- function(pca, dims = 1:2, cl = NULL, boot = NULL) {
  pct <- pca$eig[, 2]
  ind <- data.frame(et = rownames(pca$ind$coord), x = pca$ind$coord[, dims[1]], y = pca$ind$coord[, dims[2]])
  ind$Grupo <- if (!is.null(cl)) factor(paste("Grupo", cl[ind$et])) else factor("Producto")
  v <- data.frame(et = rownames(pca$var$coord), x = pca$var$coord[, dims[1]], y = pca$var$coord[, dims[2]])
  k <- max(abs(c(ind$x, ind$y))) * 0.9
  p <- ggplot() + ejes()
  if (!is.null(boot)) { boot$Grupo <- ind$Grupo[match(boot$Producto, ind$et)]
    p <- p + stat_ellipse(data = boot, aes(x, y, group = Producto, colour = Grupo), level = 0.95, linewidth = 0.7) }
  p +
    geom_segment(data = v, aes(0, 0, xend = x * k, yend = y * k), colour = "#db2777", alpha = 0.7, arrow = arrow(length = unit(0.18, "cm"))) +
    ggrepel::geom_text_repel(data = v, aes(x * k, y * k, label = et), colour = "#be185d", size = 3.7, max.overlaps = 80) +
    geom_point(data = ind, aes(x, y, colour = Grupo), size = 4) +
    ggrepel::geom_label_repel(data = ind, aes(x, y, label = et, colour = Grupo), fontface = "bold", label.size = 0, fill = alpha("white", 0.85), show.legend = FALSE, max.overlaps = 80) +
    scale_colour_manual(values = col_n(nlevels(ind$Grupo)), name = NULL) + coord_equal() +
    labs(x = etq(dims[1], pct[dims[1]]), y = etq(dims[2], pct[dims[2]]), title = "PCA de las medias (estandarizado)",
         subtitle = paste0("Un producto está del lado hacia donde apuntan sus descriptores más intensos",
                           if (!is.null(boot)) "\nElipses de confianza al 95% (remuestreo de panelistas)" else "")) + tema()
}

# mapas de calor del desempeño (panelistas × descriptores)
g_calor <- function(M, titulo, sub, tipo = c("p", "r", "sd")) {
  tipo <- match.arg(tipo)
  df <- as.data.frame(as.table(M)); names(df) <- c("Panelista", "Descriptor", "v")
  df$Panelista <- factor(df$Panelista, levels = rev(rownames(M)))
  esc <- switch(tipo,
    p = scale_fill_gradientn(colours = c("#16a34a", "#bbf7d0", "#fef9c3", "#fecaca", "#dc2626"), values = c(0, 0.05, 0.2, 0.5, 1), limits = c(0, 1), name = "p", na.value = "grey95"),
    r = scale_fill_gradient2(low = "#dc2626", mid = "#fef9c3", high = "#16a34a", midpoint = 0.5, limits = c(-1, 1), name = "r", na.value = "grey95"),
    sd = scale_fill_gradient(low = "#ecfdf5", high = "#b91c1c", name = "desvío", na.value = "grey95"))
  ggplot(df, aes(Descriptor, Panelista, fill = v)) + geom_tile(colour = "white") +
    geom_text(aes(label = ifelse(is.na(v), "", sprintf(if (tipo == "p") "%.2f" else "%.2f", v))), size = 2.8) + esc +
    labs(x = NULL, y = NULL, title = titulo, subtitle = sub) + tema() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right", panel.grid = element_blank())
}

g_citas <- function(A, cochr, solo_sig = FALSE) {
  atrs <- A$atributos; if (solo_sig) atrs <- atrs[vapply(atrs, function(a) isTRUE(cochr$res[[a]]$p < 0.05), TRUE)]
  if (!length(atrs)) stop("Ningún descriptor se marca distinto entre productos (p < 0,05).")
  df <- expand.grid(Producto = A$productos, Descriptor = atrs, stringsAsFactors = FALSE)
  df$pct <- mapply(function(p, a) 100 * mean(A$d[[a]][A$d$NombreProducto == p] > 0), df$Producto, df$Descriptor)
  df$l <- mapply(function(p, a) if (isTRUE(cochr$res[[a]]$p < 0.05)) cochr$res[[a]]$letras[[p]] else "", df$Producto, df$Descriptor)
  ord <- atrs[order(vapply(atrs, function(a) mean(A$d[[a]] > 0), 0))]
  df$Descriptor <- factor(df$Descriptor, levels = ord, labels = vapply(ord, function(a) paste0(a, estrellas(cochr$res[[a]]$p)), ""))
  df$Producto <- factor(df$Producto, levels = A$productos)
  ggplot(df, aes(Producto, Descriptor, fill = pct)) + geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.0f%s", pct, ifelse(nzchar(l), paste0(" ", l), "")), colour = pct > 55), size = 3.5) +
    scale_colour_manual(values = c(`FALSE` = "#0f172a", `TRUE` = "white"), guide = "none") +
    scale_fill_gradient(low = "#f0f9ff", high = "#0369a1", limits = c(0, 100), name = "% marcado") + scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Frecuencia de citación (%)", subtitle = "Q de Cochran: *** p<0,001 ** p<0,01 * p<0,05 · letras distintas = McNemar de a pares") +
    tema() + theme(legend.position = "right", panel.grid = element_blank())
}
