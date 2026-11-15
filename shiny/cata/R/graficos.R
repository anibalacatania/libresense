# Gráficos (ggplot2) de CATA con aceptabilidad
library(ggplot2)

PALETA <- c("#4f46e5", "#db2777", "#0891b2", "#d97706", "#059669", "#7c3aed", "#dc2626", "#65a30d",
            "#0d9488", "#c026d3", "#ea580c", "#2563eb")
tema <- function() theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom", plot.title.position = "plot")
ejes <- function() list(geom_hline(yintercept = 0, linetype = 2, colour = "grey70"), geom_vline(xintercept = 0, linetype = 2, colour = "grey70"))
etq <- function(dim, pct) sprintf("Dim %d (%.1f%%)", dim, pct)

# medias de aceptabilidad con error estándar y letras de Tukey
g_agrado <- function(ag) {
  t <- ag$tabla; t$Producto <- factor(t$Producto, levels = t$Producto[order(t$Media)])
  ggplot(t, aes(Media, Producto)) +
    geom_col(fill = "#a78bfa", width = 0.65) +
    geom_errorbarh(aes(xmin = Media - EE, xmax = Media + EE), height = 0.2, colour = "#4c1d95") +
    geom_text(aes(x = Media + EE + 0.15, label = sprintf("%.2f %s", Media, Grupo)), hjust = 0, fontface = "bold", colour = "#1e1b4b") +
    scale_x_continuous(limits = c(0, 9.8), breaks = 1:9) +
    labs(x = "Aceptabilidad media (1–9) ± error estándar", y = NULL,
         title = "Aceptabilidad", subtitle = "Letras distintas: diferencias significativas (Tukey, p < 0,05)") + tema()
}

# distribución de las respuestas en la escala hedónica
g_distribucion <- function(ag) {
  d <- ag$datos
  tab <- as.data.frame(prop.table(table(d$Producto, factor(d$Aceptabilidad, levels = 1:9)), 1))
  names(tab) <- c("Producto", "Puntaje", "Prop")
  tab$Puntaje <- factor(tab$Puntaje, levels = 9:1, labels = paste(9:1, "·", rev(HEDONICA)))
  ggplot(tab, aes(Prop, Producto, fill = Puntaje)) + geom_col(width = 0.7) +
    scale_fill_manual(values = grDevices::colorRampPalette(c("#15803d", "#86efac", "#f3f4f6", "#fca5a5", "#b91c1c"))(9), name = NULL) +
    scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    labs(x = "% de consumidores", y = NULL, title = "Distribución de las respuestas") + tema() +
    theme(legend.position = "right") + guides(fill = guide_legend(ncol = 1))
}

# mapa de calor de frecuencias de citación (con letras de McNemar en los términos que discriminan)
g_frecuencias <- function(A, coch, solo_sig = FALSE) {
  F <- frecuencias(A); n <- as.numeric(table(A$ev$Producto)[A$productos])
  ts <- A$terminos; if (solo_sig) ts <- ts[vapply(ts, function(t) isTRUE(coch$res[[t]]$p < 0.05), TRUE)]
  if (!length(ts)) stop("Ningún término discrimina entre productos (p < 0,05).")
  ts <- ts[order(colSums(F[, ts, drop = FALSE]))]
  df <- expand.grid(Producto = A$productos, Termino = ts, stringsAsFactors = FALSE)
  df$pct <- 100 * F[cbind(df$Producto, df$Termino)] / n[match(df$Producto, A$productos)]
  df$let <- mapply(function(p, t) if (isTRUE(coch$res[[t]]$p < 0.05)) coch$res[[t]]$letras[[p]] else "", df$Producto, df$Termino)
  ylab <- vapply(ts, function(t) { p <- coch$res[[t]]$p; paste0(t, if (is.na(p)) "" else if (p < 0.001) " ***" else if (p < 0.01) " **" else if (p < 0.05) " *" else "") }, "")
  if (!is.null(A$ideal)) {
    id <- data.frame(Producto = "★ Ideal", Termino = ts, pct = 100 * colMeans(A$ideal[, ts, drop = FALSE] > 0), let = "")
    df <- rbind(df, id)
  }
  df$Producto <- factor(df$Producto, levels = c(A$productos, "★ Ideal"))
  df$Termino <- factor(df$Termino, levels = ts, labels = ylab)
  ggplot(df, aes(Producto, Termino, fill = pct)) + geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.0f%s", pct, ifelse(nzchar(let), paste0(" ", let), "")), colour = pct > 55), size = 3.6) +
    scale_colour_manual(values = c(`FALSE` = "#0f172a", `TRUE` = "white"), guide = "none") +
    scale_fill_gradient(low = "#f0f9ff", high = "#0369a1", name = "% de citación", limits = c(0, 100)) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Frecuencia de citación (%)",
         subtitle = "Q de Cochran: *** p<0,001 ** p<0,01 * p<0,05 · letras distintas = McNemar de a pares (p<0,05)") + tema() +
    theme(legend.position = "right", panel.grid = element_blank())
}

# mapa de correspondencias: productos, términos, ideal suplementario y agrado como variable suplementaria
g_ca <- function(ca, dims = 1:2) {
  pct <- ca$eig[, 2]
  pr <- data.frame(et = rownames(ca$row$coord), x = ca$row$coord[, dims[1]], y = ca$row$coord[, dims[2]], tipo = "Producto")
  te <- data.frame(et = rownames(ca$col$coord), x = ca$col$coord[, dims[1]], y = ca$col$coord[, dims[2]], tipo = "Término")
  id <- if (!is.null(ca$row.sup)) data.frame(et = rownames(ca$row.sup$coord), x = ca$row.sup$coord[, dims[1]], y = ca$row.sup$coord[, dims[2]], tipo = "Ideal") else NULL
  p <- ggplot(mapping = aes(x, y)) + ejes() +
    geom_point(data = te, colour = "#db2777", size = 1.6, shape = 17) +
    ggrepel::geom_text_repel(data = te, aes(label = et), colour = "#be185d", size = 3.7, max.overlaps = 60) +
    geom_point(data = pr, colour = "#1e1b4b", size = 3.8) +
    ggrepel::geom_label_repel(data = pr, aes(label = et), fontface = "bold", size = 4.3, colour = "#1e1b4b", fill = "white", label.size = 0, max.overlaps = 60)
  if (!is.null(id)) p <- p + geom_point(data = id, shape = 8, size = 5, stroke = 1.4, colour = "#d97706") +
    ggrepel::geom_text_repel(data = id, aes(label = "IDEAL"), fontface = "bold", colour = "#b45309", size = 4.5)
  if (!is.null(ca$quanti.sup)) {
    q <- ca$quanti.sup$coord; r <- max(abs(c(pr$x, pr$y, te$x, te$y))) * 0.85
    ag <- data.frame(x = q[1, dims[1]] * r, y = q[1, dims[2]] * r)
    p <- p + geom_segment(data = ag, aes(x = 0, y = 0, xend = x, yend = y), colour = "#059669", linewidth = 1.1, arrow = arrow(length = unit(0.25, "cm"))) +
      geom_text(data = ag, aes(label = "Aceptabilidad"), colour = "#047857", fontface = "bold", vjust = -0.6)
  }
  p + coord_equal() + labs(x = etq(dims[1], pct[dims[1]]), y = etq(dims[2], pct[dims[2]]), title = "Análisis de correspondencias",
                           subtitle = "Productos (azul), términos (rosa), ideal (asterisco naranja) y dirección de la aceptabilidad (flecha verde)") + tema()
}

# impacto en el agrado vs. frecuencia de citación
g_impacto <- function(im) {
  im <- im[!is.na(im$Diferencia), ]
  im$tipo <- ifelse(!is.na(im$p) & im$p < 0.05, ifelse(im$Diferencia > 0, "Sube el agrado", "Baja el agrado"), "Sin efecto claro")
  ggplot(im, aes(`% marcado`, Diferencia, colour = tipo)) + geom_hline(yintercept = 0, colour = "grey60") +
    geom_point(size = 3) + ggrepel::geom_text_repel(aes(label = Termino), size = 3.8, show.legend = FALSE, max.overlaps = 60) +
    scale_colour_manual(values = c("Sube el agrado" = "#16a34a", "Baja el agrado" = "#dc2626", "Sin efecto claro" = "#94a3b8"), name = NULL) +
    labs(x = "% de evaluaciones en que se marcó", y = "Agrado con − agrado sin", title = "Impacto de cada término en la aceptabilidad",
         subtitle = "Coloreados: diferencia significativa (t de Welch, p < 0,05)") + tema()
}

# penalidades respecto del ideal: caída del agrado vs. % de evaluaciones
g_penal <- function(pe, umbral = 20) {
  if (is.null(pe)) return(NULL)
  d <- rbind(data.frame(Termino = pe$Termino, pct = pe$`Falta %`, caida = pe$`Caída si falta`, p = pe$`p falta`, tipo = "Falta (está en el ideal, no en el producto)"),
             data.frame(Termino = pe$Termino, pct = pe$`Sobra %`, caida = pe$`Caída si sobra`, p = pe$`p sobra`, tipo = "Sobra (está en el producto, no en el ideal)"))
  d <- d[!is.na(d$caida), ]
  d$fuerte <- d$pct >= umbral & !is.na(d$p) & d$p < 0.05 & d$caida > 0
  ggplot(d, aes(pct, caida, colour = tipo)) +
    annotate("rect", xmin = umbral, xmax = Inf, ymin = 0, ymax = Inf, fill = "#fee2e2", alpha = 0.5) +
    geom_vline(xintercept = umbral, linetype = 2, colour = "grey60") + geom_hline(yintercept = 0, colour = "grey60") +
    geom_point(aes(size = fuerte)) + ggrepel::geom_text_repel(aes(label = Termino, fontface = ifelse(fuerte, "bold", "plain")), size = 3.7, show.legend = FALSE, max.overlaps = 60) +
    scale_size_manual(values = c(`FALSE` = 2, `TRUE` = 3.6), guide = "none") +
    scale_colour_manual(values = c("#7c3aed", "#ea580c"), name = NULL) + guides(colour = guide_legend(ncol = 1)) +
    labs(x = "% de evaluaciones", y = "Caída del agrado (puntos)", title = "Penalidades respecto del ideal",
         subtitle = sprintf("Zona roja: al menos %d%% de las evaluaciones; en negrita, caída significativa (p < 0,05)", umbral)) + tema()
}

# brecha con el ideal (puntos porcentuales): positivo = le falta, negativo = le sobra
g_brecha <- function(B) {
  df <- expand.grid(Producto = rownames(B), Termino = colnames(B), stringsAsFactors = FALSE)
  df$v <- B[cbind(df$Producto, df$Termino)]
  ggplot(df, aes(Producto, Termino, fill = v)) + geom_tile(colour = "white") + geom_text(aes(label = sprintf("%+.0f", v)), size = 3.5) +
    scale_fill_gradient2(low = "#ea580c", mid = "white", high = "#7c3aed", midpoint = 0, name = "Ideal − producto\n(puntos %)") +
    scale_x_discrete(position = "top") + labs(x = NULL, y = NULL, title = "Diferencia con el ideal",
      subtitle = "Violeta: al producto le falta (el ideal lo tiene más); naranja: le sobra") + tema() + theme(legend.position = "right", panel.grid = element_blank())
}

# mapa de preferencias interno: productos y consumidores (flechas cortas, coloreadas por segmento)
g_pref <- function(pc, cl = NULL) {
  pct <- pc$eig[, 2]
  pr <- data.frame(et = rownames(pc$ind$coord), x = pc$ind$coord[, 1], y = pc$ind$coord[, 2])
  co <- data.frame(et = rownames(pc$var$coord), x = pc$var$coord[, 1], y = pc$var$coord[, 2])
  r <- max(abs(c(pr$x, pr$y))) * 0.9
  co$seg <- if (!is.null(cl)) factor(cl[co$et]) else factor("Consumidor")
  ggplot() + ejes() +
    geom_segment(data = co, aes(x = 0, y = 0, xend = x * r, yend = y * r, colour = seg), alpha = 0.55, arrow = arrow(length = unit(0.12, "cm"))) +
    geom_point(data = pr, aes(x, y), size = 4, colour = "#1e1b4b") +
    ggrepel::geom_label_repel(data = pr, aes(x, y, label = et), fontface = "bold", label.size = 0) +
    scale_colour_manual(values = PALETA, name = if (!is.null(cl)) "Segmento" else NULL) + coord_equal() +
    labs(x = etq(1, pct[1]), y = etq(2, pct[2]), title = "Mapa de preferencias interno",
         subtitle = "Cada flecha es un consumidor, hacia lo que más le gustó") + tema()
}

g_segmentos <- function(sg) {
  df <- as.data.frame(as.table(sg$medias)); names(df) <- c("Segmento", "Producto", "Media")
  ggplot(df, aes(Producto, Media, fill = Segmento)) + geom_col(position = position_dodge(0.8), width = 0.75) +
    geom_text(aes(label = sprintf("%.1f", Media)), position = position_dodge(0.8), vjust = -0.4, size = 3.4) +
    scale_fill_manual(values = PALETA, name = NULL) + scale_y_continuous(limits = c(0, 9.5), breaks = 1:9) +
    labs(x = NULL, y = "Aceptabilidad media", title = "Aceptabilidad por segmento de consumidores") + tema()
}
