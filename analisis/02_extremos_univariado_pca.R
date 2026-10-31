# =============================================================================
# 02. Extremos, ratios corregidos, análisis univariado, transición de zonas y PCA
# Entrada: indicadores_panel.rds (salida de limpieza_indicadores.R)
# Salida : carpeta "resultados" junto al .rds, con CSV listos para Power BI y
#          gráficos PNG de control.
#
# Solo usa R base (no requiere paquetes adicionales).
# Uso: cambie RUTA_RDS si hace falta y presione Source UNA vez.
# =============================================================================

# ---- CONFIGURACIÓN ----------------------------------------------------------
RUTA_RDS <- "indicadores_panel.rds"
P_INF <- 0.01   # winsorización: percentil inferior (por año)
P_SUP <- 0.99   # winsorización: percentil superior (por año)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) RUTA_RDS <- args[1]
CARPETA <- file.path(dirname(RUTA_RDS), "resultados")
dir.create(CARPETA, showWarnings = FALSE)
t0 <- Sys.time()
guardar <- function(df, nombre) {
  write.csv(df, file.path(CARPETA, nombre), row.names = FALSE, na = "",
            fileEncoding = "UTF-8")
}

panel <- readRDS(RUTA_RDS)
stopifnot(nrow(panel) == length(unique(panel$nit)) * length(unique(panel$anio)))
cat("Base:", nrow(panel), "filas,", length(unique(panel$nit)), "NIT\n")

# ---- 1. RATIOS CORREGIDOS -----------------------------------------------------
# La plantilla tiene tres ratios cuya fórmula no corresponde a su definición
# en CONVENCIONES. Se conservan los originales y se agregan versiones corregidas,
# reconstruidas sin volver al Excel:
#   om_c = utilidad operacional / ingresos  = x3 / x5   (ER!T/ESF!AM) / (ER!L/ESF!AM)
#   wk_c = (activo cte - pasivo cte) / activo total = x1
#   st_c = proveedores / costo de ventas * 365 = st * 365
panel$om_c <- ifelse(!is.na(panel$x5) & panel$x5 != 0, panel$x3 / panel$x5, NA)
panel$wk_c <- panel$x1
panel$st_c <- panel$st * 365
# Patrimonio negativo: x4 = patrimonio / pasivo total < 0
panel$patrimonio_negativo <- as.integer(!is.na(panel$x4) & panel$x4 < 0)

# Variables de análisis (ratios con definición correcta)
VARS <- c("cr", "at", "wk_c", "ct",            # liquidez
          "ti", "fi", "tl", "fl",              # endeudamiento
          "gm", "om_c", "nm", "re", "ra",      # rentabilidad
          "ar", "it", "st_c",                  # eficiencia (días)
          "x1", "x2", "x3", "x4", "x5", "z")   # Altman Z'
GRUPO <- c(rep("Liquidez", 4), rep("Endeudamiento", 4), rep("Rentabilidad", 5),
           rep("Eficiencia", 3), rep("Altman Z'", 6))
names(GRUPO) <- VARS

# ---- 2. DIAGNÓSTICO DE EXTREMOS ------------------------------------------------
cat("Diagnóstico de extremos...\n")
asimetria <- function(x) { x <- x[!is.na(x)]; m <- mean(x); s <- sd(x)
  if (s == 0) NA else mean((x - m)^3) / s^3 }
curtosis <- function(x) { x <- x[!is.na(x)]; m <- mean(x); s <- sd(x)
  if (s == 0) NA else mean((x - m)^4) / s^4 - 3 }
diag <- do.call(rbind, lapply(VARS, function(v) {
  x <- panel[[v]]; q <- quantile(x, c(0, .01, .05, .25, .5, .75, .95, .99, 1), na.rm = TRUE)
  fuera <- 0
  for (a in unique(panel$anio)) {
    i <- panel$anio == a & !is.na(x)
    l <- quantile(x[i], c(P_INF, P_SUP))
    fuera <- fuera + sum(x[i] < l[1] | x[i] > l[2])
  }
  data.frame(variable = v, grupo = GRUPO[[v]], n = sum(!is.na(x)), n_na = sum(is.na(x)),
             media = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
             min = q[1], p1 = q[2], p5 = q[3], p25 = q[4], mediana = q[5], p75 = q[6],
             p95 = q[7], p99 = q[8], max = q[9],
             asimetria = asimetria(x), curtosis_exceso = curtosis(x),
             n_winsorizados = fuera, pct_winsorizados = 100 * fuera / sum(!is.na(x)),
             row.names = NULL)
}))
guardar(diag, "01_diagnostico_extremos.csv")

# ---- 3. WINSORIZACIÓN POR AÑO --------------------------------------------------
cat("Winsorizando al", P_INF * 100, "% y", P_SUP * 100, "% por año...\n")
winsor <- function(x, g) {
  out <- x
  for (a in unique(g)) {
    i <- g == a & !is.na(x)
    l <- quantile(x[i], c(P_INF, P_SUP), type = 7)
    out[i] <- pmin(pmax(x[i], l[1]), l[2])
  }
  out
}
for (v in VARS) panel[[paste0(v, "_w")]] <- winsor(panel[[v]], panel$anio)
VW <- paste0(VARS, "_w")

# Comparación antes y después
comp <- do.call(rbind, lapply(VARS, function(v) {
  x <- panel[[v]]; w <- panel[[paste0(v, "_w")]]
  data.frame(variable = v, media_cruda = mean(x, na.rm = TRUE), media_w = mean(w, na.rm = TRUE),
             sd_cruda = sd(x, na.rm = TRUE), sd_w = sd(w, na.rm = TRUE),
             mediana = median(x, na.rm = TRUE),
             curtosis_cruda = curtosis(x), curtosis_w = curtosis(w), row.names = NULL)
}))
guardar(comp, "02_antes_despues_winsorizacion.csv")

# ---- 4. DESCRIPTIVA ------------------------------------------------------------
cat("Estadística descriptiva...\n")
describir <- function(df, por) {
  claves <- unique(df[[por]]); claves <- claves[!is.na(claves)]
  do.call(rbind, lapply(sort(claves), function(k) {
    d <- df[!is.na(df[[por]]) & df[[por]] == k, ]
    do.call(rbind, lapply(VARS, function(v) {
      w <- d[[paste0(v, "_w")]]; x <- d[[v]]
      q <- quantile(w, c(.25, .5, .75), na.rm = TRUE)
      data.frame(dimension = por, categoria = as.character(k), variable = v,
                 grupo = GRUPO[[v]], n = sum(!is.na(x)), pct_na = 100 * mean(is.na(x)),
                 media_w = mean(w, na.rm = TRUE), sd_w = sd(w, na.rm = TRUE),
                 cv_w = sd(w, na.rm = TRUE) / abs(mean(w, na.rm = TRUE)),
                 p25 = q[1], mediana = q[2], p75 = q[3], riq = q[3] - q[1],
                 row.names = NULL)
    }))
  }))
}
panel$todas <- "Total"
desc <- rbind(describir(panel, "todas"), describir(panel, "anio"),
              describir(panel, "ciiu_seccion"), describir(panel, "zona"),
              describir(panel, "departamento"))
guardar(desc, "03_descriptiva.csv")

# ---- 5. TENDENCIAS: efecto año con efectos fijos de empresa ----------------------
# y_it = a_i + b * anio + e_it ; error estándar agrupado por empresa (cluster)
cat("Tendencias...\n")
tendencia <- function(y, id, t) {
  ok <- !is.na(y); y <- y[ok]; id <- id[ok]; t <- t[ok]
  yd <- y - ave(y, id); td <- t - ave(t, id)
  b <- sum(yd * td) / sum(td^2); e <- yd - b * td
  sg <- tapply(td * e, id, sum)
  G <- length(sg); N <- length(y)
  v <- sum(sg^2) / sum(td^2)^2 * G / (G - 1) * (N - 1) / (N - 2)
  c(b = b, se = sqrt(v), n = N, G = G)
}
tend <- do.call(rbind, lapply(VARS, function(v) {
  r <- tendencia(panel[[paste0(v, "_w")]], panel$nit, panel$anio)
  med <- median(panel[[v]], na.rm = TRUE)
  data.frame(variable = v, grupo = GRUPO[[v]], pendiente_anual = r[["b"]],
             error_est_cluster = r[["se"]], t = r[["b"]] / r[["se"]],
             p_valor = 2 * pnorm(-abs(r[["b"]] / r[["se"]])),
             pendiente_pct_mediana = 100 * r[["b"]] / abs(med),
             mediana_2016 = median(panel[[v]][panel$anio == min(panel$anio)], na.rm = TRUE),
             mediana_final = median(panel[[v]][panel$anio == max(panel$anio)], na.rm = TRUE),
             n = r[["n"]], empresas = r[["G"]], row.names = NULL)
}))
# Medida robusta (principal): pendiente propia de cada empresa (MCO sobre sus 10 años),
# luego mediana entre empresas y prueba de rangos con signo de Wilcoxon (H0: mediana = 0).
# No depende de las colas porque cada empresa pesa lo mismo y se usa la mediana.
pend_empresa <- function(y, id, t) {
  ok <- !is.na(y); y <- y[ok]; id <- id[ok]; t <- t[ok]
  yd <- y - ave(y, id); td <- t - ave(t, id)
  num <- tapply(yd * td, id, sum); den <- tapply(td^2, id, sum); nn <- tapply(y, id, length)
  (num / den)[nn >= 5 & den > 0]          # empresas con al menos 5 años con dato
}
rob <- do.call(rbind, lapply(VARS, function(v) {
  b <- pend_empresa(panel[[v]], panel$nit, panel$anio)
  data.frame(variable = v, pendiente_mediana_empresas = median(b),
             pct_empresas_al_alza = 100 * mean(b > 0),
             p_wilcoxon = wilcox.test(b, mu = 0, exact = FALSE)$p.value,
             empresas_pendiente = length(b), row.names = NULL)
}))
tend <- merge(rob, tend, by = "variable", sort = FALSE)
tend$pendiente_mediana_pct <- 100 * tend$pendiente_mediana_empresas / abs(tend$mediana_2016)
tend$significativa_1pct <- tend$p_wilcoxon < 0.01
names(tend)[names(tend) == "pendiente_anual"] <- "pendiente_media_w_ef"
guardar(tend, "04_tendencias.csv")

# ---- 6. ZONAS Y MATRIZ DE TRANSICIÓN -------------------------------------------
cat("Zonas y transiciones...\n")
zonas_anio <- as.data.frame(table(anio = panel$anio, zona = panel$zona, useNA = "no"))
zonas_anio$pct <- 100 * zonas_anio$Freq / ave(zonas_anio$Freq, zonas_anio$anio, FUN = sum)
names(zonas_anio)[3] <- "empresas"
guardar(zonas_anio, "05_zonas_por_anio.csv")

p <- panel[order(panel$nit, panel$anio), c("nit", "anio", "zona")]
sig <- p[-1, ]; act <- p[-nrow(p), ]
ok <- sig$nit == act$nit & sig$anio == act$anio + 1
tr <- data.frame(anio_desde = act$anio[ok], desde = act$zona[ok], hacia = sig$zona[ok])
tr <- tr[!is.na(tr$desde) & !is.na(tr$hacia), ]
m <- table(desde = tr$desde, hacia = tr$hacia)
mp <- prop.table(m, 1)
trans <- as.data.frame(m); names(trans)[3] <- "casos"
trans$pct_fila <- 100 * as.vector(mp)
guardar(trans, "06_transicion_zonas_total.csv")

tr_anio <- as.data.frame(table(anio_desde = tr$anio_desde, desde = tr$desde, hacia = tr$hacia))
names(tr_anio)[4] <- "casos"
tr_anio$pct_fila <- 100 * tr_anio$casos /
  ave(tr_anio$casos, tr_anio$anio_desde, tr_anio$desde, FUN = sum)
guardar(tr_anio, "07_transicion_zonas_por_anio.csv")

# Probabilidad de caer a Distress en t+1 y su intervalo de confianza 95%
caida <- do.call(rbind, lapply(levels(tr$desde), function(z) {
  d <- tr[tr$desde == z, ]
  k <- sum(d$hacia == "Distress Zone"); n <- nrow(d)
  ci <- binom.test(k, n)$conf.int
  data.frame(desde = z, casos = n, a_distress = k, prob = k / n,
             ic95_inf = ci[1], ic95_sup = ci[2], row.names = NULL)
}))
guardar(caida, "08_prob_caer_a_distress.csv")

# Años consecutivos en Distress por empresa
rachas <- tapply(p$zona == "Distress Zone", p$nit, function(x) {
  x[is.na(x)] <- FALSE; r <- rle(x); if (any(r$values)) max(r$lengths[r$values]) else 0 })
guardar(as.data.frame(table(max_anios_seguidos_en_distress = rachas)),
        "09_rachas_distress.csv")

# ---- 7. PCA --------------------------------------------------------------------
# Sobre ratios winsorizados y estandarizados. Se excluyen it y st_c (23% NA:
# empresas sin costo de ventas) y los X/Z (son combinación de otros ratios).
cat("PCA...\n")
VPCA <- c("cr_w", "at_w", "wk_c_w", "ct_w", "ti_w", "fi_w", "tl_w", "fl_w",
          "gm_w", "om_c_w", "nm_w", "re_w", "ra_w", "ar_w")
# Los ratios tienen colas extremas aun winsorizados (p99 de días de cartera: ~16.000
# días). Para que el PCA no quede dominado por esas colas, cada ratio se transforma
# dentro de cada año a su puntaje normal por rango: qnorm((rango - 0,5) / n).
# Equivale a un PCA sobre correlaciones de rangos (tipo Spearman).
rango_normal <- function(x, g) {
  out <- rep(NA_real_, length(x))
  for (a in unique(g)) {
    i <- which(g == a & !is.na(x))
    out[i] <- qnorm((rank(x[i]) - 0.5) / length(i))
  }
  out
}
RN <- sapply(sub("_w$", "", VPCA), function(v) rango_normal(panel[[v]], panel$anio))
colnames(RN) <- VPCA
cc <- complete.cases(RN)
pca <- prcomp(RN[cc, ], center = TRUE, scale. = TRUE)
var_exp <- pca$sdev^2
pca_var <- data.frame(componente = paste0("PC", seq_along(var_exp)), autovalor = var_exp,
                      pct_varianza = 100 * var_exp / sum(var_exp),
                      pct_acumulado = 100 * cumsum(var_exp) / sum(var_exp),
                      kaiser_mayor_1 = var_exp > 1)
guardar(pca_var, "10_pca_varianza.csv")
nk <- max(2, sum(var_exp > 1))
cargas <- as.data.frame(pca$rotation[, 1:nk])
cargas$variable <- sub("_w$", "", rownames(cargas))
cargas$grupo <- GRUPO[cargas$variable]
guardar(cargas[, c("variable", "grupo", paste0("PC", 1:nk))], "11_pca_cargas.csv")
scores <- matrix(NA_real_, nrow(panel), nk, dimnames = list(NULL, paste0("PC", 1:nk)))
scores[cc, ] <- pca$x[, 1:nk]
panel <- cbind(panel, scores)
cat("  filas completas para PCA:", sum(cc), "de", nrow(panel), "| componentes con autovalor > 1:", nk, "\n")

# ---- 8. TABLA ANALÍTICA PARA POWER BI ---------------------------------------------
panel$todas <- NULL
guardar(panel, "12_panel_analitico.csv")
saveRDS(panel, file.path(CARPETA, "panel_analitico.rds"))

# ---- 9. GRÁFICOS DE CONTROL ------------------------------------------------------
cat("Gráficos...\n")
png(file.path(CARPETA, "g1_boxplots_por_anio.png"), 1600, 1000, res = 110)
op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (v in c("cr", "ti", "om_c", "ra", "ar", "z"))  # cajas sin atípicos dibujados
  boxplot(panel[[paste0(v, "_w")]] ~ panel$anio, outline = FALSE, las = 2,
          main = paste0(toupper(v), " (winsorizado)"), xlab = "", ylab = "", col = "grey90")
par(op); invisible(dev.off())

png(file.path(CARPETA, "g2_zonas_por_anio.png"), 1200, 700, res = 110)
tz <- prop.table(table(panel$zona, panel$anio), 2) * 100
op <- par(mar = c(5, 4, 4, 10), xpd = TRUE)
barplot(tz, col = c("#c0392b", "#f1c40f", "#27ae60"), border = NA, las = 2,
        ylab = "% de empresas", main = "Zonas Z' de Altman por año")
legend("topright", inset = c(-0.2, 0), legend = rev(rownames(tz)),
       fill = rev(c("#c0392b", "#f1c40f", "#27ae60")), bty = "n", cex = .85)
par(op)
invisible(dev.off())

png(file.path(CARPETA, "g3_pca_scree_cargas.png"), 1600, 700, res = 110)
op <- par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
plot(var_exp, type = "b", pch = 19, xlab = "Componente", ylab = "Autovalor",
     main = "PCA: autovalores"); abline(h = 1, lty = 2)
rot <- pca$rotation[, 1:2]; lim <- c(-1, 1) * max(abs(rot)) * 1.15
plot(0, 0, type = "n", xlim = lim, ylim = lim, xlab = "PC1", ylab = "PC2",
     main = "PCA: cargas PC1 y PC2"); abline(h = 0, v = 0, col = "grey80")
arrows(0, 0, rot[, 1], rot[, 2], length = .08, col = "grey40")
text(rot[, 1] * 1.08, rot[, 2] * 1.08, sub("_w$", "", rownames(rot)), cex = .85)
par(op); invisible(dev.off())

# ---- 10. RESUMEN --------------------------------------------------------------------
cat("\n================ RESUMEN ================\n")
cat("Variables donde la cola domina aun winsorizada (media_w > 2 x mediana):\n ",
    paste(comp$variable[abs(comp$media_w) > 2 * abs(comp$mediana)], collapse = ", "),
    "\n  -> para estas use mediana y RIQ, no la media.\n")
cat("Empresas-año con patrimonio negativo:", sum(panel$patrimonio_negativo), "\n")
cat("\nTendencias (robustas): pendiente mediana por empresa, % de empresas al alza, Wilcoxon\n")
print(tend[, c("variable", "mediana_2016", "mediana_final", "pendiente_mediana_empresas",
               "pct_empresas_al_alza", "p_wilcoxon", "significativa_1pct")],
      row.names = FALSE, digits = 3)
cat("\nMatriz de transición (% por fila, t -> t+1):\n"); print(round(100 * mp, 1))
cat("\nProbabilidad de caer a Distress al año siguiente:\n")
print(caida, row.names = FALSE, digits = 3)
cat("\nPCA: % varianza acumulada en", nk, "componentes:",
    round(pca_var$pct_acumulado[nk], 1), "\n")
print(round(as.matrix(cargas[, paste0("PC", 1:nk)]), 2))
cat("\nResultados en:", CARPETA, "\n")
cat("Tiempo:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")
