# =============================================================================
# 03. Análisis bivariado
#   A. Correlaciones de Spearman (total, por año e intra-empresa)
#   B. Riesgo de liquidez: cuadrantes liquidez x endeudamiento
#   C. ¿Qué significa endeudarse? Endeudamiento vs rentabilidad (efectos fijos)
#   D. Z' de Altman: qué indicadores se asocian al Z', contribución de cada X,
#      qué X explica las caídas a Distress, y zonas por sector
# Entrada: resultados/panel_analitico.rds (salida del script 02)
# Salida : resultados/bivariado/  (CSV para Power BI y gráficos PNG)
# Solo usa R base. Uso: presione Source UNA vez.
# =============================================================================

# ---- CONFIGURACIÓN ----------------------------------------------------------
RUTA_PANEL <- "resultados/panel_analitico.rds"
UMBRAL_LIQUIDEZ <- 1.0   # razón corriente < 1: el pasivo corriente supera al activo corriente
UMBRAL_DEUDA    <- 0.7   # endeudamiento total > 0,7: más del 70% del activo financiado con terceros
MIN_EMPRESAS_SECTOR <- 30  # secciones CIIU con menos empresas no se reportan

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) RUTA_PANEL <- args[1]
CARPETA <- file.path(dirname(RUTA_PANEL), "bivariado")
dir.create(CARPETA, showWarnings = FALSE)
t0 <- Sys.time()
guardar <- function(df, nombre) write.csv(df, file.path(CARPETA, nombre), row.names = FALSE,
                                          na = "", fileEncoding = "UTF-8")
panel <- readRDS(RUTA_PANEL)
panel <- panel[order(panel$nit, panel$anio), ]
cat("Base:", nrow(panel), "filas,", length(unique(panel$nit)), "NIT\n")

VARS <- c("cr", "at", "wk_c", "ct", "ti", "fi", "tl", "fl", "gm", "om_c", "nm",
          "re", "ra", "ar", "it", "st_c", "x1", "x2", "x3", "x4", "x5", "z")
GRUPO <- setNames(c(rep("Liquidez", 4), rep("Endeudamiento", 4), rep("Rentabilidad", 5),
                    rep("Eficiencia", 3), rep("Altman Z'", 6)), VARS)
magnitud <- function(r) cut(abs(r), c(-Inf, .1, .3, .5, Inf),
                            labels = c("despreciable", "débil", "moderada", "fuerte"))

# Valor del año siguiente para cada fila (panel ordenado por nit y anio)
siguiente <- function(x) {
  s <- c(x[-1], NA)
  mismo <- c(panel$nit[-1], NA) == panel$nit & c(panel$anio[-1], NA) == panel$anio + 1
  s[is.na(mismo) | !mismo] <- NA
  s
}

# Regresión con efectos fijos de empresa y de año, errores estándar agrupados por empresa
fe_cluster <- function(y, X, id, anio) {
  X <- as.matrix(X)
  ok <- complete.cases(y, X); y <- y[ok]; X <- X[ok, , drop = FALSE]; id <- id[ok]; anio <- anio[ok]
  D <- model.matrix(~ factor(anio))[, -1, drop = FALSE]           # dummies de año
  Z <- cbind(X, D)
  dm <- function(v) v - ave(v, id)                                  # quita la media de cada empresa
  yd <- dm(y); Zd <- apply(Z, 2, dm)
  XtX_inv <- solve(crossprod(Zd)); b <- XtX_inv %*% crossprod(Zd, yd); e <- as.vector(yd - Zd %*% b)
  S <- rowsum(Zd * e, id)                                            # sumas por empresa
  G <- nrow(S); N <- length(y); K <- ncol(Zd) + G
  V <- XtX_inv %*% crossprod(S) %*% XtX_inv * G / (G - 1) * (N - 1) / (N - K)
  k <- ncol(X)
  list(coef = b[1:k], se = sqrt(diag(V))[1:k], vcov = V[1:k, 1:k, drop = FALSE],
       n = N, empresas = G, r2_within = 1 - sum(e^2) / sum((yd - mean(yd))^2))
}

# =============================================================================
# A. CORRELACIONES DE SPEARMAN
# =============================================================================
cat("A. Correlaciones...\n")
largo <- function(M, etiqueta) {
  d <- as.data.frame(as.table(M)); names(d) <- c("var1", "var2", "rho")
  d$corte <- etiqueta; d$grupo1 <- GRUPO[as.character(d$var1)]; d$grupo2 <- GRUPO[as.character(d$var2)]
  d$magnitud <- magnitud(d$rho); d
}
M_total <- cor(panel[, VARS], method = "spearman", use = "pairwise.complete.obs")
cor_largo <- largo(M_total, "Total")
for (a in sort(unique(panel$anio))) {
  d <- panel[panel$anio == a, VARS]
  cor_largo <- rbind(cor_largo, largo(cor(d, method = "spearman", use = "pairwise.complete.obs"),
                                      as.character(a)))
}
guardar(cor_largo, "A1_spearman_total_y_por_anio.csv")

# Estabilidad: rango de rho entre años para cada par
pares <- subset(cor_largo, corte != "Total" & as.character(var1) < as.character(var2))
estab <- aggregate(rho ~ var1 + var2, pares, function(r) c(min = min(r), max = max(r)))
estab <- data.frame(var1 = estab$var1, var2 = estab$var2, rho_min = estab$rho[, "min"],
                    rho_max = estab$rho[, "max"])
estab$rho_total <- M_total[cbind(as.character(estab$var1), as.character(estab$var2))]
estab$rango <- estab$rho_max - estab$rho_min
guardar(estab[order(-abs(estab$rho_total)), ], "A2_estabilidad_por_anio.csv")

# Intra-empresa: correlación de los cambios dentro de cada empresa (rangos por año,
# luego se resta la media de cada empresa). Responde: cuando ESTA empresa sube X, ¿sube Y?
rn <- sapply(VARS, function(v) {
  x <- panel[[v]]; out <- rep(NA_real_, length(x))
  for (a in unique(panel$anio)) { i <- which(panel$anio == a & !is.na(x))
    out[i] <- qnorm((rank(x[i]) - .5) / length(i)) }
  out - ave(out, panel$nit, FUN = function(z) mean(z, na.rm = TRUE))
})
M_intra <- cor(rn, use = "pairwise.complete.obs")
guardar(largo(M_intra, "Intra-empresa"), "A3_correlacion_intra_empresa.csv")

png(file.path(CARPETA, "gA_mapa_correlaciones.png"), 1300, 1150, res = 120)
op <- par(mar = c(6, 6, 3, 2))
pal <- colorRampPalette(c("#b2182b", "#f7f7f7", "#2166ac"))(41)
n <- length(VARS)
image(1:n, 1:n, M_total[, n:1], col = pal, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "",
      main = "Correlación de Spearman (todas las empresas-año)")
axis(1, 1:n, VARS, las = 2, cex.axis = .8); axis(2, 1:n, rev(VARS), las = 1, cex.axis = .8)
for (i in 1:n) for (j in 1:n) text(i, n + 1 - j, sprintf("%.1f", M_total[i, j]), cex = .5)
par(op); invisible(dev.off())

# =============================================================================
# B. RIESGO DE LIQUIDEZ: CUADRANTES LIQUIDEZ x ENDEUDAMIENTO
# =============================================================================
cat("B. Cuadrantes de riesgo de liquidez...\n")
liq_baja  <- panel$cr < UMBRAL_LIQUIDEZ
deuda_alta <- panel$ti > UMBRAL_DEUDA
panel$cuadrante <- ifelse(is.na(liq_baja) | is.na(deuda_alta), NA,
  ifelse(liq_baja & deuda_alta, "1 Crítico: baja liquidez y alta deuda",
  ifelse(liq_baja, "2 Tensión de liquidez: baja liquidez, deuda moderada",
  ifelse(deuda_alta, "3 Apalancada con liquidez: alta deuda, liquidez suficiente",
         "4 Holgada: liquidez suficiente y deuda moderada"))))
cuad_anio <- as.data.frame(table(anio = panel$anio, cuadrante = panel$cuadrante))
names(cuad_anio)[3] <- "empresas"
cuad_anio$pct <- 100 * cuad_anio$empresas / ave(cuad_anio$empresas, cuad_anio$anio, FUN = sum)
guardar(cuad_anio, "B1_cuadrantes_por_anio.csv")

# Zona Z' y probabilidad de estar en Distress el año siguiente, por cuadrante
panel$zona_sig <- siguiente(as.character(panel$zona))
cuad_riesgo <- do.call(rbind, lapply(sort(unique(na.omit(panel$cuadrante))), function(q) {
  d <- panel[!is.na(panel$cuadrante) & panel$cuadrante == q, ]
  dd <- d[!is.na(d$zona_sig), ]; k <- sum(dd$zona_sig == "Distress Zone"); nn <- nrow(dd)
  ci <- binom.test(k, nn)$conf.int
  data.frame(cuadrante = q, empresas_anio = nrow(d), pct_del_total = 100 * nrow(d) / sum(!is.na(panel$cuadrante)),
             pct_distress_hoy = 100 * mean(d$zona == "Distress Zone", na.rm = TRUE),
             prob_distress_t1 = k / nn, ic95_inf = ci[1], ic95_sup = ci[2],
             mediana_ra = median(d$ra, na.rm = TRUE), mediana_om_c = median(d$om_c, na.rm = TRUE),
             row.names = NULL)
}))
guardar(cuad_riesgo, "B2_cuadrantes_riesgo.csv")

# Migración entre cuadrantes (t -> t+1)
panel$cuadrante_sig <- siguiente(panel$cuadrante)
mig <- table(desde = panel$cuadrante, hacia = panel$cuadrante_sig)
mig_df <- as.data.frame(mig); names(mig_df)[3] <- "casos"
mig_df$pct_fila <- 100 * as.vector(prop.table(mig, 1))
guardar(mig_df, "B3_migracion_cuadrantes.csv")

png(file.path(CARPETA, "gB_cuadrantes_por_anio.png"), 1300, 750, res = 120)
op <- par(mar = c(5, 4, 4, 16), xpd = TRUE)
tq <- prop.table(table(substr(panel$cuadrante, 1, 1), panel$anio), 2) * 100
cols <- c("#c0392b", "#e67e22", "#f1c40f", "#27ae60")
barplot(tq, col = cols, border = NA, las = 2, ylab = "% de empresas",
        main = "Cuadrantes de liquidez y endeudamiento por año")
legend("topright", inset = c(-0.42, 0), bty = "n", cex = .75, fill = rev(cols),
       legend = rev(c("Crítico", "Tensión de liquidez", "Apalancada con liquidez", "Holgada")))
par(op); invisible(dev.off())

# =============================================================================
# C. ¿QUÉ SIGNIFICA ENDEUDARSE? ENDEUDAMIENTO VS RENTABILIDAD
# =============================================================================
cat("C. Endeudamiento vs rentabilidad...\n")
# C1. Rentabilidad mediana por decil de endeudamiento (deciles dentro de cada año)
panel$decil_ti <- ave(panel$ti, panel$anio, FUN = function(x)
  if (all(is.na(x))) NA else cut(x, unique(quantile(x, 0:10 / 10, na.rm = TRUE)), labels = FALSE,
                                 include.lowest = TRUE))
dec <- do.call(rbind, lapply(1:10, function(k) {
  d <- panel[!is.na(panel$decil_ti) & panel$decil_ti == k, ]
  data.frame(decil_endeudamiento = k, ti_mediana = median(d$ti, na.rm = TRUE),
             ra_mediana = median(d$ra, na.rm = TRUE),
             re_mediana = median(d$re[d$patrimonio_negativo == 0], na.rm = TRUE),
             om_c_mediana = median(d$om_c, na.rm = TRUE), nm_mediana = median(d$nm, na.rm = TRUE),
             cr_mediana = median(d$cr, na.rm = TRUE),
             pct_distress = 100 * mean(d$zona == "Distress Zone", na.rm = TRUE),
             n = nrow(d), row.names = NULL)
}))
guardar(dec, "C1_rentabilidad_por_decil_endeudamiento.csv")

# C2. Efectos fijos de empresa y año: rentabilidad ~ ti + ti^2 (winsorizados)
#     Compara a cada empresa consigo misma: si ESTA empresa se endeuda más, ¿qué pasa con
#     su rentabilidad? Es asociación, no causalidad (las empresas rentables pueden
#     endeudarse menos porque se financian con utilidades: teoría del pecking order).
modelos <- list(
  list(y = "ra_w",   etiqueta = "ROA (RA)", filtro = rep(TRUE, nrow(panel))),
  list(y = "re_w",   etiqueta = "ROE (RE), sin patrimonio negativo", filtro = panel$patrimonio_negativo == 0),
  list(y = "om_c_w", etiqueta = "Margen operacional corregido", filtro = rep(TRUE, nrow(panel))),
  list(y = "nm_w",   etiqueta = "Margen neto", filtro = rep(TRUE, nrow(panel))))
res_fe <- do.call(rbind, lapply(modelos, function(m) {
  f <- m$filtro
  X <- cbind(ti = panel$ti_w[f], ti2 = panel$ti_w[f]^2)
  r <- fe_cluster(panel[[m$y]][f], X, panel$nit[f], panel$anio[f])
  tp <- -r$coef[1] / (2 * r$coef[2])
  # error estándar del punto de giro por método delta
  g <- c(-1 / (2 * r$coef[2]), r$coef[1] / (2 * r$coef[2]^2))
  tp_se <- sqrt(as.numeric(t(g) %*% r$vcov %*% g))
  data.frame(dependiente = m$etiqueta, coef_ti = r$coef[1], se_ti = r$se[1],
             p_ti = 2 * pnorm(-abs(r$coef[1] / r$se[1])), coef_ti2 = r$coef[2], se_ti2 = r$se[2],
             p_ti2 = 2 * pnorm(-abs(r$coef[2] / r$se[2])),
             punto_giro_ti = tp, punto_giro_ic95_inf = tp - 1.96 * tp_se,
             punto_giro_ic95_sup = tp + 1.96 * tp_se,
             efecto_10pp_desde_mediana = 0.1 * (r$coef[1] + 2 * r$coef[2] * median(panel$ti_w, na.rm = TRUE)),
             r2_within = r$r2_within, n = r$n, empresas = r$empresas, row.names = NULL)
}))
guardar(res_fe, "C2_efectos_fijos_rentabilidad_vs_deuda.csv")

# C3. ¿Endeudarse más este año se asocia con qué el año siguiente?
# Ojo: el endeudamiento total también sube cuando hay pérdidas (cae el patrimonio),
# así que estas asociaciones mezclan "pedir deuda" con "perder patrimonio".
panel$d_ti <- siguiente(panel$ti_w) - panel$ti_w
cambios <- data.frame(
  medida = c("Spearman entre aumento de deuda (t a t+1) y ROA en t",
             "Spearman entre aumento de deuda (t a t+1) y razón corriente en t"),
  rho = c(cor(panel$d_ti, panel$ra, method = "spearman", use = "complete.obs"),
          cor(panel$d_ti, panel$cr, method = "spearman", use = "complete.obs")))
cambios$magnitud <- magnitud(cambios$rho)
guardar(cambios, "C3_dinamica_deuda.csv")

png(file.path(CARPETA, "gC_rentabilidad_por_decil_deuda.png"), 1300, 650, res = 120)
op <- par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
plot(dec$decil_endeudamiento, 100 * dec$ra_mediana, type = "b", pch = 19,
     xlab = "Decil de endeudamiento total (1 = menos deuda)", ylab = "ROA mediano (%)",
     main = "ROA por decil de endeudamiento"); abline(h = 0, lty = 2, col = "grey60")
plot(dec$decil_endeudamiento, dec$pct_distress, type = "b", pch = 19, col = "#c0392b",
     xlab = "Decil de endeudamiento total (1 = menos deuda)", ylab = "% en Distress",
     main = "Distress por decil de endeudamiento")
par(op); invisible(dev.off())

# =============================================================================
# D. Z' DE ALTMAN
# =============================================================================
cat("D. Z' de Altman...\n")
PESOS <- c(x1 = 0.717, x2 = 0.847, x3 = 3.107, x4 = 0.42, x5 = 0.998)

# D1. ¿Qué indicadores se asocian más con el Z'? (Spearman con z; los X son componentes)
d1 <- data.frame(variable = VARS[VARS != "z"], grupo = GRUPO[VARS[VARS != "z"]],
                 rho_con_z = M_total[VARS[VARS != "z"], "z"],
                 rho_intra_empresa = M_intra[VARS[VARS != "z"], "z"])
d1$es_componente_z <- d1$variable %in% names(PESOS)
d1$magnitud <- magnitud(d1$rho_con_z)
guardar(d1[order(-abs(d1$rho_con_z)), ], "D1_indicadores_vs_z.csv")

# D2. Contribución de cada X al Z' (peso x valor), mediana por zona
contrib <- sapply(names(PESOS), function(x) PESOS[[x]] * panel[[x]])
d2 <- do.call(rbind, lapply(levels(panel$zona), function(zn) {
  i <- !is.na(panel$zona) & panel$zona == zn
  data.frame(zona = zn, componente = names(PESOS),
             contribucion_mediana = apply(contrib[i, ], 2, median, na.rm = TRUE), row.names = NULL)
}))
guardar(d2, "D2_contribucion_x_por_zona.csv")

# D3. ¿Qué X explica las caídas a Distress? Cambio de cada contribución entre t y t+1
#     para quienes caen (Gray o Safe -> Distress) frente a quienes se mantienen.
sig_c <- apply(contrib, 2, siguiente)
dc <- sig_c - contrib
cae <- panel$zona %in% c("Gray Zone", "Safe Zone") & panel$zona_sig %in% "Distress Zone"
se_mantiene <- panel$zona %in% c("Gray Zone", "Safe Zone") & panel$zona_sig %in% c("Gray Zone", "Safe Zone")
dz <- rowSums(dc)
d3 <- do.call(rbind, lapply(names(PESOS), function(x) {
  data.frame(componente = x,
             cambio_mediano_caen = median(dc[cae, x], na.rm = TRUE),
             cambio_mediano_se_mantienen = median(dc[se_mantiene, x], na.rm = TRUE),
             pct_del_deterioro_caen = 100 * median(dc[cae, x], na.rm = TRUE) / median(dz[cae], na.rm = TRUE),
             pct_caen_donde_es_la_mayor_caida = 100 * mean(apply(dc[cae, , drop = FALSE], 1,
                                                       function(r) names(which.min(r))) == x, na.rm = TRUE),
             casos_caen = sum(cae), row.names = NULL)
}))
guardar(d3, "D3_que_x_explica_caidas_a_distress.csv")

# D4. Zonas y riesgo por sector CIIU
emp_sec <- tapply(panel$nit, panel$ciiu_seccion, function(x) length(unique(x)))
sec_ok <- names(emp_sec)[emp_sec >= MIN_EMPRESAS_SECTOR]
d4 <- do.call(rbind, lapply(sec_ok, function(s) {
  d <- panel[panel$ciiu_seccion %in% s, ]
  dd <- d[!is.na(d$zona_sig) & d$zona %in% c("Gray Zone", "Safe Zone"), ]
  k <- sum(dd$zona_sig == "Distress Zone"); nn <- nrow(dd); ci <- binom.test(k, nn)$conf.int
  data.frame(ciiu_seccion = s, empresas_alguna_vez_en_seccion = emp_sec[[s]],
             pct_distress = 100 * mean(d$zona == "Distress Zone", na.rm = TRUE),
             pct_gray = 100 * mean(d$zona == "Gray Zone", na.rm = TRUE),
             pct_safe = 100 * mean(d$zona == "Safe Zone", na.rm = TRUE),
             z_mediano = median(d$z, na.rm = TRUE),
             prob_caer_a_distress_t1 = k / nn, ic95_inf = ci[1], ic95_sup = ci[2],
             row.names = NULL)
}))
d4 <- d4[order(-d4$pct_distress), ]
guardar(d4, "D4_zonas_por_sector.csv")
cat("  Secciones CIIU omitidas por tener menos de", MIN_EMPRESAS_SECTOR, "empresas:",
    paste(setdiff(names(emp_sec), sec_ok), collapse = ", "), "\n")

png(file.path(CARPETA, "gD_contribucion_x_por_zona.png"), 1200, 700, res = 120)
op <- par(mar = c(5, 5, 3, 10), xpd = TRUE)
mz <- sapply(levels(panel$zona), function(zn) d2$contribucion_mediana[d2$zona == zn])
rownames(mz) <- names(PESOS)
barplot(mz, beside = TRUE, col = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
        border = NA, ylab = "Contribución mediana al Z'", main = "Contribución de cada X al Z' por zona")
legend("topright", inset = c(-0.2, 0), bty = "n", legend = toupper(names(PESOS)),
       fill = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"))
par(op); invisible(dev.off())

# Panel con cuadrante para Power BI
guardar(panel[, c("nit", "anio", "cuadrante", "decil_ti")], "panel_cuadrantes.csv")

# =============================================================================
# RESUMEN
# =============================================================================
cat("\n================ RESUMEN BIVARIADO ================\n")
top <- estab[order(-abs(estab$rho_total)), ]
top <- top[!(top$var1 %in% c("x1", "x2", "x3", "x4", "x5", "z") & top$var2 %in% c("x1", "x2", "x3", "x4", "x5", "z")), ]
cat("A. Pares más correlacionados (Spearman total, rango entre años):\n")
print(head(top[, c("var1", "var2", "rho_total", "rho_min", "rho_max")], 12), row.names = FALSE, digits = 2)
cat("\nB. Cuadrantes de riesgo de liquidez:\n")
print(cuad_riesgo[, c("cuadrante", "pct_del_total", "pct_distress_hoy", "prob_distress_t1", "ic95_inf", "ic95_sup")],
      row.names = FALSE, digits = 3)
cat("\nC1. Rentabilidad por decil de endeudamiento:\n")
print(dec[, c("decil_endeudamiento", "ti_mediana", "ra_mediana", "re_mediana", "om_c_mediana", "pct_distress")],
      row.names = FALSE, digits = 3)
cat("\nC2. Efectos fijos (empresa y año), errores agrupados por empresa:\n")
print(res_fe[, c("dependiente", "coef_ti", "p_ti", "coef_ti2", "p_ti2", "punto_giro_ti", "efecto_10pp_desde_mediana")],
      row.names = FALSE, digits = 3)
cat("\nC3. Dinámica de la deuda:\n"); print(cambios, row.names = FALSE, digits = 3)
cat("\nD1. Indicadores más asociados al Z' (sin contar sus componentes):\n")
print(head(d1[!d1$es_componente_z, ][order(-abs(d1$rho_con_z[!d1$es_componente_z])), c("variable", "rho_con_z", "rho_intra_empresa")], 8),
      row.names = FALSE, digits = 2)
cat("\nD3. Qué X explica las caídas a Distress:\n")
print(d3[, c("componente", "cambio_mediano_caen", "cambio_mediano_se_mantienen", "pct_caen_donde_es_la_mayor_caida")],
      row.names = FALSE, digits = 3)
cat("\nD4. Zonas por sector:\n"); print(d4, row.names = FALSE, digits = 3)
cat("\nResultados en:", CARPETA, "\n")
cat("Tiempo:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")
