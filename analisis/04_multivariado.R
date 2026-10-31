# =============================================================================
# 04. Análisis multivariado
#   E1. Persistencia de cada indicador (qué tanto se parece t+1 a t)
#   E2. Modelos de probabilidad de CAER A ZONA DISTRESS el año siguiente
#       (empresas que hoy están en Gray o Safe), logit con errores agrupados por empresa:
#         M0  Solo el Z' de hoy (referencia)
#         M1  Z' + indicadores que NO forman parte del Z' + tamaño, crecimiento, sector
#         M2  Lo mismo sin el Z' (¿qué anticipa el deterioro por sí solo?)
#         M3  Índices del PCA + tamaño, crecimiento, sector
#   E3. Validación fuera de muestra: se estima con t = 2017-2020 y se prueba con
#       t = 2021-2024 (AUC, Brier, captura del decil de mayor riesgo, calibración)
#   E4. Alerta temprana: probabilidad de caer a Distress en 2026 para cada empresa
#       que en 2025 está en Gray o Safe (modelo M1 reestimado con todos los años)
#
# IMPORTANTE: el evento es "entrar a zona Distress del Z' de Altman", una señal
# contable de deterioro. NO es quiebra observada (la base no trae ese evento).
#
# Entradas: resultados/panel_analitico.rds (script 02) y el Excel (para tamaño y ventas)
# Salida  : resultados/multivariado/
# Requiere readxl (ya instalado). Uso: presione Source UNA vez.
# =============================================================================

# ---- CONFIGURACIÓN ----------------------------------------------------------
RUTA_PANEL <- "resultados/panel_analitico.rds"
RUTA_EXCEL <- "BD_INDICADORES_2016_2025_DIC.xlsx"
ANIOS_ENTRENAMIENTO <- 2017:2020   # año t de la observación (el evento se mide en t+1)
ANIOS_PRUEBA        <- 2021:2024
MIN_EMPRESAS_SECTOR <- 30

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) RUTA_PANEL <- args[1]
if (length(args) >= 2) RUTA_EXCEL <- args[2]
CARPETA <- file.path(dirname(RUTA_PANEL), "multivariado")
dir.create(CARPETA, showWarnings = FALSE)
t0 <- Sys.time()
suppressPackageStartupMessages(library(readxl))
guardar <- function(df, nombre) write.csv(df, file.path(CARPETA, nombre), row.names = FALSE,
                                          na = "", fileEncoding = "UTF-8")

panel <- readRDS(RUTA_PANEL)
panel <- panel[order(panel$nit, panel$anio), ]
rownames(panel) <- NULL

# ---- Tamaño (activo total) y ventas desde el Excel ----------------------------
cat("Leyendo activo total e ingresos del Excel...\n")
esf <- read_excel(RUTA_EXCEL, sheet = "ESF", progress = FALSE, .name_repair = "minimal",
                  col_types = c("text", "text", "numeric", rep("text", 8), rep("numeric", 61)))
er  <- read_excel(RUTA_EXCEL, sheet = "ER", progress = FALSE, .name_repair = "minimal",
                  col_types = c("text", "text", "numeric", rep("text", 8), rep("numeric", 16)))
niv <- data.frame(nit = as.integer(esf[[3]]), anio = as.integer(substr(esf[[4]], 1, 4)),
                  activo_total = esf[[39]],      # ESF!AM
                  ingresos = er[[12]])           # ER!L
stopifnot(all(esf[[3]] == er[[3]]), !anyDuplicated(niv[, c("nit", "anio")]))
rm(esf, er); invisible(gc())
panel <- merge(panel, niv, by = c("nit", "anio"), all.x = TRUE, sort = FALSE)
panel <- panel[order(panel$nit, panel$anio), ]
rownames(panel) <- NULL
stopifnot(nrow(panel) == nrow(niv))
# Celda vacía en el Excel = 0 (igual que en las fórmulas)
panel$activo_total[is.na(panel$activo_total)] <- 0
panel$ingresos[is.na(panel$ingresos)] <- 0

# ---- Utilidades ---------------------------------------------------------------
anterior <- function(x) {
  a <- c(NA, x[-length(x)])
  ok <- c(NA, panel$nit[-nrow(panel)]) == panel$nit & c(NA, panel$anio[-nrow(panel)]) == panel$anio - 1
  a[is.na(ok) | !ok] <- NA; a
}
siguiente <- function(x) {
  s <- c(x[-1], NA)
  ok <- c(panel$nit[-1], NA) == panel$nit & c(panel$anio[-1], NA) == panel$anio + 1
  s[is.na(ok) | !ok] <- NA; s
}
# Puntaje normal por rango dentro de cada año: robusto a colas, sin inflación
rango_normal <- function(x, g = panel$anio) {
  out <- rep(NA_real_, length(x))
  for (a in unique(g)) { i <- which(g == a & !is.na(x))
    if (length(i)) out[i] <- qnorm((rank(x[i]) - .5) / length(i)) }
  out
}
auc <- function(p, y) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }

# =============================================================================
# E1. PERSISTENCIA
# =============================================================================
cat("E1. Persistencia...\n")
VARS <- c("cr", "at", "wk_c", "ct", "ti", "fi", "tl", "fl", "gm", "om_c", "nm",
          "re", "ra", "ar", "it", "st_c", "z")
pers <- do.call(rbind, lapply(VARS, function(v) {
  x <- panel[[v]]; s <- siguiente(x); ok <- !is.na(x) & !is.na(s)
  rho <- cor(x[ok], s[ok], method = "spearman")
  # vida media de una desviación: años hasta que se diluye la mitad (si rho > 0)
  data.frame(variable = v, rho_t_t1 = rho, vida_media_anios = ifelse(rho > 0, log(.5) / log(rho), NA),
             pares = sum(ok), row.names = NULL)
}))
guardar(pers[order(-pers$rho_t_t1), ], "E1_persistencia.csv")

# =============================================================================
# E2. MODELOS
# =============================================================================
cat("E2. Preparando variables...\n")
panel$zona_sig <- siguiente(as.character(panel$zona))
panel$cae_distress <- as.integer(panel$zona_sig == "Distress Zone")
crec <- ifelse(panel$ingresos > 0 & anterior(panel$ingresos) > 0,
               log(panel$ingresos / anterior(panel$ingresos)), NA)

# Predictores en puntaje normal por año (1 unidad ~ 1 desviación estándar).
# Faltantes -> 0 (la mediana) con una variable indicadora del motivo.
pred <- data.frame(
  z_rn        = rango_normal(panel$z),
  gray        = as.integer(panel$zona == "Gray Zone"),
  cr_rn       = rango_normal(panel$cr),
  ct_rn       = rango_normal(panel$ct),
  fi_rn       = rango_normal(panel$fi),
  fl_rn       = rango_normal(panel$fl),
  gm_rn       = rango_normal(panel$gm),
  om_c_rn     = rango_normal(panel$om_c),
  nm_rn       = rango_normal(panel$nm),
  ra_rn       = rango_normal(panel$ra),
  ar_rn       = rango_normal(panel$ar),
  tamano_rn   = rango_normal(log(pmax(panel$activo_total, 1))),
  crec_rn     = rango_normal(crec),
  pat_neg     = panel$patrimonio_negativo,
  PC1 = panel$PC1, PC2 = panel$PC2, PC3 = panel$PC3, PC4 = panel$PC4)
pred$sin_ventas          <- as.integer(is.na(panel$x5) | panel$x5 == 0)
pred$sin_pasivo_corriente <- as.integer(is.na(panel$cr))
pred$sin_crecimiento     <- as.integer(is.na(pred$crec_rn))
for (v in names(pred)) pred[[v]][is.na(pred[[v]])] <- 0
pred$sin_pca <- as.integer(is.na(panel$PC1))

# Sector: secciones CIIU con pocas empresas se agrupan en "Otros"; base = comercio (G)
emp_sec <- tapply(panel$nit, panel$ciiu_seccion, function(x) length(unique(x)))
sec <- ifelse(panel$ciiu_seccion %in% names(emp_sec)[emp_sec >= MIN_EMPRESAS_SECTOR],
              panel$ciiu_seccion, "Otros")
sec[is.na(sec)] <- "Otros"
pred$sector <- relevel(factor(sec), ref = "G")

EXTRAS <- c("cr_rn", "ct_rn", "fi_rn", "fl_rn", "gm_rn", "om_c_rn", "nm_rn", "ra_rn", "ar_rn",
            "tamano_rn", "crec_rn", "pat_neg", "sin_ventas", "sin_pasivo_corriente", "sector")
FORMULAS <- list(
  M0 = c("z_rn", "gray"),
  M1 = c("z_rn", "gray", EXTRAS),
  M2 = EXTRAS,
  M3 = c("PC1", "PC2", "PC3", "PC4", "sin_pca", "tamano_rn", "crec_rn", "sin_ventas", "sector"))
NOMBRES <- c(M0 = "M0 Solo Z'", M1 = "M1 Z' + indicadores", M2 = "M2 Indicadores sin Z'",
             M3 = "M3 Índices PCA")

# Muestra: empresas en Gray o Safe en t con zona observada en t+1, desde 2017 (crecimiento)
base <- cbind(pred, y = panel$cae_distress, nit = panel$nit, anio = panel$anio)
muestra <- panel$zona %in% c("Gray Zone", "Safe Zone") & !is.na(panel$zona_sig)
ent <- base[muestra & panel$anio %in% ANIOS_ENTRENAMIENTO, ]
pru <- base[muestra & panel$anio %in% ANIOS_PRUEBA, ]
cat("  Entrenamiento:", nrow(ent), "obs,", sum(ent$y), "caídas | Prueba:", nrow(pru), "obs,",
    sum(pru$y), "caídas\n")

# Logit con errores estándar agrupados por empresa
logit_cluster <- function(vars, datos) {
  f <- as.formula(paste("y ~", paste(vars, collapse = " + ")))
  m <- glm(f, data = datos, family = binomial())
  X <- model.matrix(m); p <- fitted(m); u <- X * (datos$y - p)
  B <- solve(crossprod(X * sqrt(p * (1 - p))))
  S <- rowsum(u, datos$nit); G <- nrow(S)
  V <- B %*% crossprod(S) %*% B * G / (G - 1)
  list(modelo = m, se = sqrt(diag(V)))
}

cat("E2. Estimando modelos...\n")
coefs <- NULL; desemp <- NULL; calib <- NULL; preds_prueba <- list()
for (k in names(FORMULAS)) {
  r <- logit_cluster(FORMULAS[[k]], ent)
  b <- coef(r$modelo); se <- r$se
  coefs <- rbind(coefs, data.frame(modelo = NOMBRES[[k]], termino = names(b), coef = b, se_cluster = se,
                                   odds_ratio = exp(b), or_ic95_inf = exp(b - 1.96 * se),
                                   or_ic95_sup = exp(b + 1.96 * se), p_valor = 2 * pnorm(-abs(b / se)),
                                   row.names = NULL))
  pe <- fitted(r$modelo)
  pp <- predict(r$modelo, newdata = pru, type = "response"); preds_prueba[[k]] <- pp
  top <- pp >= quantile(pp, .9)
  desemp <- rbind(desemp, data.frame(
    modelo = NOMBRES[[k]], auc_entrenamiento = auc(pe, ent$y), auc_prueba = auc(pp, pru$y),
    brier_prueba = mean((pp - pru$y)^2), brier_ingenuo = mean((mean(ent$y) - pru$y)^2),
    pct_caidas_capturadas_decil_top = 100 * sum(pru$y[top]) / sum(pru$y),
    tasa_caida_decil_top = 100 * mean(pru$y[top]), tasa_caida_promedio = 100 * mean(pru$y),
    pseudo_r2_mcfadden = 1 - r$modelo$deviance / r$modelo$null.deviance,
    aic = AIC(r$modelo), row.names = NULL))
  dcl <- cut(pp, unique(quantile(pp, 0:10 / 10)), include.lowest = TRUE, labels = FALSE)
  calib <- rbind(calib, data.frame(modelo = NOMBRES[[k]], decil = sort(unique(dcl)),
                                   prob_predicha = tapply(pp, dcl, mean), tasa_observada = tapply(pru$y, dcl, mean),
                                   n = as.vector(table(dcl)), row.names = NULL))
}
guardar(coefs, "E2_coeficientes_modelos.csv")
guardar(desemp, "E3_desempeno_fuera_de_muestra.csv")
guardar(calib, "E3_calibracion_por_decil.csv")

# =============================================================================
# E4. ALERTA TEMPRANA 2026 (M1 reestimado con t = 2017-2024)
# =============================================================================
cat("E4. Alerta temprana...\n")
todo <- base[muestra & panel$anio %in% c(ANIOS_ENTRENAMIENTO, ANIOS_PRUEBA), ]
m_final <- logit_cluster(FORMULAS$M1, todo)$modelo
ult <- max(panel$anio)
hoy <- panel$anio == ult & panel$zona %in% c("Gray Zone", "Safe Zone")
alerta <- data.frame(nit = panel$nit[hoy], razon_social = panel$razon_social[hoy],
                     ciiu_seccion = panel$ciiu_seccion[hoy], departamento = panel$departamento[hoy],
                     zona_actual = as.character(panel$zona[hoy]), z_actual = panel$z[hoy],
                     prob_caer_distress_siguiente_anio = predict(m_final, newdata = base[hoy, ], type = "response"))
alerta$nivel_alerta <- cut(alerta$prob_caer_distress_siguiente_anio, c(-Inf, .02, .05, .15, Inf),
                           labels = c("Baja (<2%)", "Moderada (2-5%)", "Alta (5-15%)", "Muy alta (>15%)"))
alerta <- alerta[order(-alerta$prob_caer_distress_siguiente_anio), ]
guardar(alerta, "E4_alerta_temprana_siguiente_anio.csv")

# =============================================================================
# GRÁFICOS
# =============================================================================
cols <- c("#7f8c8d", "#2166ac", "#e67e22", "#1b9e77")
png(file.path(CARPETA, "gE1_curvas_roc_prueba.png"), 900, 850, res = 120)
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "1 - especificidad (falsas alarmas)",
     ylab = "Sensibilidad (caídas detectadas)", main = "Curvas ROC fuera de muestra (t = 2021-2024)")
abline(0, 1, lty = 2, col = "grey70")
for (i in seq_along(preds_prueba)) {
  pp <- preds_prueba[[i]]; o <- order(-pp); y <- pru$y[o]
  lines(c(0, cumsum(1 - y) / sum(1 - y)), c(0, cumsum(y) / sum(y)), col = cols[i], lwd = 2)
}
legend("bottomright", bty = "n", col = cols, lwd = 2,
       legend = sprintf("%s (AUC %.3f)", desemp$modelo, desemp$auc_prueba))
invisible(dev.off())

png(file.path(CARPETA, "gE2_calibracion_M1.png"), 900, 850, res = 120)
cm <- calib[calib$modelo == NOMBRES[["M1"]], ]
lim <- c(0, max(cm$prob_predicha, cm$tasa_observada) * 1.05)
plot(100 * cm$prob_predicha, 100 * cm$tasa_observada, pch = 19, col = cols[2], xlim = 100 * lim, ylim = 100 * lim,
     xlab = "Probabilidad predicha (%)", ylab = "Tasa observada (%)",
     main = "Calibración M1 por decil (fuera de muestra)")
abline(0, 1, lty = 2, col = "grey60")
invisible(dev.off())

png(file.path(CARPETA, "gE3_odds_ratios_M1.png"), 1100, 900, res = 120)
cf <- coefs[coefs$modelo == NOMBRES[["M1"]] & coefs$termino != "(Intercept)" & !grepl("^sector", coefs$termino), ]
cf <- cf[order(cf$odds_ratio), ]
op <- par(mar = c(5, 12, 3, 2))
plot(cf$odds_ratio, seq_len(nrow(cf)), log = "x", pch = 19, yaxt = "n", ylab = "",
     xlim = range(c(cf$or_ic95_inf, cf$or_ic95_sup)), xlab = "Odds ratio (escala log)",
     main = "M1: efecto sobre la probabilidad de caer a Distress")
segments(cf$or_ic95_inf, seq_len(nrow(cf)), cf$or_ic95_sup, seq_len(nrow(cf)))
abline(v = 1, lty = 2, col = "grey60"); axis(2, seq_len(nrow(cf)), cf$termino, las = 1, cex.axis = .85)
par(op); invisible(dev.off())

# =============================================================================
# RESUMEN
# =============================================================================
cat("\n================ RESUMEN MULTIVARIADO ================\n")
cat("E1. Persistencia (Spearman t vs t+1):\n")
print(pers[order(-pers$rho_t_t1), ], row.names = FALSE, digits = 3)
cat("\nE3. Desempeño fuera de muestra:\n")
print(desemp[, c("modelo", "auc_entrenamiento", "auc_prueba", "brier_prueba", "brier_ingenuo",
                 "pct_caidas_capturadas_decil_top", "tasa_caida_decil_top", "tasa_caida_promedio")],
      row.names = FALSE, digits = 3)
cat("\nE2. M1: odds ratios (por 1 desviación estándar; errores agrupados por empresa):\n")
print(coefs[coefs$modelo == NOMBRES[["M1"]], c("termino", "odds_ratio", "or_ic95_inf", "or_ic95_sup", "p_valor")],
      row.names = FALSE, digits = 3)
cat("\nE4. Alerta temprana para", ult + 1, "(empresas en Gray o Safe en", ult, "):\n")
print(table(alerta$nivel_alerta))
cat("\nResultados en:", CARPETA, "\n")
cat("Tiempo:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")
