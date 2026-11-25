# =============================================================================
# Limpieza de la pestaña INDICADORES para análisis de panel en R
# Fuente: BD_INDICADORES_2016_2025_DIC.xlsx (7.772 NIT x 10 años, corte 31-dic)
#
# Qué hace:
#   1. Lee INDICADORES, ESF y ER directamente del Excel (valores ya calculados).
#   2. Valida: filas, alineación de NIT entre hojas, llave NIT-año única,
#      panel balanceado (10 años por NIT).
#   3. Recalcula cada ratio desde ESF y ER y lo compara con el valor del Excel.
#   4. Renombra columnas, separa CIIU en código y descripción, fija tipos.
#   5. #DIV/0! del Excel pasa a NA.
#   6. Opcional: ceros artificiales (IFERROR(...,0) con denominador cero) pasan a NA.
#   7. Exporta indicadores_panel.csv, indicadores_panel.rds y diccionario_variables.csv
#
# Requisitos: R >= 4.0 y el paquete readxl  ->  install.packages("readxl")
# Uso: cambie RUTA_EXCEL y ejecute todo el script (Ctrl+Shift+Enter en RStudio).
# =============================================================================

# ---- CONFIGURACIÓN ----------------------------------------------------------
RUTA_EXCEL <- "BD_INDICADORES_2016_2025_DIC.xlsx"
# Use "/" o "\\" en la ruta, nunca "\" sola.

# TRUE: un ratio cuyo denominador es 0 (el Excel muestra 0 por el IFERROR) queda NA.
# FALSE: se conserva el 0 tal como sale del Excel.
CEROS_ARTIFICIALES_A_NA <- TRUE

# TRUE: si algún X1..X5 es NA, Z y zona quedan NA (el Z no está definido).
# FALSE: Z y zona se conservan como los calculó el Excel.
Z_NA_SI_FALTA_ALGUN_X <- TRUE

# Carpeta de salida: la misma del Excel
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) RUTA_EXCEL <- args[1]
CARPETA_SALIDA <- "resultados"   # todas las salidas de R quedan dentro de analisis/resultados
if (length(args) >= 2) CARPETA_SALIDA <- args[2]

suppressPackageStartupMessages(library(readxl))
stopifnot(file.exists(RUTA_EXCEL))
t0 <- Sys.time()

# ---- 1. LECTURA -------------------------------------------------------------
# Convierte letra de columna de Excel a número (A=1, Z=26, AA=27, ...)
col_num <- function(l) {
  n <- 0
  for (ch in strsplit(l, "")[[1]]) n <- n * 26 + (utf8ToInt(ch) - 64)
  n
}

cat("Leyendo INDICADORES...\n")
# A NIT, B Periodo, C razon social, D departamento, E CIIU, F:U 16 ratios,
# V columna vacía (se omite), W:AA X1..X5, AB Z, AC Zone
tipos_ind <- c("numeric", "numeric", "text", "text", "text",
               rep("numeric", 16), "skip", rep("numeric", 6), "text")
ind <- read_excel(RUTA_EXCEL, sheet = "INDICADORES", col_types = tipos_ind,
                  .name_repair = "minimal", progress = FALSE)

cat("Leyendo ESF...\n")
# A:K texto (C es NIT numérico), L:BT cifras (61 columnas)
tipos_esf <- c("text", "text", "numeric", rep("text", 8), rep("numeric", 61))
esf <- read_excel(RUTA_EXCEL, sheet = "ESF", col_types = tipos_esf,
                  .name_repair = "minimal", progress = FALSE)

cat("Leyendo ER...\n")
# A:K texto (C es NIT numérico), L:AA cifras (16 columnas)
tipos_er <- c("text", "text", "numeric", rep("text", 8), rep("numeric", 16))
er <- read_excel(RUTA_EXCEL, sheet = "ER", col_types = tipos_er,
                 .name_repair = "minimal", progress = FALSE)

# Acceso por letra de columna; celda vacía = 0, igual que en Excel
E <- function(l) { v <- esf[[col_num(l)]]; v[is.na(v)] <- 0; v }
R <- function(l) { v <- er[[col_num(l)]];  v[is.na(v)] <- 0; v }

# ---- 2. VALIDACIONES DE ESTRUCTURA -------------------------------------------
cat("Validando estructura...\n")
n <- nrow(ind)
stopifnot(nrow(esf) == n, nrow(er) == n)
stopifnot(ncol(ind) == 28)                               # 29 columnas menos V
stopifnot(all(ind[[1]] == esf[[3]]), all(ind[[1]] == er[[3]]))  # misma empresa por fila
stopifnot(all(substr(esf[[4]], 6, 10) == "12-31"))       # solo cortes anuales
stopifnot(!anyDuplicated(paste(ind[[1]], ind[[2]])))     # llave NIT-año única
anios_por_nit <- table(ind[[1]])
stopifnot(all(anios_por_nit == length(unique(ind[[2]]))))  # panel balanceado

# ---- 3. RECÁLCULO DE RATIOS DESDE ESF Y ER ------------------------------------
# Fórmulas literales de la plantilla (incluye sus particularidades, ver diccionario)
deuda_fin <- E("AS") + E("AT") + E("AU") + E("BF") + E("BG")
num_den <- list(
  cr = list(E("W"),                         E("AY")),
  at = list(E("W") - E("N"),                E("AY")),
  wk = list(E("W") / E("AY"),               E("AM")),
  ti = list(E("BJ"),                        E("AM")),
  fi = list(deuda_fin,                      R("L")),
  tl = list(E("BJ"),                        E("BS")),
  fl = list(deuda_fin,                      E("BS")),
  gm = list(R("N"),                         R("L")),
  om = list(R("N"),                         R("T")),
  nm = list(R("AA"),                        R("L")),
  re = list(R("AA"),                        E("BS")),
  ra = list(R("AA"),                        E("AM")),
  ar = list((E("AE") + E("M")) * 365,       R("L")),
  it = list((E("N") + E("AF")) * 365,       R("M")),
  st = list(E("AQ") + E("BC"),              R("M")),
  ct = list(E("L"),                         E("AY")),
  x1 = list(E("W") - E("AY"),               E("AM")),
  x2 = list(E("BR"),                        E("AM")),
  x3 = list(R("T"),                         E("AM")),
  x4 = list(E("BS"),                        E("BJ")),
  x5 = list(R("L"),                         E("AM"))
)
# WK tiene dos divisiones: falla si AY o AM es 0
den_cero <- lapply(num_den, function(p) p[[2]] == 0 | !is.finite(p[[1]]))
recalc   <- lapply(num_den, function(p) ifelse(p[[2]] == 0 | !is.finite(p[[1]]), NA, p[[1]] / p[[2]]))

# ---- 4. BASE LIMPIA -----------------------------------------------------------
ratios <- c("cr", "at", "wk", "ti", "fi", "tl", "fl", "gm", "om", "nm",
            "re", "ra", "ar", "it", "st", "ct")
xs <- c("x1", "x2", "x3", "x4", "x5")
names(ind) <- c("nit", "anio", "razon_social", "departamento", "ciiu",
                ratios, xs, "z", "zona")

# CIIU "L6810 - Actividades ..." -> código y descripción
patron <- "^([A-Z][0-9]{4}) - (.*)$"
ok_ciiu <- grepl(patron, ind$ciiu)
ciiu_cod  <- ifelse(ok_ciiu, sub(patron, "\\1", ind$ciiu), NA_character_)
ciiu_desc <- ifelse(ok_ciiu, sub(patron, "\\2", ind$ciiu), ind$ciiu)

panel <- data.frame(
  nit          = as.integer(ind$nit),
  anio         = as.integer(ind$anio),
  razon_social = ind$razon_social,
  departamento = ind$departamento,
  ciiu_seccion = substr(ciiu_cod, 1, 1),
  ciiu_cod     = ciiu_cod,
  ciiu_desc    = ciiu_desc,
  stringsAsFactors = FALSE
)
for (v in c(ratios, xs, "z")) panel[[v]] <- ind[[v]]
panel$zona <- ind$zona

# Errores de Excel (#DIV/0!) ya llegan como NA. En zona puede llegar el texto del error.
panel$zona[!panel$zona %in% c("Distress Zone", "Gray Zone", "Safe Zone")] <- NA

# Validación: el ratio del Excel coincide con el recalculado (donde el denominador no es 0)
cat("Comparando ratios del Excel contra el recálculo...\n")
max_dif <- sapply(c(ratios, xs), function(v) {
  ok <- !den_cero[[v]] & !is.na(panel[[v]])
  d <- abs(panel[[v]][ok] - recalc[[v]][ok]) / pmax(1, abs(recalc[[v]][ok]))
  if (length(d)) max(d) else 0
})
stopifnot(all(max_dif < 1e-9))
# Donde el denominador es 0, el Excel debe mostrar 0 (IFERROR) o NA (X1, sin IFERROR)
stopifnot(all(sapply(c(ratios, xs), function(v) {
  x <- panel[[v]][den_cero[[v]]]; all(is.na(x) | x == 0)
})))

# Ceros artificiales -> NA
n_div0 <- Reduce(`+`, lapply(c(ratios, xs), function(v) as.integer(den_cero[[v]])))
if (CEROS_ARTIFICIALES_A_NA) {
  for (v in c(ratios, xs)) panel[[v]][den_cero[[v]]] <- NA
}
panel$n_div_cero <- n_div0   # cuántos ratios de la fila tenían denominador 0

# Z con algún X faltante
pesos <- c(x1 = 0.717, x2 = 0.847, x3 = 3.107, x4 = 0.42, x5 = 0.998)
z_ok <- complete.cases(panel[, xs])
z_chk <- as.matrix(panel[z_ok, xs]) %*% pesos
stopifnot(max(abs(z_chk - panel$z[z_ok]) / pmax(1, abs(panel$z[z_ok]))) < 1e-9)
n_z_sin_x <- sum(!z_ok & !is.na(panel$z))   # Z del Excel calculado con algún X en 0 artificial
if (Z_NA_SI_FALTA_ALGUN_X) {
  panel$z[!z_ok] <- NA
  panel$zona[!z_ok] <- NA
}
panel$zona <- factor(panel$zona, levels = c("Distress Zone", "Gray Zone", "Safe Zone"))

panel <- panel[order(panel$nit, panel$anio), ]
rownames(panel) <- NULL

# ---- 5. DICCIONARIO -----------------------------------------------------------
dic <- data.frame(
  variable = c("nit", "anio", "razon_social", "departamento", "ciiu_seccion",
               "ciiu_cod", "ciiu_desc", ratios, xs, "z", "zona", "n_div_cero"),
  descripcion = c(
    "NIT de la sociedad", "Año del corte (31 de diciembre)", "Razón social",
    "Departamento", "Sección CIIU (letra)", "Código CIIU", "Descripción CIIU",
    # nombres según la hoja CONVENCIONES de la plantilla
    "Razón corriente (Current Ratio)", "Prueba ácida (Acid Test)",
    "Capital de trabajo (Working Capital Ratio)", "Endeudamiento total (Total Indebtedness)",
    "Endeudamiento financiero (Financial Indebtedness)", "Apalancamiento total (Total Leverage)",
    "Apalancamiento financiero (Financial Leverage)", "Margen bruto (Gross Margin)",
    "Margen operacional (Operating Margin)", "Margen neto (Net Margin)",
    "Retorno sobre patrimonio (Return on Equity)", "Retorno sobre activos (Return on Assets)",
    "Rotación de cartera, días (Accounts Receivable turnover)",
    "Rotación de inventario, días (Inventory turns)", "Rotación proveedores (Supplier turnover)",
    "Rotación de efectivo (Cash Ratio)",
    "X1 Altman Z'", "X2 Altman Z'", "X3 Altman Z'", "X4 Altman Z'", "X5 Altman Z'",
    "Z' de Altman (empresas privadas)", "Zona Z'",
    "Número de ratios de la fila con denominador cero"),
  formula_excel = c(rep("", 7),
    "ESF!W / ESF!AY", "(ESF!W - ESF!N) / ESF!AY", "(ESF!W / ESF!AY) / ESF!AM",
    "ESF!BJ / ESF!AM", "(ESF!AS+AT+AU+BF+BG) / ER!L", "ESF!BJ / ESF!BS",
    "(ESF!AS+AT+AU+BF+BG) / ESF!BS", "ER!N / ER!L", "ER!N / ER!T", "ER!AA / ER!L",
    "ER!AA / ESF!BS", "ER!AA / ESF!AM", "(ESF!AE + ESF!M) * 365 / ER!L",
    "(ESF!N + ESF!AF) * 365 / ER!M", "(ESF!AQ + ESF!BC) / ER!M", "ESF!L / ESF!AY",
    "(ESF!W - ESF!AY) / ESF!AM", "ESF!BR / ESF!AM", "ER!T / ESF!AM",
    "ESF!BS / ESF!BJ", "ER!L / ESF!AM",
    "0.717*X1 + 0.847*X2 + 3.107*X3 + 0.42*X4 + 0.998*X5",
    "Z < 1.23 Distress; Z < 2.9 Gray; resto Safe", ""),
  nota = c(rep("", 7),
    "", "", "Fórmula = (AC/PC)/AT; CONVENCIONES dice (AC - PC)/AT",
    "", "", "", "",
    "", "Fórmula = utilidad bruta / utilidad operacional; CONVENCIONES dice UO / ingresos",
    "", "Fórmula = UN / patrimonio (ROE); el texto de CONVENCIONES está intercambiado con RA",
    "Fórmula = UN / activo (ROA); el texto de CONVENCIONES está intercambiado con RE",
    "", "", "Fórmula no multiplica por 365; CONVENCIONES dice *365", "",
    "Sin IFERROR en el Excel: activo total 0 da #DIV/0!", "", "", "", "", "", "", ""),
  stringsAsFactors = FALSE
)

# ---- 6. EXPORTAR --------------------------------------------------------------
dir.create(CARPETA_SALIDA, showWarnings = FALSE, recursive = TRUE)
f_csv <- file.path(CARPETA_SALIDA, "indicadores_panel.csv")
f_rds <- file.path(CARPETA_SALIDA, "indicadores_panel.rds")
f_dic <- file.path(CARPETA_SALIDA, "diccionario_variables.csv")
write.csv(panel, f_csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")
saveRDS(panel, f_rds)
write.csv(dic, f_dic, row.names = FALSE, fileEncoding = "UTF-8")

# ---- 7. RESUMEN ---------------------------------------------------------------
cat("\n================ RESUMEN ================\n")
cat("Filas:", nrow(panel), "| NIT:", length(unique(panel$nit)),
    "| Años:", paste(range(panel$anio), collapse = "-"), "\n")
cat("Filas por año:\n"); print(table(panel$anio))
cat("CIIU sin formato estándar:", sum(!ok_ciiu), "\n")
cat("NA por variable:\n")
print(colSums(is.na(panel[, c(ratios, xs, "z", "zona")])))
cat("Zonas:\n"); print(table(panel$zona, useNA = "ifany"))
cat("Filas con algún X con denominador 0 (Z del Excel usaba 0):", n_z_sin_x,
    if (Z_NA_SI_FALTA_ALGUN_X) "-> Z y zona en NA\n" else "-> Z y zona conservados\n")
cat("Archivos:\n ", f_csv, "\n ", f_rds, "\n ", f_dic, "\n")
cat("Tiempo:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")

# Para análisis de panel (opcional):
# install.packages("plm"); library(plm)
# pdat <- pdata.frame(panel, index = c("nit", "anio"))
