# =============================================================================
# 06. Exportar datos para el tablero web (carpeta ../sitio/datos)
# Escribe archivos livianos que lee el sitio publicado en GitHub Pages:
#   - panel.csv        una fila por empresa y año, solo las columnas que usa el tablero
#   - empresas.csv     una fila por empresa (nombre, sector, departamento)
#   - sectores.csv     nombre corto de cada sección CIIU
#   - alerta.csv       probabilidad de caer a Distress el año siguiente (script 04)
#   - modelos.csv      desempeño fuera de muestra de los modelos (script 04)
#   - descriptiva.csv  estadística descriptiva (script 02)
#   - diccionario.csv  qué significa cada columna de panel.csv
#   - meta.json        fecha de actualización, corte y totales (se muestran en el sitio)
# departamentos.geojson (mapa DANE MGN 2018) es fijo: este script NO lo toca.
# Cada ejecución SOBRESCRIBE los demás archivos. Para publicar: subir la carpeta
# sitio/datos a GitHub y el sitio se reconstruye solo.
# Requiere haber corrido 01 a 05. Solo usa R base.
# =============================================================================

# TRUE: el tablero muestra la tabla de alerta 2026 con el nombre de cada empresa.
# FALSE: solo se publican resultados agregados (sector, departamento, nivel).
PUBLICAR_ALERTA_POR_EMPRESA <- TRUE

CARPETA_RES <- "resultados"
CARPETA_WEB <- file.path("..", "sitio", "datos")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) CARPETA_RES <- args[1]
if (length(args) >= 2) CARPETA_WEB <- args[2]
dir.create(CARPETA_WEB, showWarnings = FALSE, recursive = TRUE)
escribir <- function(df, nombre) write.csv(df, file.path(CARPETA_WEB, nombre), row.names = FALSE,
                                           na = "", fileEncoding = "UTF-8")
leer <- function(ruta) read.csv(file.path(CARPETA_RES, ruta), fileEncoding = "UTF-8",
                                check.names = FALSE, stringsAsFactors = FALSE, na.strings = "")

panel <- readRDS(file.path(CARPETA_RES, "panel_analitico.rds"))
cuad  <- leer(file.path("bivariado", "panel_cuadrantes.csv"))
panel <- merge(panel, cuad, by = c("nit", "anio"), all.x = TRUE, sort = FALSE)
panel <- panel[order(panel$nit, panel$anio), ]

# ---- panel.csv: columnas mínimas y redondeadas (archivo pequeño = sitio rápido) ----
zona_corta <- c("Distress Zone" = "D", "Gray Zone" = "G", "Safe Zone" = "S")
web <- data.frame(
  nit  = panel$nit,
  anio = panel$anio,
  sec  = panel$ciiu_seccion,
  dep  = panel$departamento,
  zona = unname(zona_corta[as.character(panel$zona)]),
  z    = round(panel$z, 3),
  cr   = round(panel$cr, 4),        # razón corriente
  ti   = round(panel$ti, 4),        # endeudamiento total
  om   = round(panel$om_c, 4),      # margen operacional (corregido, script 02)
  nm   = round(panel$nm, 4),        # margen neto
  ra   = round(panel$ra, 4),        # ROA
  re   = round(panel$re, 4),        # ROE
  ar   = round(panel$ar, 1),        # días de cartera
  pn   = panel$patrimonio_negativo, # 1 = patrimonio negativo
  cuad = as.integer(substr(panel$cuadrante, 1, 1)),  # 1 Crítico ... 4 Holgada (script 03)
  dec  = panel$decil_ti,            # decil de endeudamiento dentro del año (script 03)
  stringsAsFactors = FALSE)
escribir(web, "panel.csv")

# ---- empresas.csv: datos del último año reportado ----------------------------------
ult <- panel[panel$anio == ave(panel$anio, panel$nit, FUN = max), ]
escribir(ult[, c("nit", "razon_social", "departamento", "ciiu_seccion", "ciiu_cod", "ciiu_desc")],
         "empresas.csv")

# ---- sectores.csv ----------------------------------------------------------------
sectores <- data.frame(
  sec = LETTERS[1:21],
  sector = c("Agropecuario", "Minería", "Manufactura", "Energía y gas", "Agua y saneamiento",
             "Construcción", "Comercio", "Transporte", "Alojamiento y comida",
             "Información y comunicaciones", "Financiero y seguros", "Inmobiliario",
             "Profesionales y técnicas", "Servicios administrativos", "Administración pública",
             "Educación", "Salud", "Arte y recreación", "Otros servicios", "Hogares",
             "Organismos extraterritoriales"),
  stringsAsFactors = FALSE)
escribir(sectores, "sectores.csv")

# ---- alerta.csv (script 04, E4) -----------------------------------------------------
al <- leer(file.path("multivariado", "E4_alerta_temprana_siguiente_anio.csv"))
alerta <- data.frame(
  nit = al$nit, razon_social = al$razon_social, sec = al$ciiu_seccion, dep = al$departamento,
  zona = unname(zona_corta[al$zona_actual]), z = round(al$z_actual, 3),
  prob = round(al$prob_caer_distress_siguiente_anio, 5),
  nivel = as.integer(factor(al$nivel_alerta, levels = c("Baja (<2%)", "Moderada (2-5%)",
                                                         "Alta (5-15%)", "Muy alta (>15%)"))),
  stringsAsFactors = FALSE)
if (!PUBLICAR_ALERTA_POR_EMPRESA) { alerta$nit <- NA; alerta$razon_social <- NA }
escribir(alerta[order(-alerta$prob), ], "alerta.csv")

# ---- modelos.csv y descriptiva.csv ----------------------------------------------------
escribir(leer(file.path("multivariado", "E3_desempeno_fuera_de_muestra.csv")), "modelos.csv")
desc <- leer("03_descriptiva.csv")
num <- sapply(desc, is.numeric)
desc[num] <- lapply(desc[num], function(x) signif(x, 5))
escribir(desc, "descriptiva.csv")

# ---- diccionario.csv ------------------------------------------------------------------
escribir(data.frame(
  columna = names(web),
  descripcion = c("NIT de la sociedad", "Año de corte (31 de diciembre)", "Sección CIIU (letra)",
    "Departamento del domicilio", "Zona del Z' de Altman: D Distress (<1,23), G Gris, S Segura (>=2,9)",
    "Z' de Altman", "Razón corriente: activo corriente / pasivo corriente",
    "Endeudamiento total: pasivo total / activo total",
    "Margen operacional: utilidad operacional / ingresos ordinarios",
    "Margen neto: utilidad neta / ingresos ordinarios", "ROA: utilidad neta / activo total",
    "ROE: utilidad neta / patrimonio", "Días de cartera: cuentas por cobrar x 365 / ingresos",
    "1 si el patrimonio es negativo",
    "Cuadrante: 1 Crítico, 2 Tensión de liquidez, 3 Apalancada con liquidez, 4 Holgada (razón corriente < 1; endeudamiento > 0,7)",
    "Decil de endeudamiento dentro del año (1 = menos endeudadas)"),
  stringsAsFactors = FALSE), "diccionario.csv")

# ---- meta.json -------------------------------------------------------------------
meta <- sprintf(paste0('{\n  "actualizado": "%s",\n  "anio_min": %d,\n  "anio_max": %d,\n',
                       '  "empresas": %d,\n  "filas": %d,\n  "alerta_por_empresa": %s,\n  "fuente": "%s"\n}\n'),
                format(Sys.Date(), "%Y-%m-%d"), min(web$anio), max(web$anio),
                length(unique(web$nit)), nrow(web), tolower(PUBLICAR_ALERTA_POR_EMPRESA),
                "Superintendencia de Sociedades - reportes NIIF Pymes individuales, corte 31-dic")
writeLines(meta, file.path(CARPETA_WEB, "meta.json"), useBytes = TRUE)

tam <- file.size(file.path(CARPETA_WEB, "panel.csv")) / 1024^2
cat("\n================ SITIO WEB ================\n")
cat("Archivos escritos en", normalizePath(CARPETA_WEB), "\n")
cat("panel.csv:", nrow(web), "filas,", round(tam, 1), "MB |", length(unique(web$nit)), "empresas\n")
cat("alerta.csv:", nrow(alerta), "empresas evaluadas\n")
cat("Siguiente paso: subir la carpeta sitio/datos a GitHub.\n")
