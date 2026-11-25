# =============================================================================
# 05. Exportar datos para el tablero web (carpeta ../sitio/datos)
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
# Requiere haber corrido 01 a 04. Solo usa R base.
# =============================================================================

# TRUE: el tablero muestra la tabla de alerta 2026 con el nombre de cada empresa.
# FALSE: solo se publican resultados agregados (sector, departamento, nivel).
PUBLICAR_ALERTA_POR_EMPRESA <- TRUE

CARPETA_RES <- "resultados"
CARPETA_WEB <- file.path("..", "sitio", "datos")
RUTA_EXCEL  <- "BD_INDICADORES_2016_2025_DIC.xlsx"
TASA_IMPUESTO <- 0.35   # tarifa de renta para la utilidad operativa después de impuestos (inductores de valor)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) CARPETA_RES <- args[1]
if (length(args) >= 2) CARPETA_WEB <- args[2]
if (length(args) >= 3) RUTA_EXCEL  <- args[3]
dir.create(CARPETA_WEB, showWarnings = FALSE, recursive = TRUE)
escribir <- function(df, nombre) write.csv(df, file.path(CARPETA_WEB, nombre), row.names = FALSE,
                                           na = "", fileEncoding = "UTF-8")
leer <- function(ruta) read.csv(file.path(CARPETA_RES, ruta), fileEncoding = "UTF-8",
                                check.names = FALSE, stringsAsFactors = FALSE, na.strings = "")

panel <- readRDS(file.path(CARPETA_RES, "panel_analitico.rds"))
cuad  <- leer(file.path("bivariado", "panel_cuadrantes.csv"))
panel <- merge(panel, cuad, by = c("nit", "anio"), all.x = TRUE, sort = FALSE)
panel <- panel[order(panel$nit, panel$anio), ]

# ---- Cuentas del Excel para los inductores de valor (mismo método que el script 04) ----
suppressPackageStartupMessages(library(readxl))
cat("Leyendo cuentas del Excel para los inductores de valor...\n")
esf <- read_excel(RUTA_EXCEL, sheet = "ESF", progress = FALSE, .name_repair = "minimal",
                  col_types = c("text", "text", "numeric", rep("text", 8), rep("numeric", 61)))
er  <- read_excel(RUTA_EXCEL, sheet = "ER", progress = FALSE, .name_repair = "minimal",
                  col_types = c("text", "text", "numeric", rep("text", 8), rep("numeric", 16)))
cero <- function(x) ifelse(is.na(x), 0, x)       # celda vacía = 0, como en las fórmulas del Excel
niv <- data.frame(nit = as.integer(esf[[3]]), anio = as.integer(substr(esf[[4]], 1, 4)),
                  cxc = cero(esf[[13]]),          # ESF!M  cuentas por cobrar corrientes
                  inv = cero(esf[[14]]),          # ESF!N  inventarios corrientes
                  ppe = cero(esf[[24]]),          # ESF!X  propiedades, planta y equipo
                  prov = cero(esf[[43]]),         # ESF!AQ cuentas por pagar comerciales
                  ing = cero(er[[12]]))           # ER!L   ingresos de actividades ordinarias
stopifnot(all(esf[[3]] == er[[3]]), !anyDuplicated(niv[, c("nit", "anio")]))
rm(esf, er); invisible(gc())
panel <- merge(panel, niv, by = c("nit", "anio"), all.x = TRUE, sort = FALSE)
panel <- panel[order(panel$nit, panel$anio), ]
ing_ant <- ave(panel$ing, panel$nit, FUN = function(v) c(NA, head(v, -1)))
panel$pkt  <- ifelse(panel$ing > 0, (panel$cxc + panel$inv - panel$prov) / panel$ing, NA)  # productividad del capital de trabajo
panel$paf  <- ifelse(panel$ing > 0 & panel$ppe > 0, panel$ing / panel$ppe, NA)             # productividad del activo fijo
panel$crec <- ifelse(panel$ing > 0 & !is.na(ing_ant) & ing_ant > 0, panel$ing / ing_ant - 1, NA)  # crecimiento de ingresos

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
  x1   = round(panel$x1, 4),        # componentes del Z' (pestaña "¿Por qué caen?")
  x2   = round(panel$x2, 4),
  x3   = round(panel$x3, 4),
  x4   = round(panel$x4, 4),
  x5   = round(panel$x5, 4),
  pkt  = round(panel$pkt, 4),       # inductores de valor: KTNO / ingresos
  paf  = round(panel$paf, 3),       # ingresos / propiedades, planta y equipo
  crec = round(panel$crec, 4),      # crecimiento anual de ingresos
  ing  = signif(panel$ing, 4),      # ingresos (solo para ordenar por tamaño en la ficha)
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

# ---- tendencias.csv (script 02) e indicadores_vs_z.csv (script 03, D1) ------------
tend <- leer("04_tendencias.csv")
escribir(data.frame(variable = tend$variable, grupo = tend$grupo,
                    mediana_2016 = signif(tend$mediana_2016, 5), mediana_final = signif(tend$mediana_final, 5),
                    pct_empresas_al_alza = round(tend$pct_empresas_al_alza, 1),
                    significativa = as.integer(tend$significativa_1pct)), "tendencias.csv")
d1 <- leer(file.path("bivariado", "D1_indicadores_vs_z.csv"))
d1$es_componente_z <- as.integer(d1$es_componente_z)
d1[c("rho_con_z", "rho_intra_empresa")] <- lapply(d1[c("rho_con_z", "rho_intra_empresa")], round, 4)
escribir(d1, "indicadores_vs_z.csv")

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
    "Decil de endeudamiento dentro del año (1 = menos endeudadas)",
    "X1 Capital de trabajo / activo total", "X2 Utilidades acumuladas / activo total",
    "X3 Utilidad operacional / activo total", "X4 Patrimonio / pasivo total", "X5 Ventas / activo total",
    "PKT: (cuentas por cobrar + inventarios - proveedores) / ingresos", "Ingresos / propiedades, planta y equipo",
    "Crecimiento anual de los ingresos", "Ingresos de actividades ordinarias (unidades de la base original)"),
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
