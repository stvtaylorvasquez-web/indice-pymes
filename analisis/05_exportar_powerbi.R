# =============================================================================
# 05. Exportar tablas para Power BI
# Junta en resultados/powerbi/ todas las tablas del análisis, listas para cargar:
#   - fact_panel.csv     una fila por empresa y año (ratios, zona, cuadrante, PCA)
#   - dim_empresa.csv    una fila por empresa (para relacionar todas las tablas)
#   - dim_sector.csv     nombre de cada sección CIIU
#   - el resto de tablas de los scripts 02, 03 y 04 con nombres ordenados
# Además escribe powerbi_codigo_M.txt: el código para pegar en Power BI, con la
# lista exacta de columnas de texto, y muestra la ruta para el parámetro.
# Requiere haber corrido 01 a 04. Solo usa R base.
# =============================================================================

CARPETA_RES <- "resultados"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) CARPETA_RES <- args[1]
SALIDA <- file.path(CARPETA_RES, "powerbi")
dir.create(SALIDA, showWarnings = FALSE)
unlink(list.files(SALIDA, full.names = TRUE))     # parte de una carpeta limpia

leer <- function(ruta) read.csv(ruta, fileEncoding = "UTF-8", check.names = FALSE,
                                stringsAsFactors = FALSE, na.strings = "")
tablas <- list()

# ---- Tabla de hechos: panel empresa-año -------------------------------------------
panel <- readRDS(file.path(CARPETA_RES, "panel_analitico.rds"))
cuad <- leer(file.path(CARPETA_RES, "bivariado", "panel_cuadrantes.csv"))
panel <- merge(panel, cuad, by = c("nit", "anio"), all.x = TRUE, sort = FALSE)
panel$zona <- as.character(panel$zona)
panel <- panel[order(panel$nit, panel$anio), ]
tablas$fact_panel <- panel

# ---- Dimensión empresa: datos del último año reportado --------------------------
ult <- panel[panel$anio == ave(panel$anio, panel$nit, FUN = max), ]
tablas$dim_empresa <- ult[, c("nit", "razon_social", "departamento", "ciiu_seccion",
                              "ciiu_cod", "ciiu_desc")]

# ---- Dimensión sector (secciones CIIU Rev. 4 A.C.) -------------------------------
tablas$dim_sector <- data.frame(
  ciiu_seccion = LETTERS[1:21],
  sector = c("Agropecuario", "Minería", "Manufactura", "Energía y gas", "Agua y saneamiento",
             "Construcción", "Comercio", "Transporte", "Alojamiento y comida",
             "Información y comunicaciones", "Financiero y seguros", "Inmobiliario",
             "Profesionales y técnicas", "Servicios administrativos", "Administración pública",
             "Educación", "Salud", "Arte y recreación", "Otros servicios", "Hogares",
             "Organismos extraterritoriales"),
  stringsAsFactors = FALSE)

# ---- Tablas de resultados de los scripts 02 a 04 ---------------------------------
origen <- c(
  u01_diagnostico_extremos   = "01_diagnostico_extremos.csv",
  u02_winsorizacion          = "02_antes_despues_winsorizacion.csv",
  u03_descriptiva            = "03_descriptiva.csv",
  u04_tendencias             = "04_tendencias.csv",
  u05_zonas_por_anio         = "05_zonas_por_anio.csv",
  u06_transicion_total       = "06_transicion_zonas_total.csv",
  u07_transicion_por_anio    = "07_transicion_zonas_por_anio.csv",
  u08_prob_caer_distress     = "08_prob_caer_a_distress.csv",
  u09_rachas_distress        = "09_rachas_distress.csv",
  u10_pca_varianza           = "10_pca_varianza.csv",
  u11_pca_cargas             = "11_pca_cargas.csv",
  b_a1_spearman              = "bivariado/A1_spearman_total_y_por_anio.csv",
  b_a2_estabilidad           = "bivariado/A2_estabilidad_por_anio.csv",
  b_a3_intra_empresa         = "bivariado/A3_correlacion_intra_empresa.csv",
  b_b1_cuadrantes_por_anio   = "bivariado/B1_cuadrantes_por_anio.csv",
  b_b2_cuadrantes_riesgo     = "bivariado/B2_cuadrantes_riesgo.csv",
  b_b3_migracion_cuadrantes  = "bivariado/B3_migracion_cuadrantes.csv",
  b_c1_decil_endeudamiento   = "bivariado/C1_rentabilidad_por_decil_endeudamiento.csv",
  b_c2_efectos_fijos         = "bivariado/C2_efectos_fijos_rentabilidad_vs_deuda.csv",
  b_c3_dinamica_deuda        = "bivariado/C3_dinamica_deuda.csv",
  b_d1_indicadores_vs_z      = "bivariado/D1_indicadores_vs_z.csv",
  b_d2_contribucion_x        = "bivariado/D2_contribucion_x_por_zona.csv",
  b_d3_caidas_distress       = "bivariado/D3_que_x_explica_caidas_a_distress.csv",
  b_d4_zonas_sector          = "bivariado/D4_zonas_por_sector.csv",
  m_e1_persistencia          = "multivariado/E1_persistencia.csv",
  m_e2_coeficientes          = "multivariado/E2_coeficientes_modelos.csv",
  m_e3_desempeno             = "multivariado/E3_desempeno_fuera_de_muestra.csv",
  m_e3_calibracion           = "multivariado/E3_calibracion_por_decil.csv",
  m_e4_alerta_temprana       = "multivariado/E4_alerta_temprana_siguiente_anio.csv")
for (k in names(origen)) {
  f <- file.path(CARPETA_RES, origen[[k]])
  if (!file.exists(f)) stop("Falta ", f, ". Corra primero los scripts 01 a 04.")
  tablas[[k]] <- leer(f)
}

# ---- Tipos: lógicos a 0/1, factores a texto ---------------------------------------
for (k in names(tablas)) {
  for (v in names(tablas[[k]])) {
    x <- tablas[[k]][[v]]
    if (is.logical(x)) tablas[[k]][[v]] <- as.integer(x)
    if (is.factor(x))  tablas[[k]][[v]] <- as.character(x)
  }
}

# Una columna con el mismo nombre debe ser del mismo tipo en todas las tablas
tipo <- do.call(rbind, lapply(names(tablas), function(k)
  data.frame(tabla = k, columna = names(tablas[[k]]),
             texto = sapply(tablas[[k]], is.character), stringsAsFactors = FALSE)))
conflicto <- tapply(tipo$texto, tipo$columna, function(x) length(unique(x)) > 1)
if (any(conflicto)) stop("Columnas con tipo distinto entre tablas: ",
                         paste(names(conflicto)[conflicto], collapse = ", "))
cols_texto <- sort(unique(tipo$columna[tipo$texto]))

# ---- Escribir CSV (UTF-8, punto decimal) -------------------------------------------
for (k in names(tablas)) {
  write.csv(tablas[[k]], file.path(SALIDA, paste0(k, ".csv")), row.names = FALSE,
            na = "", fileEncoding = "UTF-8")
}

# ---- Código M para Power BI --------------------------------------------------------
m <- c(
  "// Función LeerCSV: pegar en Power BI como consulta en blanco y renombrarla LeerCSV",
  "// Requiere el parámetro CarpetaProyecto (ruta de la carpeta del proyecto).",
  "(archivo as text) as table =>",
  "let",
  "    Ruta = CarpetaProyecto & \"\\resultados\\powerbi\\\" & archivo,",
  "    Origen = Csv.Document(File.Contents(Ruta), [Delimiter = \",\", Encoding = 65001, QuoteStyle = QuoteStyle.Csv]),",
  "    Encabezados = Table.PromoteHeaders(Origen, [PromoteAllScalars = true]),",
  paste0("    ColumnasTexto = {", paste0("\"", cols_texto, "\"", collapse = ", "), "},"),
  "    Tipos = List.Transform(Table.ColumnNames(Encabezados), each {_, if List.Contains(ColumnasTexto, _) then type text else type number}),",
  "    Resultado = Table.TransformColumnTypes(Encabezados, Tipos, \"en-US\")",
  "in",
  "    Resultado",
  "",
  "// Luego, una consulta en blanco por tabla, con el nombre de la tabla y este contenido:",
  paste0("//   = LeerCSV(\"", names(tablas), ".csv\")"))
writeLines(m, file.path(SALIDA, "powerbi_codigo_M.txt"), useBytes = TRUE)

ruta_proyecto <- normalizePath(".", winslash = "\\")
cat("\n================ POWER BI ================\n")
cat("Tablas exportadas:", length(tablas), "en", SALIDA, "\n")
cat("Filas de fact_panel:", nrow(tablas$fact_panel), "| empresas en dim_empresa:",
    nrow(tablas$dim_empresa), "\n")
cat("Columnas de texto:", length(cols_texto), "\n")
cat("\nValor para el parámetro CarpetaProyecto en Power BI:\n  ", ruta_proyecto, "\n")
