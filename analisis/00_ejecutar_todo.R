# Ejecuta todo el análisis en orden. Abrir primero el archivo .Rproj.
# Entrada: el Excel en esta carpeta. Salidas: analisis/resultados (intermedios) y ../sitio/datos (tablero web).
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
scripts <- c("01_limpieza_indicadores.R", "02_extremos_univariado_pca.R",
             "03_bivariado.R", "04_multivariado.R", "05_exportar_web.R")
for (s in scripts) {
  cat("\n>>> Ejecutando", s, "\n")
  source(s, encoding = "UTF-8", local = new.env())
}
cat("\nLISTO: todo el análisis se ejecutó sin errores.\n")
