# Salud Financiera PYME · Colombia 2016–2025

Tablero público del índice de salud financiera de las sociedades que reportan a la Superintendencia de
Sociedades bajo NIIF para Pymes. Semillero de Investigación Pensamiento y Praxis, Pontificia Universidad Javeriana Cali.

**Ver el tablero:** `https://<usuario>.github.io/<repositorio>/`. Funciona en cualquier navegador y en el celular, sin instalar nada.

## Cómo está organizado

```
analisis/            Motor de cálculo en R (lo mantiene el semillero)
  00_ejecutar_todo.R   Corre todo en orden (01 → 06)
  01 … 04              Limpieza, univariado, bivariado, multivariado
  05_exportar_powerbi.R  Tablas para Power BI (uso interno)
  06_exportar_web.R      Archivos livianos para el sitio → sitio/datos/
sitio/               Tablero web (Quarto + Observable)
  index.qmd            Filtros globales, carga de datos y funciones compartidas
  paginas/             Una pestaña por archivo:
    _panorama.qmd        Panorama (mapa, zonas, sectores, indicadores clave)
    _indicadores.qmd     Indicadores por sector, evolución, ranking y descriptiva
    _liquidez.qmd        Cuadrantes de liquidez y deuda, deciles de endeudamiento
    _zscore.qmd          Z' de Altman: zonas, distribución, transiciones, rachas
    _alerta.qmd          Alerta temprana del año siguiente
    _datos.qmd           Centro de descargas (CSV y Excel)
    _metodologia.qmd     Metodología y limitaciones
  datos/               Archivos que genera el script 06 (+ departamentos.geojson, fijo)
  estilos/javeriana.scss  Colores y tipografía institucional
  assets/              Logo
.github/workflows/   Publicación automática en GitHub Pages
```

**Principio:** R calcula y el sitio solo muestra. Quien consulta el tablero no necesita R, ni Excel, ni Power BI.

## Actualizar los datos (por ejemplo, cuando salga el corte 2026)

1. Copiar el Excel nuevo en `analisis/` con el nombre que esperan los scripts (`BD_INDICADORES_…xlsx`). El Excel **no** se sube a GitHub.
2. Abrir `analisis/3 PYMES FABIO.Rproj` en RStudio, abrir `00_ejecutar_todo.R` y presionar **Source**.
3. Al final, el script 06 sobrescribe los archivos de `sitio/datos/`.
4. Subir esos cambios a GitHub (en RStudio: pestaña *Git* → *Commit* → *Push*, o arrastrando los archivos en github.com).
5. En 1 o 2 minutos el sitio se actualiza solo, y el link sigue siendo el mismo.

## Agregar o modificar una pestaña

Cada pestaña es un archivo en `sitio/paginas/`. Para modificar una, se edita su archivo.

Para **agregar una pestaña nueva**:

1. Crear `sitio/paginas/_nueva.qmd`. La primera línea es el nombre de la pestaña: `# Nueva`.
2. Dentro, cada **fila** es `## Row {height="auto"}` y cada **tarjeta** es un bloque ```` ```{ojs} ```` con `//| title: "…"`.
3. Agregar al final de `sitio/index.qmd` la línea `{{< include paginas/_nueva.qmd >}}`.

Dentro de cualquier tarjeta se pueden usar:
- Datos: `datos` (todo el panel), `base` (filtrado por sector y departamento), `sel` (además por año), `alerta`, `geo` (mapa).
- Filtros: `anio`, `sector`, `depto`.
- Funciones: `resumen(filas)` (medianas y % por zona), `conPNG(grafico, "nombre")` (botón de imagen),
  `botonesDescarga(() => filas, "nombre")` (CSV y Excel), `fijar(viewof sector, "F")` (cambiar un filtro desde un gráfico).
- Tamaños: `anchoMedio` (media fila) y `anchoCompleto` (fila completa). Todo `Plot.plot` debe llevar `width` y `height`.

Dos reglas de Quarto que conviene saber: no escribir una celda como `x = ({...})` (usar `Object.assign({}, {...})`),
y cada celda con `//| output: false` debe definir una sola variable.

Para un indicador nuevo: agregar la columna en `06_exportar_web.R`, volver a correrlo y usarla en la página.

Para ver los cambios antes de publicarlos: abrir `sitio/index.qmd` en RStudio y presionar **Render**.

## Logo institucional

`sitio/assets/logo-javeriana-cali.png` es el logo oficial (escudo central sobre azul). Para el sitio se usa
`logo-javeriana-cali-blanco.png`, el mismo logo con fondo transparente y en blanco, ubicado en el encabezado azul
a 120 px de alto (el Manual de Identidad pide un mínimo digital de 88 px para el escudo central). El logo no se modifica.

Colores institucionales usados: azul `#2C5697` (Pantone 7685C) y amarillo `#F8CD00` (Pantone 7405C).

## Opciones del script 06

`PUBLICAR_ALERTA_POR_EMPRESA <- TRUE` muestra la tabla de alerta con el nombre de cada empresa. Con `FALSE` el tablero
solo publica la alerta agregada por sector. Decisión del semillero.

## Notas metodológicas

- Panel balanceado: 7.772 sociedades que reportaron los 10 años, lo que implica un posible sesgo de supervivencia.
- "Distress" es la zona de riesgo del Z' de Altman (Z' < 1,23), una señal contable. **No es quiebra observada.**
- "Pymes" se refiere al marco contable NIIF Grupo 2, no al tamaño legal de la empresa (Decreto 957 de 2019).
