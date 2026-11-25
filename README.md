# ODIN · Salud Financiera PYME · Colombia 2016–2025

Primer producto de **ODIN · Observatorio de Desarrollo, Innovación y Negocios**, del Semillero de Investigación Pensamiento y Praxis
(Pontificia Universidad Javeriana Cali): un tablero público sobre la salud financiera de las sociedades que reportan
a la Superintendencia de Sociedades bajo NIIF para Pymes.

**Ver el tablero:** https://stvtaylorvasquez-web.github.io/indice-pymes/ (navegador o celular, sin instalar nada).

## Estructura

```
indice-pymes/
├── analisis/                  Cálculo en R (lo corre quien actualiza los datos)
│   ├── 00_ejecutar_todo.R       Corre todo en orden: 01 → 05
│   ├── 01 … 04                  Limpieza, univariado, bivariado y multivariado
│   ├── 05_exportar_web.R        Escribe los datos del tablero en sitio/datos
│   ├── 3 PYMES FABIO.Rproj      Abrir con doble clic antes de correr
│   ├── BD_INDICADORES_…xlsx     Base original (solo en el computador, no se sube)
│   └── resultados/              Salidas intermedias de R (se regeneran, no se suben)
├── sitio/                     Tablero web
│   ├── index.qmd                Filtros, carga de datos y funciones compartidas
│   ├── estilo.scss              Colores y tipografía institucional
│   ├── paginas/                 Una pestaña por archivo
│   ├── datos/                   Datos del tablero (los escribe el script 05)
│   ├── semillero/               trabajos.csv, eventos.csv, lideres.csv y sus fotos
│   └── assets/                  Logos de ODIN y de la Javeriana
└── .github/                   Publicación automática en GitHub Pages (no tocar)
```

**Principio:** R calcula y el sitio solo muestra. Quien consulta el tablero no necesita R ni Excel.

## Actualizar los datos

1. Poner el Excel en `analisis/` (si cambia el nombre, ajustar `RUTA_EXCEL` en los scripts 01, 04 y 05).
2. Doble clic en `analisis/3 PYMES FABIO.Rproj` → abrir `00_ejecutar_todo.R` → **Source**. Debe terminar con "LISTO".
3. Revisar: abrir `sitio/index.qmd` → **Render**.
4. GitHub Desktop: resumen del cambio → **Commit to main** → **Push origin**. El sitio se actualiza en unos 2 minutos.

## Pestañas

| Pestaña | Archivo |
|---|---|
| ¿Quiénes somos? | `paginas/_semillero.qmd` (misión, visión, líderes, trabajos y eventos; contenido en `sitio/semillero/`) |
| Panorama | `paginas/_panorama.qmd` |
| Indicadores | `paginas/_indicadores.qmd` |
| Inductores | `paginas/_inductores.qmd` |
| Liquidez | `paginas/_liquidez.qmd` |
| Z' Altman | `paginas/_zscore.qmd` |
| ¿Por qué caen? | `paginas/_porque.qmd` |
| Empresas | `paginas/_empresas.qmd` |
| Alerta | `paginas/_alerta.qmd` |
| Descargas | `paginas/_datos.qmd` |
| Metodología | `paginas/_metodologia.qmd` |

**Pestaña nueva:** crear `sitio/paginas/_nueva.qmd` (primera línea `# Nombre`) y agregar
`{{< include paginas/_nueva.qmd >}}` al final de `sitio/index.qmd`, en el orden en que debe aparecer.

**Semillero:** los trabajos se editan en `sitio/semillero/trabajos.csv` (integrantes separados por punto y coma) y los eventos
en `sitio/semillero/eventos.csv` y los líderes en `sitio/semillero/lideres.csv`. Cada foto (JPG, máximo 1400 px) va en la misma carpeta `sitio/semillero/`.

### Reglas para programar tarjetas

- Datos disponibles: `datos` (panel completo), `base` (filtrado por sector y departamento), `sel` (además por año), `alerta`, `geo`.
- Filtros: `anio`, `sector`, `depto`. Funciones: `resumen()`, `conPNG()`, `botonesDescarga()`, `fijar()`.
- Todo `Plot.plot` lleva `width` y `height` (`anchoMedio`, `anchoCompleto`, `anchoTercio`).
- No escribir una celda como `x = ({...})` (usar `Object.assign({}, {...})`); cada celda `//| output: false` define una sola variable.

**Filtros por pestaña:** la barra «Explorar» se oculta en las pestañas listadas en `PESTANAS_SIN_FILTROS` (encabezado de `sitio/index.qmd`).

## Opciones del script 05

- `TASA_IMPUESTO <- 0.35`: tarifa para la rentabilidad operativa después de impuestos (inductores de valor).
- `PUBLICAR_ALERTA_POR_EMPRESA <- TRUE`: con `FALSE` la alerta solo se publica agregada por sector.

## Notas metodológicas

- Panel balanceado: 7.772 sociedades que reportaron los 10 años (posible sesgo de supervivencia).
- "Distress" es la zona de riesgo del Z' de Altman (Z' < 1,23), una señal contable. **No es quiebra observada.**
- "Pymes" se refiere al marco contable NIIF Grupo 2, no al tamaño legal de la empresa (Decreto 957 de 2019).
- Logos: el de la Javeriana es el oficial (`assets/logo-javeriana-cali*.png`); el de ODIN está en `assets/odin-*.png` (logo, versión blanca e ícono).
