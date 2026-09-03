# CPS Project

Estructura base para dos encuestas de sociologos, su codificacion cualitativa y los analisis comparativos.

## Carpetas

- `data/raw/`: fuentes originales sin modificar.
- `data/processed/`: bases limpias, codebook y etiquetas.
- `analysis/`: scripts y notebooks de exploracion, modelado y comparacion.
- `reports/`: entregables en Quarto para exportar a HTML.
- `docs/`: metodologia, referencias y notas de codificacion.
- `outputs/`: figuras, tablas y modelos.
- `src/`: funciones reutilizables.

## Fuentes

- `data/raw/muestra_500/base_500_sociologos.xlsx`
- `data/raw/muestra_1000/base_24_de_mayo.xlsx`

## Quarto

- Los articulos y reportes van en `reports/`.
- Las referencias comunes van en `docs/referencias.bib`.

## Flujo

1. Ingesta y limpieza.
2. Normalizacion de variables entre muestras.
3. Consolidacion del codebook.
4. Codificacion cualitativa y entrenamiento del clasificador.
5. Descriptivos por muestra.
6. Analisis comparativo.
7. Redaccion en Quarto.
