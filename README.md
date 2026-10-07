# Trabajo Práctico Final – EPH

**Curso:** Automatización y Visualización de Datos con R y Google Tools  
**Docentes:** Facundo Lastra y Guido Weksler  
**Integrante:** Lucas Cargnel

Este proyecto analiza la precariedad laboral de las personas ocupadas asalariadas con microdatos de la Encuesta Permanente de Hogares (EPH) del INDEC. Se define como precario el puesto asalariado sin descuento jubilatorio declarado (`PP07H = 2`).

## Estructura

```text
.
├── bases/                              # Microdatos locales opcionales (.rds, .txt o .csv)
├── procesamiento.R                    # Carga, transforma y genera indicadores, gráficos y log
├── resultados/
│   ├── indicadores_precariedad.xlsx   # Tablas de resultados
│   ├── log_ejecuciones.csv            # Historial de ejecuciones
│   └── graficos/                      # Gráficos PNG generados por el procesamiento
├── informe/
│   ├── informe_precariedad.qmd        # Fuente Quarto del informe
│   └── informe_precariedad.pdf        # Informe renderizado
└── dashboard/
    └── app.R                          # Dashboard interactivo en Shiny
```

## Ejecución

Desde la carpeta raíz del proyecto:

1. Instalar los paquetes de R requeridos, si aún no están instalados:

   ```r
   install.packages(c("tidyverse", "eph", "openxlsx", "scales", "readxl", "shiny"))
   ```

2. Procesar los datos:

   ```sh
   Rscript procesamiento.R
   ```

   El script busca archivos `.rds`, `.txt` y `.csv` en `bases/` y sus subcarpetas. Si no encuentra microdatos locales, el paquete `eph` facilita la descarga de los microdatos publicados por INDEC. Los resultados se escriben en `resultados/`.

3. Renderizar el informe Quarto:

   ```sh
   quarto render informe/informe_precariedad.qmd
   ```

4. Iniciar el dashboard:

   ```r
   shiny::runApp("dashboard")
   ```

El informe y el dashboard leen las hojas generadas en `resultados/indicadores_precariedad.xlsx`; por eso, se debe ejecutar primero el procesamiento.
