options(repos = c(CRAN = "https://cloud.r-project.org"))

paquetes <- c("tidyverse", "eph", "openxlsx", "lubridate", "scales")
invisible(lapply(paquetes, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}))

library(tidyverse)
library(eph)
library(openxlsx)
library(lubridate)
library(scales)

# Directorios del proyecto
base_dir <- file.path(getwd(), "bases")
result_dir <- file.path(getwd(), "resultados")
if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
if (!dir.exists(result_dir)) dir.create(result_dir, recursive = TRUE)

# -------- Función auxiliar para leer todas las bases presentes --------
leer_bases <- function(path) {
  archivos <- list.files(
    path,
    pattern = "\\.(csv|xlsx|xls)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  if (length(archivos) == 0) {
    return(NULL)
  }

  lista_datos <- lapply(archivos, function(archivo) {
    ext <- tolower(tools::file_ext(archivo))

    if (ext == "csv") {
      read.csv(archivo, stringsAsFactors = FALSE)
    } else {
      openxlsx::read.xlsx(archivo)
    }
  })

  datos <- bind_rows(lista_datos)
  return(datos)
}

# -------- Carga de datos --------
raw_data <- leer_bases(base_dir)

if (is.null(raw_data) || nrow(raw_data) == 0) {
  message("No se encontraron bases en 'bases/'. Se intenta cargar la EPH desde la API.")
  raw_data <- get_microdata(year = 2024, trimester = 4, type = "individual")
  fuente <- "EPH_API"
} else {
  fuente <- "BASES_REPO"
}

# -------- Transformación: limpieza, filtrado y recodificación --------
raw_data <- raw_data %>%
  mutate(
    across(c(P21, P47T), ~ ifelse(as.numeric(.) == -9, NA_real_, as.numeric(.))),
    precario_dummy = if_else(PP07H == 2, 1, 0),
    precario_label = if_else(PP07H == 2, "Empleo Precario (Sin Descuento)", "Empleo Registrado (Con Descuento)"),
    grupo_etario = case_when(
      CH06 < 30 ~ "18 a 29 años",
      CH06 >= 30 & CH06 <= 49 ~ "30 a 49 años",
      CH06 >= 50 ~ "50 años y más"
    ),
    grupo_etario = factor(grupo_etario, levels = c("18 a 29 años", "30 a 49 años", "50 años y más")),
    nivel_educativo = case_when(
      NIVEL_ED %in% c(1, 2, 7) ~ "Menor a secundaria",
      NIVEL_ED %in% c(3, 4) ~ "Secundaria completa",
      NIVEL_ED %in% c(5, 6) ~ "Superior / Universitario"
    ),
    nivel_educativo = factor(nivel_educativo, levels = c("Menor a secundaria", "Secundaria completa", "Superior / Universitario")),
    tamano_establec = case_when(
      PP04C %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "Pequeño (hasta 10 pers.)",
      PP04C %in% c(9, 10, 11, 12) ~ "Mediano (11 a 50 pers.)",
      PP04C == 99 ~ "Grande (más de 50 pers.)",
      TRUE ~ "NS/NR"
    ),
    tamano_establec = factor(tamano_establec, levels = c("Pequeño (hasta 10 pers.)", "Mediano (11 a 50 pers.)", "Grande (más de 50 pers.)", "NS/NR")),
    sexo = if_else(CH04 == 1, "Varón", "Mujer")
  ) %>%
  filter(ESTADO == 1, CAT_OCUP == 3)

# -------- Indicadores ponderados --------
calcular_tasa <- function(df, grupo = NULL) {
  if (is.null(grupo)) {
    df %>% summarise(
      poblacion_asalariada = sum(PONDERA, na.rm = TRUE),
      precarios_ponderados = sum(PONDERA[precario_dummy == 1], na.rm = TRUE),
      tasa_precariedad = if_else(poblacion_asalariada > 0, precarios_ponderados / poblacion_asalariada, NA_real_),
      .groups = "drop"
    )
  } else {
    df %>%
      group_by(across(all_of(grupo))) %>%
      summarise(
        poblacion_asalariada = sum(PONDERA, na.rm = TRUE),
        precarios_ponderados = sum(PONDERA[precario_dummy == 1], na.rm = TRUE),
        tasa_precariedad = if_else(poblacion_asalariada > 0, precarios_ponderados / poblacion_asalariada, NA_real_),
        .groups = "drop"
      )
  }
}

tasa_general <- calcular_tasa(raw_data)
tasa_demografica <- calcular_tasa(raw_data, c("sexo", "grupo_etario"))
tasa_tamano <- calcular_tasa(raw_data, "tamano_establec")
tasa_aglomerado <- calcular_tasa(raw_data, "AGLOMERADO")

# -------- Guardado de resultados --------
hojas_excel <- list(
  Resumen_General = tasa_general,
  Por_Sexo_y_Edad = tasa_demografica,
  Por_Tamano_Empresa = tasa_tamano,
  Por_Aglomerado = tasa_aglomerado
)

write.xlsx(hojas_excel, file = file.path(result_dir, "indicadores_precariedad.xlsx"), overwrite = TRUE)

# -------- Log de ejecuciones --------
log_file <- file.path(result_dir, "log_ejecuciones.csv")
run_counter <- 0
if (file.exists(log_file)) {
  prev_log <- read.csv(log_file, stringsAsFactors = FALSE)
  if (nrow(prev_log) > 0) {
    run_counter <- max(prev_log$run_number, na.rm = TRUE)
  }
}

new_run <- run_counter + 1
log_row <- data.frame(
  run_number = new_run,
  timestamp = Sys.time(),
  registros = nrow(raw_data),
  fuente = fuente,
  stringsAsFactors = FALSE
)

if (file.exists(log_file)) {
  write.table(
    rbind(read.csv(log_file, stringsAsFactors = FALSE), log_row),
    file = log_file,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    append = FALSE
  )
} else {
  write.csv(log_row, file = log_file, row.names = FALSE)
}

message("Procesamiento finalizado con éxito. Archivos guardados en 'resultados/'.")
message("Ejecución registrada en 'resultados/log_ejecuciones.csv'.")