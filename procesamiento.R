# ==============================================================================
# TRABAJO PRÁCTICO FINAL - CURSO ASET 2026
# Script de Procesamiento de Datos e Indicadores de Precariedad Laboral
# ==============================================================================

# 1. Carga de Librerías
library(tidyverse)
library(eph)
library(openxlsx)
library(scales)

# Diccionario de aglomerados de la EPH (códigos oficiales).
diccionario_aglomerados <- tibble(
  AGLOMERADO = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17, 18, 19,
                 20, 22, 23, 25, 26, 27, 29, 30, 31, 32, 33, 34, 36, 38, 91, 93),
  nom_aglo = c(
    "Gran La Plata", "Bahía Blanca - Cerri", "Gran Rosario", "Gran Santa Fe",
    "Gran Paraná", "Posadas", "Gran Resistencia", "Comodoro Rivadavia - Rada Tilly",
    "Gran Mendoza", "Corrientes", "Gran Córdoba", "Concordia", "Formosa",
    "Neuquén - Plottier", "Santiago del Estero - La Banda", "Jujuy - Palpalá",
    "Río Gallegos", "Gran Catamarca", "Salta", "La Rioja", "San Luis - El Chorrillo",
    "Gran San Juan", "Gran Tucumán - Tafí Viejo", "Santa Rosa - Toay",
    "Ushuaia - Río Grande", "Ciudad de Buenos Aires", "Partidos del GBA",
    "Mar del Plata - Batán", "Río Cuarto", "San Nicolás - Villa Constitución",
    "Rawson - Trelew", "Viedma - Carmen de Patagones"
  )
)

# 2. Carga Dinámica de Datos desde la carpeta \bases
directorio_bases <- "bases"

if (!dir.exists(directorio_bases)) {
  dir.create(directorio_bases)
  message("Carpeta 'bases/' creada. Coloque los archivos de microdatos allí.")
}

archivos_bases <- list.files(
  path = directorio_bases,
  pattern = "\\.(rds|txt|csv)$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

# Si no hay bases locales, el paquete facilita obtener los microdatos publicados por INDEC.
if (length(archivos_bases) == 0) {
  message("No se encontraron bases locales en 'bases/'. Se descargan los microdatos publicados por INDEC mediante el paquete eph.")
  eph_raw <- get_microdata(year = 2024, trimester = 4, type = "individual")
  fuente <- "Descarga mediante eph (microdatos de INDEC)"
} else {
  fuente <- "BASES_REPO"
  eph_raw <- archivos_bases %>%
    map_dfr(function(archivo) {
      extension <- tolower(tools::file_ext(archivo))
      switch(
        extension,
        rds = readRDS(archivo),
        csv = readr::read_csv(archivo, show_col_types = FALSE),
        txt = read.delim(archivo, sep = ";", dec = ","),
        stop("Formato de archivo no admitido: ", archivo)
      )
    })
}

# 3. Limpieza, Filtrado y Recodificación según Diccionario Oficial EPH
eph_procesada <- eph_raw %>%
  # Tratamiento de valores no respuesta en ingresos (-9 a NA)
  mutate(
    P21 = if_else(P21 == -9, NA_real_, as.numeric(P21)),
    P47T = if_else(P47T == -9, NA_real_, as.numeric(P47T))
  ) %>%
  # Delimitación del Universo: Ocupados Asalariados (ESTADO == 1 & CAT_OCUP == 3)
  filter(ESTADO == 1, CAT_OCUP == 3) %>%
  mutate(
    # Variable Dummy de Precariedad Laboral (Descuento Jubilatorio PP07H)
    # 1 = Sin descuento (Precario/Informal), 0 = Con descuento (Registrado)
    precario_dummy = case_when(
      PP07H == 2 ~ 1L,
      PP07H == 1 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Categorización por Sexo (CH04: 1 = Varón, 2 = Mujer)
    sexo = case_when(CH04 == 1 ~ "Varón", CH04 == 2 ~ "Mujer", TRUE ~ "Sin dato"),
    
    # Edad cumplida (CH06): los menores de 18 no forman parte de estos tramos.
    grupo_etario = case_when(
      CH06 >= 18 & CH06 <= 29 ~ "18 a 29 años",
      CH06 >= 30 & CH06 <= 49 ~ "30 a 49 años",
      CH06 >= 50 ~ "50 años y más",
      TRUE ~ NA_character_
    ),
    grupo_etario = factor(
      grupo_etario,
      levels = c("18 a 29 años", "30 a 49 años", "50 años y más")
    ),
    
    # NIVEL_ED según el diccionario: 7 es sin instrucción y 9 es NS/NR.
    nivel_educativo = case_when(
      NIVEL_ED %in% c(1, 2, 3, 7) ~ "Hasta secundario incompleto",
      NIVEL_ED %in% c(4, 5) ~ "Secundario completo / superior incompleto",
      NIVEL_ED == 6 ~ "Superior universitario completo",
      NIVEL_ED == 9 ~ "No sabe / no responde",
      TRUE ~ NA_character_
    ),
    nivel_educativo = factor(
      nivel_educativo,
      levels = c(
        "Hasta secundario incompleto",
        "Secundario completo / superior incompleto",
        "Superior universitario completo",
        "No sabe / no responde"
      )
    ),
    
    # PP04C: 1-5 personas (1-5), 6 (6-10), 7-8 (11-40), 9-12 (41+), 99 NS/NR.
    tamano_establec = case_when(
      PP04C %in% 1:6 ~ "Hasta 10 personas",
      PP04C %in% 7:8 ~ "11 a 40 personas",
      PP04C %in% 9:12 ~ "41 personas o más",
      PP04C == 99 ~ "No sabe / no responde",
      TRUE ~ NA_character_
    ),
    tamano_establec = factor(
      tamano_establec,
      levels = c(
        "Hasta 10 personas",
        "11 a 40 personas",
        "41 personas o más",
        "No sabe / no responde"
      )
    )
  ) %>%
  # Join con el Diccionario Oficial de Aglomerados de la EPH
  filter(PP07H %in% c(1, 2)) %>%
  left_join(diccionario_aglomerados, by = "AGLOMERADO") %>%
  mutate(nom_aglo = if_else(is.na(nom_aglo), paste("Código", AGLOMERADO), nom_aglo))

# 4. Cálculo de Indicadores Ponderados (PONDERA)

# A) Tasa General
tasa_general <- eph_procesada %>%
  summarize(
    casos_muestrales = n(),
    poblacion_asalariada = sum(PONDERA, na.rm = TRUE),
    precarios_ponderados = sum(PONDERA[precario_dummy == 1], na.rm = TRUE),
    registrados_ponderados = sum(PONDERA[precario_dummy == 0], na.rm = TRUE),
    tasa_precariedad = precarios_ponderados / poblacion_asalariada
  )

# B) Por Sexo y Tramo Etario
tasa_demografica <- eph_procesada %>%
  filter(!is.na(grupo_etario)) %>%
  group_by(sexo, grupo_etario) %>%
  summarize(
    casos_muestrales = n(),
    poblacion_asalariada = sum(PONDERA, na.rm = TRUE),
    precarios_ponderados = sum(PONDERA[precario_dummy == 1], na.rm = TRUE),
    tasa_precariedad = precarios_ponderados / poblacion_asalariada,
    .groups = "drop"
  )

# C) Por Nivel Educativo
tasa_educacion <- eph_procesada %>%
  filter(!is.na(nivel_educativo)) %>%
  group_by(nivel_educativo) %>%
  summarize(
    casos_muestrales = n(),
    poblacion_asalariada = sum(PONDERA, na.rm = TRUE),
    precarios_ponderados = sum(PONDERA[precario_dummy == 1], na.rm = TRUE),
    tasa_precariedad = precarios_ponderados / poblacion_asalariada,
    .groups = "drop"
  )

# D) Por Tamaño de Establecimiento
tasa_tamano <- eph_procesada %>%
  filter(!is.na(tamano_establec)) %>%
  group_by(tamano_establec) %>%
  summarize(
    casos_muestrales = n(),
    poblacion_asalariada = sum(PONDERA, na.rm = TRUE),
    precarios_ponderados = sum(PONDERA[precario_dummy == 1], na.rm = TRUE),
    tasa_precariedad = precarios_ponderados / poblacion_asalariada,
    .groups = "drop"
  )

# E) Por Aglomerado con Nombres Oficiales
tasa_aglomerado <- eph_procesada %>%
  group_by(AGLOMERADO, nom_aglo) %>%
  summarize(
    casos_muestrales = n(),
    poblacion_asalariada = sum(PONDERA, na.rm = TRUE),
    precarios_ponderados = sum(PONDERA[precario_dummy == 1], na.rm = TRUE),
    tasa_precariedad = precarios_ponderados / poblacion_asalariada,
    .groups = "drop"
  ) %>%
  arrange(desc(tasa_precariedad))

# 5. Exportación de tablas y gráficos
directorio_resultados <- "resultados"
if (!dir.exists(directorio_resultados)) {
  dir.create(directorio_resultados)
}

hojas_excel <- list(
  "Resumen_General"      = tasa_general,
  "Por_Sexo_y_Edad"      = tasa_demografica,
  "Por_Nivel_Educativo"  = tasa_educacion,
  "Por_Tamano_Empresa"   = tasa_tamano,
  "Por_Aglomerado"       = tasa_aglomerado
)

write.xlsx(hojas_excel, file = file.path(directorio_resultados, "indicadores_precariedad.xlsx"))

directorio_graficos <- file.path(directorio_resultados, "graficos")
if (!dir.exists(directorio_graficos)) {
  dir.create(directorio_graficos)
}

grafico_demografico <- ggplot(
  tasa_demografica,
  aes(x = grupo_etario, y = tasa_precariedad, fill = sexo)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(tasa_demografica$tasa_precariedad, na.rm = TRUE) * 1.15)
  ) +
  scale_fill_manual(values = c("Mujer" = "#e74c3c", "Varón" = "#2980b9", "Sin dato" = "#7f8c8d")) +
  labs(
    title = "Tasa de precariedad por grupo etario y sexo",
    x = "Grupo etario",
    y = "Tasa de precariedad",
    fill = "Sexo"
  ) +
  theme_minimal()

grafico_tamano <- ggplot(
  filter(tasa_tamano, tamano_establec != "No sabe / no responde"),
  aes(x = tasa_precariedad, y = tamano_establec)
) +
  geom_col(fill = "#27ae60", width = 0.65) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(tasa_tamano$tasa_precariedad, na.rm = TRUE) * 1.15)
  ) +
  labs(
    title = "Tasa de precariedad según tamaño del establecimiento",
    x = "Tasa de precariedad",
    y = "Personas que trabajan en el establecimiento"
  ) +
  theme_minimal()

grafico_educacion <- ggplot(
  filter(tasa_educacion, nivel_educativo != "No sabe / no responde"),
  aes(x = tasa_precariedad, y = nivel_educativo)
) +
  geom_col(fill = "#8e44ad", width = 0.65) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(tasa_educacion$tasa_precariedad, na.rm = TRUE) * 1.15)
  ) +
  labs(
    title = "Tasa de precariedad por nivel educativo",
    x = "Tasa de precariedad",
    y = "Máximo nivel educativo alcanzado"
  ) +
  theme_minimal()

grafico_aglomerado <- ggplot(
  tasa_aglomerado,
  aes(x = tasa_precariedad, y = reorder(nom_aglo, tasa_precariedad))
) +
  geom_col(fill = "#247a78", width = 0.72) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Tasa de precariedad por aglomerado",
    x = "Tasa de precariedad",
    y = "Aglomerado"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(directorio_graficos, "precariedad_sexo_edad.png"), grafico_demografico, width = 8, height = 5, dpi = 300)
ggsave(file.path(directorio_graficos, "precariedad_tamano_establecimiento.png"), grafico_tamano, width = 8, height = 5, dpi = 300)
ggsave(file.path(directorio_graficos, "precariedad_nivel_educativo.png"), grafico_educacion, width = 8, height = 5, dpi = 300)
ggsave(file.path(directorio_graficos, "precariedad_aglomerado.png"), grafico_aglomerado, width = 9, height = 8, dpi = 300)

# 6. Log de Ejecución
log_csv <- file.path(directorio_resultados, "log_ejecuciones.csv")
if (file.exists(log_csv)) {
  registro_previo <- readr::read_csv(log_csv, show_col_types = FALSE)
  run_number <- if (nrow(registro_previo) > 0 && any(!is.na(registro_previo$run_number))) {
    max(registro_previo$run_number, na.rm = TRUE) + 1
  } else {
    1
  }
} else {
  registro_previo <- tibble()
  run_number <- 1
}

registro_nuevo <- tibble(
  run_number = run_number,
  timestamp = Sys.time(),
  registros = nrow(eph_procesada),
  fuente = fuente
)
readr::write_csv(
  bind_rows(registro_previo, registro_nuevo),
  log_csv
)

message("Procesamiento finalizado con éxito. Tablas y gráficos guardados en 'resultados/'.")
message("Ejecución registrada en 'resultados/log_ejecuciones.csv'.")
