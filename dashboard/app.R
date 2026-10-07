library(shiny)
library(tidyverse)
library(openxlsx)
library(scales)

path_excel <- "../resultados/indicadores_precariedad.xlsx"
if (!file.exists(path_excel)) {
  stop("No se encontró el Excel de resultados. Ejecute procesamiento.R desde la raíz del proyecto.")
}

hojas_requeridas <- c(
  "Resumen_General",
  "Por_Sexo_y_Edad",
  "Por_Nivel_Educativo",
  "Por_Tamano_Empresa",
  "Por_Aglomerado"
)
hojas_disponibles <- getSheetNames(path_excel)
hojas_faltantes <- setdiff(hojas_requeridas, hojas_disponibles)
if (length(hojas_faltantes) > 0) {
  stop(
    "El Excel no tiene las hojas requeridas: ",
    paste(hojas_faltantes, collapse = ", "),
    ". Vuelva a ejecutar procesamiento.R."
  )
}

datos_general <- read.xlsx(path_excel, sheet = "Resumen_General")
datos_demog <- read.xlsx(path_excel, sheet = "Por_Sexo_y_Edad")
datos_educacion <- read.xlsx(path_excel, sheet = "Por_Nivel_Educativo")
datos_tamano <- read.xlsx(path_excel, sheet = "Por_Tamano_Empresa")
datos_aglo <- read.xlsx(path_excel, sheet = "Por_Aglomerado")

ui <- fluidPage(
  titlePanel("Precariedad laboral en Argentina | EPH-INDEC"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros"),
      selectInput(
        "filtro_sexo",
        "Sexo:",
        choices = c("Todos", sort(unique(datos_demog$sexo))),
        selected = "Todos"
      ),
      helpText("Universo: personas ocupadas asalariadas."),
      helpText("Precariedad: ausencia de descuento jubilatorio declarado."),
      helpText("Fuente: microdatos de la EPH, INDEC.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Resumen",
          br(),
          tableOutput("tabla_general")
        ),
        tabPanel(
          "Edad y sexo",
          br(),
          plotOutput("grafico_demog", height = "430px")
        ),
        tabPanel(
          "Nivel educativo",
          br(),
          plotOutput("grafico_educacion", height = "430px")
        ),
        tabPanel(
          "Tamaño del establecimiento",
          br(),
          plotOutput("grafico_tamano", height = "380px")
        ),
        tabPanel(
          "Aglomerados",
          br(),
          tableOutput("tabla_aglo")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  datos_demog_filt <- reactive({
    if (input$filtro_sexo == "Todos") {
      datos_demog
    } else {
      filter(datos_demog, sexo == input$filtro_sexo)
    }
  })

  output$tabla_general <- renderTable({
    datos_general %>%
      transmute(
        `Casos muestrales` = comma(casos_muestrales, big.mark = ".", decimal.mark = ","),
        `Población asalariada estimada` = comma(poblacion_asalariada, big.mark = ".", decimal.mark = ","),
        `Población sin descuento estimada` = comma(precarios_ponderados, big.mark = ".", decimal.mark = ","),
        `Tasa de precariedad` = percent(tasa_precariedad, accuracy = 0.1)
      )
  })

  output$grafico_demog <- renderPlot({
    ggplot(
      datos_demog_filt(),
      aes(x = grupo_etario, y = tasa_precariedad, fill = sexo)
    ) +
      geom_col(position = position_dodge(width = 0.8), width = 0.72) +
      geom_text(
        aes(label = percent(tasa_precariedad, accuracy = 0.1)),
        position = position_dodge(width = 0.8),
        vjust = -0.35,
        size = 3.5
      ) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, max(datos_demog$tasa_precariedad, na.rm = TRUE) * 1.15)
      ) +
      scale_fill_manual(
        values = c("Mujer" = "#e74c3c", "Varón" = "#2980b9", "Sin dato" = "#7f8c8d")
      ) +
      labs(
        title = "Tasa de precariedad por edad y sexo",
        x = "Grupo etario",
        y = "Tasa de precariedad",
        fill = "Sexo"
      ) +
      theme_minimal()
  })

  output$grafico_educacion <- renderPlot({
    grafico_datos <- filter(
      datos_educacion,
      nivel_educativo != "No sabe / no responde"
    )

    ggplot(
      grafico_datos,
      aes(x = tasa_precariedad, y = nivel_educativo)
    ) +
      geom_col(fill = "#8e44ad", width = 0.65) +
      scale_x_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, max(grafico_datos$tasa_precariedad, na.rm = TRUE) * 1.15)
      ) +
      labs(
        title = "Tasa de precariedad por nivel educativo",
        x = "Tasa de precariedad",
        y = "Máximo nivel educativo alcanzado"
      ) +
      theme_minimal()
  })

  output$grafico_tamano <- renderPlot({
    grafico_datos <- filter(
      datos_tamano,
      tamano_establec != "No sabe / no responde"
    )

    ggplot(
      grafico_datos,
      aes(x = tasa_precariedad, y = tamano_establec)
    ) +
      geom_col(fill = "#27ae60", width = 0.65) +
      geom_text(
        aes(label = percent(tasa_precariedad, accuracy = 0.1)),
        hjust = -0.12,
        size = 3.5
      ) +
      scale_x_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, max(grafico_datos$tasa_precariedad, na.rm = TRUE) * 1.15)
      ) +
      labs(
        title = "Tasa de precariedad por tamaño del establecimiento",
        x = "Tasa de precariedad",
        y = "Personas que trabajan en el establecimiento"
      ) +
      theme_minimal()
  })

  output$tabla_aglo <- renderTable({
    datos_aglo %>%
      transmute(
        Aglomerado = nom_aglo,
        `Casos muestrales` = comma(casos_muestrales, big.mark = ".", decimal.mark = ","),
        `Población asalariada estimada` = comma(poblacion_asalariada, big.mark = ".", decimal.mark = ","),
        `Tasa de precariedad` = percent(tasa_precariedad, accuracy = 0.1)
      )
  })
}

shinyApp(ui = ui, server = server)
