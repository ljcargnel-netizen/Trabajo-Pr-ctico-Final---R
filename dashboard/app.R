library(shiny)
library(tidyverse)
library(openxlsx)
library(scales)

# Carga de datos procesados
 datos_demog <- read.xlsx("../resultados/indicadores_precariedad.xlsx", sheet = "Por_Sexo_y_Edad")
 datos_aglo  <- read.xlsx("../resultados/indicadores_precariedad.xlsx", sheet = "Por_Aglomerado")

ui <- fluidPage(
  titlePanel("Dashboard Interactivo: Precariedad Laboral en Argentina"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "filtro_sexo",
        "Seleccione Sexo:",
        choices = c("Todos", unique(datos_demog$sexo)),
        selected = "Todos"
      ),
      hr(),
      helpText("Datos procesados a partir de la Encuesta Permanente de Hogares (INDEC).")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Por Edad y Sexo", plotOutput("grafico_barras")),
        tabPanel("Tabla por Aglomerados", tableOutput("tabla_aglo"))
      )
    )
  )
)

server <- function(input, output) {
  datos_filtrados <- reactive({
    if (input$filtro_sexo == "Todos") {
      datos_demog
    } else {
      datos_demog %>% filter(sexo == input$filtro_sexo)
    }
  })

  output$grafico_barras <- renderPlot({
    ggplot(datos_filtrados(), aes(x = grupo_etario, y = tasa_precariedad, fill = sexo)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Precariedad por Grupo Etario", x = "Edad", y = "Tasa de Precariedad") +
      theme_minimal()
  })

  output$tabla_aglo <- renderTable({
    datos_aglo %>%
      mutate(tasa_precariedad = percent(tasa_precariedad, accuracy = 0.1)) %>%
      select(Aglomerado = AGLOMERADO, `Población Asalariada` = poblacion_asalariada, `Tasa Precariedad` = tasa_precariedad)
  })
}

shinyApp(ui = ui, server = server)
