library(shiny)
library(leaflet)
library(dplyr)
library(readxl)
library(fmsb)
library(tidyr)
library(scales)

# Cargar datos
base <- read_excel("www/database.xlsx", sheet = "II_ER_caracterizacion")

# Variables para radar chart
vars_radar <- names(base)[which(names(base) == "PFF"):which(names(base) == "CTC/CL")]

# Escalar de 0 a 100 para spider chart
base_scaled <- base %>%
  mutate(across(all_of(vars_radar), ~ rescale(.x, to = c(0, 100), na.rm = TRUE)))

# UI
ui <- fluidPage(
  titlePanel("Caracterización de Fenotipos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("fenotipo", "Selecciona un fenotipo:", choices = unique(base$phenotype)),
      actionButton("ayuda", "Ver abreviaturas")
    ),
    mainPanel(
      fluidRow(
        column(6, uiOutput("foto")),
        column(6, leafletOutput("mapa", height = 300))
      ),
      br(),
      h4("Características generales"),
      uiOutput("descripcion"),
      br(),
      h4("Radar chart de variables cuantitativas"),
      plotOutput("radar", height = "500px"),
      br()
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$ayuda, {
    showModal(modalDialog(
      title = "Abreviaturas de las variables",
      easyClose = TRUE,
      size = "l",
      HTML("<pre style='white-space: pre-wrap; font-size: 14px;'>
L*: coordenada colorimétrica de luminosidad
a*: coordenada colorimétrica: –verde/rojo+
b*: coordenada colorimétrica: –azul/amarillo+
ATT: Acidez total titulable
CTC: Contenido total de carotenoides
CLa: Clorofila a
CLb: Clorofila b
DMAF: Diámetro máximo ecuatorial del fruto
DMIF: Diámetro mínimo ecuatorial del fruto
DPF: Diámetro polar del fruto
DPPH•: Actividad antioxidante (Radicales DPPH•)
PEN: Resistencia a la penetración
PFF: Peso fresco fruto
PSF: Peso seco fruto
PSFL: Peso seco flor
PSS: Peso seco de las semillas
PT: Polifenoles totales
SS: Sólidos solubles
</pre>")
    ))
  })
  
  # Mostrar imagen
  output$foto <- renderUI({
    path <- paste0("fotos/", input$fenotipo, ".jpg")
    tags$img(src = path, height = "300px", style = "border: 1px solid #ccc; border-radius: 10px;")
  })
  
  # Mapa interactivo
  output$mapa <- renderLeaflet({
    datos <- base %>% filter(phenotype == input$fenotipo)
    leaflet() %>% addTiles() %>%
      addMarkers(lng = datos$longitude, lat = datos$latitude,
                 popup = paste("Fenotipo", datos$phenotype))
  })
  
  # Descripción de características generales
  output$descripcion <- renderUI({
    datos <- base %>% filter(phenotype == input$fenotipo)
    HTML(paste0(
      "<ul>",
      "<li><b>Sitio:</b> ", datos$site, "</li>",
      "<li><b>Altura:</b> ", round(datos$height, 2), " m</li>",
      "<li><b>Diámetro a la altura del pecho (DBH):</b> ", round(datos$dbh, 2), " cm</li>",
      "<li><b>Diámetro base:</b> ", round(datos$base_diameter, 2), " cm</li>",
      "<li><b>Diámetro mayor:</b> ", round(datos$larger_diameter, 2), " m</li>",
      "<li><b>Diámetro menor:</b> ", round(datos$minor_diameter, 2), " m</li>",
      "</ul>"
    ))
  })
  
  # Radar chart
  output$radar <- renderPlot({
    datos <- base_scaled %>% filter(phenotype == input$fenotipo)
    datos_radar <- datos[, vars_radar]
    rownames(datos_radar) <- NULL
    chart_data <- rbind(rep(100, length(vars_radar)), rep(0, length(vars_radar)), datos_radar)
    radarchart(chart_data, axistype = 1,
               pcol = "#1f77b4", pfcol = scales::alpha("#1f77b4", 0.5), plwd = 2,
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 100, 20),
               vlcex = 1.5)
  })
}

shinyApp(ui, server)

