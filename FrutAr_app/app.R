library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(bslib)

# Leer base de datos
especies_df <- read_delim("data_base_CFN.txt", delim = "\t", col_types = cols(.default = "c"))

# Cargar provincias de Argentina
provincias_sf <- ne_states(country = "Argentina", returnclass = "sf") %>%
  st_transform(4326)

# Diccionario de nombres a siglas
prov_siglas <- c(
  "Buenos Aires" = "BA", "Catamarca" = "CM", "Chaco" = "CH", "Chubut" = "CU", 
  "Córdoba" = "CR", "Corrientes" = "CS", "Entre Ríos" = "ER", "Formosa" = "FS",
  "Jujuy" = "JJ", "La Pampa" = "LP", "La Rioja" = "LR", "Mendoza" = "MZ",
  "Misiones" = "MS", "Neuquén" = "NQ", "Río Negro" = "RN", "Salta" = "ST",
  "San Juan" = "SJ", "San Luis" = "SL", "Santa Cruz" = "SC", "Santa Fe" = "SF",
  "Santiago del Estero" = "SE", "Tierra del Fuego" = "TF", "Tucumán" = "TC"
)

# UI
theme_custom <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat"),
  primary = "#2c7fb8"
)

ui <- fluidPage(
  theme = theme_custom,
  tags$head(
    tags$title("FrutAr - Explorador de Frutales Nativos"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/estilos.css")
  ),
  titlePanel("FrutAr – Explorador de Frutales Nativos"),
  sidebarLayout(
    sidebarPanel(
      actionButton("reset", "Reiniciar selección", class = "btn-primary"),
      br(), br(),
      textOutput("provincia_seleccionada")
    ),
    mainPanel(
      fluidRow(
        column(12,
               leafletOutput("mapa", height = 500)
        )
      ),
      fluidRow(
        column(12,
               h4("Especies recomendadas para este sitio:"),
               DTOutput("tabla")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  provincia_nombre <- reactiveVal(NULL)
  
  output$mapa <- renderLeaflet({
    leaflet(provincias_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        label = ~name,
        fillColor = "lightblue",
        fillOpacity = 0.3,
        color = "black",
        weight = 1
      )
  })
  
  observeEvent(input$mapa_click, {
    punto <- st_point(c(input$mapa_click$lng, input$mapa_click$lat)) %>%
      st_sfc(crs = 4326)
    
    prov <- provincias_sf[st_contains(provincias_sf, punto, sparse = FALSE), ]
    
    if (nrow(prov) > 0) {
      provincia_nombre(prov$name[1])
    } else {
      provincia_nombre(NULL)
    }
  })
  
  observeEvent(input$reset, {
    provincia_nombre(NULL)
  })
  
  output$provincia_seleccionada <- renderText({
    req(provincia_nombre())
    paste("Provincia detectada:", provincia_nombre())
  })
  
  output$tabla <- renderDT({
    req(provincia_nombre())
    sigla <- prov_siglas[[provincia_nombre()]]
    req(!is.null(sigla))
    
    especies_df %>%
      filter(grepl(sigla, Distribución)) %>%
      select(Especie, `Nombre común`, `Incorporación al CAA`, Hábito)
  }, options = list(pageLength = 10, dom = 'tip'))
}

shinyApp(ui = ui, server = server)
