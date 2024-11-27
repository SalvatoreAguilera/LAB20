library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(maps)

ui <- fluidPage(
  titlePanel("Auto Geospatial Mapper"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("geo_columns"),
      selectInput("map_type", "Select Map Type", 
                  choices = c("County", "State", "Country")),
      actionButton("generate_map", "Generate Map")
    ),
    
    mainPanel(
      leafletOutput("leaflet_map"),
      plotOutput("ggplot_map")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive to read data
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Dynamically detect geographical columns
  output$geo_columns <- renderUI({
    req(dataset())
    geo_cols <- names(dataset())[sapply(dataset(), function(col) 
      any(grepl("county|state|country", tolower(col))))]
    selectInput("geo_column", "Select Geospatial Column", choices = geo_cols)
  })
  
  # Generate map based on selection
  observeEvent(input$generate_map, {
    req(dataset(), input$geo_column, input$map_type)
    
    data <- dataset()
    geo_col <- input$geo_column
    map_type <- tolower(input$map_type)
    
    if (map_type == "county") {
      map_data <- map_data("county")
    } else if (map_type == "state") {
      map_data <- map_data("state")
    } else {
      map_data <- map_data("world")
    }
    
    # Join map data with uploaded data
    map_data <- map_data %>%
      mutate(region = tolower(region)) %>%
      left_join(data, by = setNames(geo_col, "region"))
    
    # Render Leaflet map
    output$leaflet_map <- renderLeaflet({
      leaflet(map_data) %>%
        addTiles() %>%
        addPolygons(
          lng = ~long,
          lat = ~lat,
          fillColor = ~factor(region),
          color = "white",
          weight = 1,
          popup = ~paste(region)
        )
    })
    
    # Render ggplot map
    output$ggplot_map <- renderPlot({
      ggplot(map_data, aes(long, lat, group = group, fill = factor(region))) +
        geom_polygon(color = "white") +
        coord_fixed(1.3) +
        theme_minimal() +
        labs(title = paste("Map of", input$map_type), fill = geo_col)
    })
  })
}

shinyApp(ui, server)
