#install.packages("renv")
#renv::init()
#renv::restore()


library(shiny)
library(readxl)
library(tidyverse)
library(plotly)
library(DT)

df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")


data <- read_excel('data/raw/data.xlsx', sheet = 2, skip = 5)

wave_quality <- unique(data$Wave_quality)
wave_quality <- unique(data$Wave_quality)

wave_power <-  unique(data$Wave_power)

region <- unique(data$Zone)

bottom <- unique(data$Wave_bottom)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("California Surf Spots"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        selectInput("region",
                      selected = NULL,
                      label = "Region",
                      choices = region,
                      multiple=TRUE),
        selectInput("bottom",
                    selected = NULL,
                    label = "Bottom",
                    choices = bottom,
                    multiple=TRUE),
        selectInput(inputId = "quality",
                    selected = NULL,
                    label = "Wave Quality",
                    choices = wave_quality,
                    multiple=TRUE),
        selectInput(inputId = "power",
                      selected = NULL,
                      label = "Wave Power",
                      choices = wave_power,
                      multiple=TRUE),
        
        plotlyOutput("plot", width = "400px")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          DTOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  table <- reactive({
    filtered <- data
    if (!is.null(input$region)) {
      filtered <- filtered %>%
        filter(Zone %in% input$region)
    }
    if (!is.null(input$quality)) {
      filtered <- filtered %>%
        filter(Wave_quality %in% input$quality)
    }
    if (!is.null(input$power)) {
      filtered <- filtered %>%
        filter(Wave_power %in% input$power)
    }
    if (!is.null(input$bottom)) {
      filtered <- filtered %>%
        filter(Wave_bottom %in% input$bottom)
    }
    return(filtered)
  })
  
  output$table <- renderDT({
    table()
  })
  
  output$plot <- renderPlotly(
    plot_geo(df, locationmode = 'USA-states') |>
      layout(width = 500, height = 400,
        geo = list(scope = 'usa', 
                        projection = list(type = 'albers usa'), 
                   center = list(lat = 37, lon = -119),
                   lonaxis = list(range = c(-120, -114)),
                   lataxis = list(range = c(35, 49)))) |>
      config(scrollZoom = FALSE) |>
      add_markers(x = ~Longitude, y = ~Latitude, data = table(), text = ~Location, hoverinfo = 'text')
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
