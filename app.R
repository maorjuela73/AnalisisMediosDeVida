#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(plotly)

# Load plots
source(file = "src/toShiny.R", encoding = 'UTF-8')

# Define UI for application that draws a histogram
ui <- navbarPage(
        title = "Análisis de medios de vida", 
        tabPanel(title = paste("GENERAL"),
                 sidebarLayout(
                         sidebarPanel(
                                 selectInput(inputId = "plotToShow",
                                             label = "Gráfico a visualizar",
                                             choices = setNames(a$nombre, a$descripcion),
                                             selected = NULL,
                                             multiple = FALSE
                                 )
                         ),
                         mainPanel(
                                 plotlyOutput("bigPlot"),
                                 tableOutput("bigPlot2")
                         )
                 )
        ),
        tabPanel(title = paste("CAPITAL FÍSICO")),
        tabPanel(title = paste("CAPITAL SOCIAL")),
        tabPanel(title = paste("CAPITAL HUMANO")),
        tabPanel(title = paste("CAPITAL FINANCIERO"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
        
        output$bigPlot <- renderPlotly({
                actividadesPrincipales[[input$plotToShow]]
        })
        
        output$bigPlot2 <- renderTable({
                actividadesPrincipales[[input$plotToShow]]
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
