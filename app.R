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
                                             choices = setNames(df_princip$nombre, df_princip$descripcion),
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
        tabPanel(title = paste("CAPITAL FÍSICO"),
                sidebarLayout(
                         sidebarPanel(
                                 selectInput(inputId = "plotToShow2",
                                             label = "Gráfico a visualizar",
                                             choices = setNames(df_fisico$nombre, df_fisico$descripcion),
                                             selected = NULL,
                                             multiple = FALSE
                                 )
                         ),
                         mainPanel(
                                 plotlyOutput("bigPlo3"),
                                 tableOutput("bigPlot4")
                         )
                 )  
        ),
        tabPanel(title = paste("CAPITAL SOCIAL")),
        tabPanel(title = paste("CAPITAL HUMANO"),
                 sidebarLayout(
                         sidebarPanel(
                                 selectInput(inputId = "plotToShow3",
                                             label = "Gráfico a visualizar",
                                             choices = setNames(df_humano$nombre, df_humano$descripcion),
                                             selected = NULL,
                                             multiple = FALSE
                                 )
                         ),
                         mainPanel(
                                 plotlyOutput("bigPlot5"),
                                 tableOutput("bigPlot6")
                         )
                 )
        ),
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
        
        output$bigPlot3 <- renderPlotly({
                capitalFisi[[input$plotToShow2]]
        })
        
        output$bigPlot4 <- renderTable({
                capitalFisi[[input$plotToShow2]]
        })
        
        output$bigPlot5 <- renderPlotly({
                capitalHumanoSalida[[input$plotToShow3]]
        })
        
        output$bigPlot6 <- renderTable({
                capitalHumanoSalida[[input$plotToShow3]]
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
