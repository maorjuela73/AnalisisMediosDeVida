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
                                 plotOutput("bigPlot")
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
                                 plotOutput("bigPlot2")
                         )
                 )  
        ),
        tabPanel(title = paste("CAPITAL SOCIAL"),
                 sidebarLayout(
                         sidebarPanel(
                                 selectInput(inputId = "plotToShow3",
                                             label = "Gráfico a visualizar",
                                             choices = setNames(df_soci$nombre, df_soci$descripcion),
                                             selected = NULL,
                                             multiple = FALSE
                                 )
                         ),
                         mainPanel(
                                 plotOutput("bigPlot3")
                         )
                 )  
        ),
        tabPanel(title = paste("CAPITAL HUMANO"),
                 sidebarLayout(
                         sidebarPanel(
                                 selectInput(inputId = "plotToShow4",
                                             label = "Gráfico a visualizar",
                                             choices = setNames(df_humano$nombre, df_humano$descripcion),
                                             selected = NULL,
                                             multiple = FALSE
                                 )
                         ),
                         mainPanel(
                                 plotOutput("bigPlot4")
                         )
                 )
        ),
        tabPanel(title = paste("CAPITAL FINANCIERO"),
                 sidebarLayout(
                         sidebarPanel(
                                 selectInput(inputId = "plotToShow5",
                                             label = "Gráfico a visualizar",
                                             choices = setNames(df_financi$nombre, df_financi$descripcion),
                                             selected = NULL,
                                             multiple = FALSE
                                 )
                         ),
                         mainPanel(
                                 plotOutput("bigPlot5")
                         )
                )
         )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
        
        output$bigPlot <- renderPlot({
                actividadesPrincipales[[input$plotToShow]]
        })
        
        output$bigPlot2 <- renderPlot({
                capitalFisi[[input$plotToShow2]]
        })
        
        output$bigPlot3 <- renderPlot({
                capitalSoci[[input$plotToShow3]]
        })
        
        output$bigPlot4 <- renderPlot({
                capitalHumanoSalida[[input$plotToShow4]]
        })
        
        output$bigPlot5 <- renderPlot({
                capitalFinanci[[input$plotToShow5]]
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
