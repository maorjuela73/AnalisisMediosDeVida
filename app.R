#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
        title = strong("Análisis de medios de vida"), 
        tabPanel(title = paste("CAPITAL FÍSICO")),
        tabPanel(title = paste("CAPITAL SOCIAL")),
        tabPanel(title = paste("CAPITAL HUMANO")),
        tabPanel(title = paste("CAPITAL FINANCIERO"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
