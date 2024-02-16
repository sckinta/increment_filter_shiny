#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)

data("iris")

source("scripts/mod_singlefilter.R")
source("scripts/mod_filter.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        tagList(
            # singleFilterUI(id = "first_filter", idx = 1),
            filterUI(id = "more_filter", names(iris)),
            dataTableOutput(outputId = "table")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # out <- singleFilterServer(id = "first_filter", idx = 1)
    # observe({
    #     req(out())
    #     print(out())
    #     
    # })
    df <- filterServer(id = "more_filter", iris)
    output$table <- renderDataTable({
        df()
    },
    options = list(pageLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
