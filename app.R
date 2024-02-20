library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)

data("iris")

source("scripts/mod_singlefilter.R")
source("scripts/mod_filter.R")

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        tagList(
            filterUI(id = "more_filter", names(iris)),
            dataTableOutput(outputId = "table")
        )
        
    )
)

server <- function(input, output) {
    df <- filterServer(id = "more_filter", iris)
    output$table <- renderDataTable({
        df()
    },
    options = list(pageLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
