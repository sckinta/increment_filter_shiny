filterUI <- function(id, column_choices) {
  ns <- NS(id)
  tagList(
      fluidRow(
        singleFilterUI(id = ns("single_filter"), column_choices = column_choices, include_and_or = FALSE)
        ),
     fluidRow(column(12, uiOutput(outputId = ns("filter_ui_1")), style = "padding:0px; padding-left:3px"))
  )
}

filterServer <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      filter_1 <- singleFilterServer(id = "single_filter", df = df, filter_label = "compare", text_style = 'padding:0px; padding-left:1px; padding-top:25px')
      click_id <- reactiveVal(1)
      
      ## Create a reactiveValues object to store the filters we create
      filters <- reactiveValues(
        "1" = filter_1
      )
      
      
      observeEvent(filters[[as.character(click_id())]]$button(),{
          
          id <- click_id()
          next_id <- id + 1
          output[[paste("filter_ui", id, sep="_")]] <- renderUI({
              ns <- session$ns
              tagList(
                fluidRow(
                  column(12, singleFilterUI(id = ns(paste("filter", id, sep="_")), column_choices = names(df)))
                  
                ),
                fluidRow(
                  column(12, uiOutput(outputId = ns(paste("filter_ui", next_id, sep="_"))))
                )
              )
              
          })
          
          
          filter_next <- singleFilterServer(id = paste("filter", id, sep="_"), df = df)
          
          
          filters[[as.character(next_id)]] <- filter_next
          
          cat("create",paste("filter_ui", next_id, sep="_"), "\n")
          
          click_id(click_id() + 1)
      })
      
      filtered_df <- reactive({
          req(filters[[as.character(click_id())]]$filter)
          id <- click_id()
          the_filter <- TRUE

          # Now, we can iterate through any filters we've applied in addition
          # to the original filter
          for (filter_id in names(filters)) {
              if (is.null(filters[[filter_id]]$and_or()) || filters[[filter_id]]$and_or() == "AND") {
                  the_filter <- the_filter & filters[[filter_id]]$filter()
              } else {
                  the_filter <- the_filter | filters[[filter_id]]$filter()
              }
          }
          cat(id, "filter :",sum(the_filter), "\n")
          
          df[the_filter,]
      })
      
      filtered_df
    }
  )
}