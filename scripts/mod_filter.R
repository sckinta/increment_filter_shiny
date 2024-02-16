filterUI <- function(id, column_choices) {
  ns <- NS(id)
  tagList(
      fluidRow(singleFilterUI(
        id = ns("single_filter"), column_choices = column_choices, include_and_or = FALSE
        )),
     fluidRow(uiOutput(outputId = ns("filter_group")))
  )
}

filterServer <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      filter_1 <- singleFilterServer(id = "single_filter", df = df, filter_label = "compare", text_style = 'padding:0px; padding-left:1px; padding-top:25px')
      click_id <- reactiveVal(1)
      
      ## Create a reactiveValues object to store the filters we create
      filters <- reactiveValues()
      
      observe({
        req(filter_1$filter())
        filters[[as.character(click_id())]] <- filter_1
      })
      
      
      filtered_df <- reactive({
          req(filter_1$filter())
          cat("filter_data", as.character(click_id()), ":")
          # We start with the filter calculated by the original filter
          the_filter <- filter_1$filter()
          
          # Now, we can iterate through any filters we've applied in addition 
          # to the original filter
          for (filter_id in names(filters)) {
              if (is.null(filters[[filter_id]]$and_or()) || filters[[filter_id]]$and_or() == "AND") {
                  the_filter <- the_filter & filters[[filter_id]]$filter()
              } else {
                  the_filter <- the_filter | filters[[filter_id]]$filter()
              }
          }
          cat(sum(the_filter), "\n")
          df[the_filter,]
      })
      
      # Create a reactive value to store additional filters that we add to our 
      # UI. We can index into this variable using the ID of the filter. filtered_df 
      # will take a dependency on it and any time components are changed, the overall
      # filter will be recomputed and a new filtered_df created
      filter_group_ui <- reactiveValues()
      
      # observeEvent(filters[[as.character(click_id())]]$button(),{ # <---- change option 1
      observeEvent(filter_1$button(), { # <---- change option 2
            # click_id(click_id() + 1) # <---- should update click_id here, but it through errors
            cat("create_filter", as.character(click_id()), ":")
            new_id = as.character(click_id())
            output$filter_group <- renderUI({
                ns <- session$ns
                filter_group_ui[[new_id]] <- singleFilterUI(id = ns(new_id), column_choices = names(df))
                tagList(
                  reactiveValuesToList(filter_group_ui)
                )
            })
            new_filter <- singleFilterServer(id = new_id, df = df)
            filters[[new_id]] <- new_filter
            cat(sum(new_filter$filter()), "\n")
            click_id(click_id() + 1)
      })
      filtered_df
    }
  )
}