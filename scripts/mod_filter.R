filterUI <- function(id) {
  ns <- NS(id)
  tagList(
      fluidRow(singleFilterUI(
        id = ns("single_filter"), idx = 1
        )),
     fluidRow(uiOutput(outputId = ns("filter_group")))
  )
}

filterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # filters <- reactiveValues()
      filters <- singleFilterServer(id = "single_filter", idx = 1)
      click_id <- reactiveVal(1)
      iris_df <- reactiveVal(iris)
      
      # observeEvent(filters(), {
      #     # req(filters()$filter_val, filters()$comp_sign, filters()$col_selected)
      #     df <- filter_df(filter_val = filters()$filter_val,
      #                     comp_sign = filters()$comp_sign,
      #                     col_selected = filters()$col_selected,
      #                     ori_df = iris_df())
      #     iris_df(df)
      # })

      # filters_val <- reactiveValues()
      filtered_df <- reactive({
          req(c(filters$filter_val(), filters$comp_sign(), filters$col_selected()))

          # filters_val$filter_val <- filters$filter_val()
          # filters_val$comp_sign <- filters$comp_sign()
          # filters_val$col_selected <- filters$col_selected()
          #
          # df <- filter_df(filter_val = filters_val$filter_val,
          #                 comp_sign = filters_val$comp_sign,
          #                 col_selected = filters_val$col_selected,
          #                 ori_df = iris_df())
          df <- filter_df(filter_val = filters$filter_val(),
                          comp_sign = filters$comp_sign(),
                          col_selected = filters$col_selected(),
                          ori_df = iris_df())
      })

      
      observeEvent(filters$button(), {
            click_id(click_id() + 1)
            output$filter_group <- renderUI({
                ns <- session$ns
                singleFilterUI(id = ns(paste("filter", click_id(), sep="_")), idx = click_id())
            })
            filters2 <- singleFilterServer(id = paste("filter", click_id(), sep="_"), idx = click_id())

            print(click_id()) # debug
            print(filters2$filter_val()) # debug
            
            req(c(filters2$filter_val(), filters2$comp_sign(), filters2$col_selected()))
            
            if(filters2$and_or()=="AND"){
                df2 <- filter_df(filter_val = filters2$filter_val(),
                                 comp_sign = filters2$comp_sign(),
                                 col_selected = filters2$col_selected(),
                                 ori_df = iris_df())
            }
            else if(filters2$and_or()=="OR"){
                df2 <- iris_df() |>
                    bind_rows(
                      filter_df(filter_val = filters2$filter_val(),
                                comp_sign = filters2$comp_sign(),
                                col_selected = filters2$col_selected(),
                                ori_df = iris)
                    ) |>
                    distinct()
            }

            # updating (not updating)
            df2 |>
                iris_df()
            
            # update filters (throw error)
            for (v in names(filters)){
              filters[[v]] <- filters2[[v]]
            }

      })
      filtered_df
      # iris_df
      
    }
  )
}