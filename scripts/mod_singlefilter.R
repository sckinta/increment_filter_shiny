singleFilterUI <- function(id, column_choices, include_and_or = TRUE) {
  ns <- NS(id)
  
  if (include_and_or) {
      column_label = NULL
      or_and_widget <- column(2, selectizeInput(inputId = ns("and_or"), 
                                                label = NULL,
                                                choices = c("AND", "OR"), 
                                                multiple = F))
      bttn_style <- 'padding:0px' 
  } else {
      or_and_widget <- column(2, h5(""))
      column_label = "feature"
      bttn_style <- 'padding:0px; padding-top:25px'
  }
  
  tagList(
      or_and_widget,
      column(2, style='padding:0px;', 
             selectizeInput(inputId = ns("column"), 
                     label = column_label,
                     choices = column_choices, 
                     multiple = F)
             ),
      column(2, style='padding:0px;', 
             uiOutput(outputId = ns("compare_ui"))
             ),
      column(3, style='padding:0px;', 
             uiOutput(outputId = ns("filter_ui"))
             ),
      column(1, style=bttn_style, 
             actionButton(inputId = ns("bttn"),
                 label = "+"))
  )
}

singleFilterServer <- function(id, df, filter_label = NULL, text_style = "padding:0px; padding-left:1px") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
        
      output$compare_ui <- renderUI({
          req(input$column)
          
          col_selected <- input$column

          if(class(df[[col_selected]]) == "numeric"){
              choice_type <- c(">", "<", ">=", "<=", "==", "!=")
          }else{
              choice_type <- c("includes", "excludes")
          }
          pickerInput(
              inputId = ns("compare"),
              label = filter_label,
              choices = choice_type,
              multiple = F
          )
      })
      
      output$filter_ui <- renderUI({
          req(input$column)
          col_selected <- input$column

          if(class(df[[col_selected]]) == "numeric"){
              col_range <- range(df[[col_selected]], na.rm = T)
              col_range <- format(col_range, scientific = T, digits = 2, drop0trailing=T)
              tagList(
                  column(6, style='padding:0px;', textInput(
                      inputId = ns("filter"),
                      label = filter_label, value = NULL
                  )),
                  column(2, style=text_style, 
                         tagList(
                             h5(paste0("(",paste(col_range, collapse = "~"), ")"))
                         )
                         
                   )
              )
          }else{
              pickerInput(
                  inputId = ns("filter"),
                  label = filter_label,
                  choices = unique(df[[col_selected]]),
                  multiple = T,
                  options = list(`live-search`=TRUE)
              )
          }
      })
      
      # observe({
      #     print(paste("compare", idx, ":",sep="_"))
      #     req(input[[paste("compare", idx, sep="_")]])
      #     print(input[[paste("compare", idx, sep="_")]])
      # })
      
      # out <- reactive({
      #     req(
      #         c(
      #             input[[paste("and_or", idx, sep="_")]],
      #             input[[paste("column", idx, sep="_")]],
      #             input[[paste("filter", idx, sep="_")]],
      #             input[[paste("compare", idx, sep="_")]]
      #         )
      #     )
      #     list(
      #         and_or = ifelse(idx==1, "AND", input[[paste("and_or", idx, sep="_")]]),
      #         col_selected = input[[paste("column", idx, sep="_")]],
      #         filter_val = input[[paste("filter", idx, sep="_")]],
      #         comp_sign = input[[paste("compare", idx, sep="_")]],
      #         button = input[[paste("bttn", idx, sep="_")]]
      #     )
      # })
      out <- list(
        and_or = reactive(input$and_or),
        col_selected = reactive(input$column),
        filter_val = reactive(input$filter),
        comp_sign = reactive(input$compare),
        button = reactive(input$bttn)


      )
      # observe({
      #     print(paste("filter", idx, sep="_"))
      #     print(out())
      # })
      out
    }
  )
}