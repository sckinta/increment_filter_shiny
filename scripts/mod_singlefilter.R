calculate_filter <- function(filter_val, comp_sign, col_selected, ori_df) {
    if (!is.null(filter_val)) {
        if (filter_val == "") {
            filter_val = NULL
        }
    }
    
    if (any(map_lgl(list(filter_val, comp_sign, col_selected), is.null))) {
        TRUE
    } else{
        if (comp_sign == "includes") {
            ori_df[[col_selected]] == filter_val
        } else if (comp_sign == "excludes") {
            ori_df[[col_selected]] != filter_val
        } else{
            comparison_fn <- get(comp_sign)
            # Create a boolean array by applying the comparison function
            boolean_array <- comparison_fn(ori_df[[col_selected]], as.numeric(filter_val))
        }
    }
}

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
          }
          else{
              pickerInput(
                  inputId = ns("filter"),
                  label = filter_label,
                  choices = unique(df[[col_selected]]),
                  multiple = T,
                  options = list(`live-search`=TRUE)
              )
          }
      })
      
      filter <- reactive({
          calculate_filter(
              filter_val = input$filter,
              comp_sign = input$compare,
              col_selected = input$column,
              ori_df = df
          )
      })
      
      out <- list(
        and_or = reactive(input$and_or),
        button = reactive(input$bttn),
        filter = filter
      )

      out
    }
  )
}