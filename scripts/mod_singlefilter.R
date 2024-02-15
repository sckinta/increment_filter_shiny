singleFilterUI <- function(id, idx) {
  ns <- NS(id)
  if(idx == 1){
      or_and_widget <- column(2, h5(""))
      column_label = "feature"
      bttn_style <- 'padding:0px; padding-top:25px'
  }else{
      column_label = NULL
      or_and_widget <- column(2, selectizeInput(inputId = ns(paste("and_or", idx, sep="_")), 
                                      label = NULL,
                                      choices = c("AND", "OR"), 
                                      multiple = F))
      bttn_style <- 'padding:0px'
  }
  
  tagList(
      or_and_widget,
      column(2, style='padding:0px;', 
             selectizeInput(inputId = ns(paste("column", idx, sep="_")), 
                     label = column_label,
                     choices = names(iris), 
                     multiple = F)
             ),
      column(2, style='padding:0px;', 
             uiOutput(outputId = ns(paste("compare_ui", idx, sep="_")))
             ),
      column(3, style='padding:0px;', 
             uiOutput(outputId = ns(paste("filter_ui", idx, sep="_")))
             ),
      column(1, style=bttn_style, 
             actionButton(inputId = ns(paste("bttn", idx, sep="_")),
                 label = "+"))
  )
}

singleFilterServer <- function(id, idx) {
  moduleServer(
    id,
    function(input, output, session) {
      output[[paste("compare_ui", idx, sep="_")]] <- renderUI({
          req(input[[paste("column", idx, sep="_")]])
          ns <- session$ns
          col_selected <- input[[paste("column", idx, sep="_")]]
          if(idx == 1){
              filter_label = "compare"
          }else{
              filter_label = NULL
          }
          if(class(iris[[col_selected]]) == "numeric"){
              choice_type <- c(">", "<", ">=", "<=", "==", "!=")
          }else{
              choice_type <- c("includes", "excludes")
          }
          pickerInput(
              inputId = ns(paste("compare", idx, sep="_")),
              label = filter_label,
              choices = choice_type,
              multiple = F
          )
      })
      
      output[[paste("filter_ui", idx, sep="_")]] <- renderUI({
          req(input[[paste("column", idx, sep="_")]])
          ns <- session$ns
          col_selected <- input[[paste("column", idx, sep="_")]]
          if(idx == 1){
              filter_label = "value"
              text_style <- 'padding:0px; padding-left:1px; padding-top:25px'
          }else{
              filter_label = NULL
              text_style <- 'padding:0px; padding-left:1px'
          }
          if(class(iris[[col_selected]]) == "numeric"){
              col_range <- range(iris[[col_selected]], na.rm = T)
              col_range <- format(col_range, scientific = T, digits = 2, drop0trailing=T)
              tagList(
                  column(6, style='padding:0px;', textInput(
                      inputId = ns(paste("filter", idx, sep="_")),
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
                  inputId = ns(paste("filter", idx, sep="_")),
                  label = filter_label,
                  choices = unique(iris[[col_selected]]),
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
        and_or = reactiveVal(input[[paste("and_or", idx, sep="_")]]),
        col_selected = reactiveVal(input[[paste("column", idx, sep="_")]]),
        filter_val = reactiveVal(input[[paste("filter", idx, sep="_")]]),
        comp_sign = reactiveVal(input[[paste("compare", idx, sep="_")]]),
        button = reactiveVal(input[[paste("bttn", idx, sep="_")]])
        
                             
      )
      # observe({
      #     print(paste("filter", idx, sep="_"))
      #     print(out())
      # })
      out
    }
  )
}