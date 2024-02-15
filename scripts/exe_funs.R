filter_df <- function(filter_val, comp_sign, col_selected, ori_df){
    if(!is.null(filter_val)){
        if(filter_val==""){
            filter_val=NULL
        }
    }
    if(any(map_lgl(list(filter_val, comp_sign, col_selected), is.null))){
        df <- ori_df
    }else{
        if(comp_sign=="includes"){
            df <- ori_df |>
                filter_at(col_selected, ~.x %in% filter_val)
        }else if(comp_sign=="excludes"){
            df <- ori_df |>
                filter_at(col_selected, ~!(.x %in% filter_val))
        }else{
            subset_str <- paste(col_selected, comp_sign, as.numeric(filter_val))
            df <- ori_df |>
                as_tibble() |>
                filter(eval(parse(text = subset_str)))
        }
    }
    
    return(df)
}