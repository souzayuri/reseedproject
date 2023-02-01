
folded_js_bar=function(data, fold_bar, spread_bar, reset_button_id){
  library(r2d3)
  ### Add folded function
  fold=ifelse( input$bar_clicked == "" | input$reset_button_id, 99 , input$bar_clicked)
  if(input$reset_button_id){
    grouped=rlang::expr(fold_bar)
  }else{
    if(input$bar_clicked != ""){
      grouped=rlang::expr(spread_bar)
    }else{
      grouped=rlang::expr(fold_bar)
    }
  }
  
  if (fold!= 99) data <- filter(data, fold_bar == fold)
  
  finaldata = data %>%
    dplyr::group_by(!!grouped) %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::mutate(
      y = n,
      x = !!grouped) %>%
    dplyr::select(x, y) %>% 
    dplyr::mutate(label = x)

  return( r2d3(finaldata, "bar_plot.js") )
}


