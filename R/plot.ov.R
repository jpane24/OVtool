#### Plotting Package for OV ####
plot.ov <- function(x, col="color", print_all="all") {
  prep_plots = prep_for_plotsjp(x)

  #### some defaults ####
  .pardefault <- par(no.readonly=TRUE)

  par(ask = TRUE)

  plot_1 = es_plot(prep=prep_plots, col=col)
  plot_2 = pval_point_plot(prep=prep_plots, col=col)
  plot_3 = es_point_plot(prep=prep_plots,col=col)
  if(print_all == "all"){
    print(plot_1)
    print(plot_2)
    if(text_high!=""){print(prep_plots$text_high)}
    if(text_high_es!=""){print(prep_plots$text_high_es)}
    print(plot_3)
    if(text_high!=""){print(prep_plots$text_high)}
    if(text_high_es!=""){print(prep_plots$text_high_es)}
  }
  else if(print_all == "1"){
    par(.pardefault)
    print(plot_1)
  } else if(print_all == "2"){
    par(.pardefault)
    print(plot_2)
    if(text_high!=""){print(prep_plots$text_high)}
    if(text_high_es!=""){print(prep_plots$text_high_es)}
  } else if(print_all == "3"){
    par(.pardefault)
    print(plot_3)
    if(text_high!=""){print(prep_plots$text_high)}
    if(text_high_es!=""){print(prep_plots$text_high_es)}
  }
  par(.pardefault)
  grDevices::palette("default")
}
