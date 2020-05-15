#### Effect Size plot (no points) ####
es_plot = function(prep, col){
  r1_df = prep$r1_df
  pvals = prep$pvals
  pval_lines = prep$pval_lines
  raw = prep$raw

  # Black and white or color
  color = ifelse(col == "color", list(c("red", "blue")),
                 ifelse(col == "bw", list(c("grey70", "black")),
                        stop("Please specify 'bw' or 'color'.")))
  redregion = "white"

  ## Parallel coordinates plot of cluster means
  #### ES with p-value overlay (no points) ####
  v <- ggplot2::ggplot(r1_df, ggplot2::aes(es_grid, rho_grid, z = trt_effect)) +
    ggplot2::geom_rect(ggplot2::aes(ymin = max(rho_grid),
                                    ymax = Inf,
                                    xmin = -Inf,
                                    xmax = Inf),
                       fill = redregion,
                       alpha = 0.01) +
    ggplot2::geom_rect(ggplot2::aes(ymin = -Inf,
                                    ymax = 0,
                                    xmin = -Inf,
                                    xmax = Inf),
                       fill = "snow2",
                       alpha = 0.01) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0,max(r1_df$rho_grid)), expand = c(0, 0)) +
    ggplot2::theme_linedraw() +
    ggplot2::geom_contour(col="black") + ggplot2::xlab("Association between unobserved confounder and treatment indicator\n (effect size scale)") +
    ggplot2::ylab("Absolute Correlation with Outcome (rho)") + ggplot2::ggtitle("ES contours") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
          legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10),
          legend.key.size =  grid::unit(0.5, "in"),
          plot.margin = ggplot2::unit(c(1,7,1,1), "lines")) +
    metR::geom_text_contour(stroke=.2) +
    ggplot2::annotation_custom(grob = grid::textGrob(label = raw, vjust = 3,
                                                      gp = grid::gpar(cex = .75)),
                                ymin = .1, ymax = .1,
                                xmax = max(r1_df$es_grid)+.1,
                                xmin = max(r1_df$es_grid)+.05)


  if(col == "bw"){
    v = v
  }
  return(v)
}

