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

  ## Parallel coordinates plot of cluster means
  #### ES with p-value overlay (no points) ####
  v <- ggplot2::ggplot(r1_df, ggplot2::aes(es_grid, rho_grid, z = trt_effect)) +
    ggplot2::ylim(0,max(r1_df$rho_grid)) +
    ggplot2::geom_contour(col="black") + ggplot2::xlab("Association between unobserved confounder and treatment indicator\n (effect size scale)") +
    ggplot2::ylab("Absolute Correlation with Outcome (rho)") + ggplot2::ggtitle("ES contours") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
          legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10),
          legend.key.size =  grid::unit(0.5, "in")) +
    # scale_linetype_manual(name = 'P-value Threshold', values = pval_lines,
    #                       labels = pvals) +
    # geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[1]),
    #              color = color[[1]][1], breaks=pvals[1]) +
    # geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[2]),
    #              color = color[[1]][1], breaks=pvals[2]) +
    # geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[3]),
    #              color = color[[1]][1], breaks=pvals[3]) +
    metR::geom_text_contour(stroke=.2) +
    ggplot2::annotation_custom(grob = grid::textGrob(label = raw, vjust = 3,
                                      gp = grid::gpar(cex = .75)),
                      ymin = 0, ymax = 0, xmin = 0, xmax = 0)
  if(col == "bw"){
    v = v + ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                               legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10),
                               legend.key.size =  grid::unit(0.5, "in"))
  }
  return(v)
}

