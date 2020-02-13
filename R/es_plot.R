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
  v <- ggplot(r1_df, aes(es_grid, rho_grid, z = trt_effect)) +
    ylim(0,max(r1_df$rho_grid)) +
    geom_contour(col="black") + xlab("Association between unobserved confounder and treatment indicator\n (effect size scale)") +
    ylab("Absolute Correlation with Outcome (rho)") + ggtitle("ES contours") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.key = element_blank(), legend.text = element_text(size = 10),
          legend.key.size =  unit(0.5, "in")) +
    scale_linetype_manual(name = 'P-value Threshold', values = pval_lines,
                          labels = pvals) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[1]),
                 color = color[[1]][1], breaks=pvals[1]) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[2]),
                 color = color[[1]][1], breaks=pvals[2]) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[3]),
                 color = color[[1]][1], breaks=pvals[3]) +
    geom_text_contour(stroke=.2) +
    annotation_custom(grob = textGrob(label = raw, vjust = 3,
                                      gp = gpar(cex = .75)),
                      ymin = 0, ymax = 0, xmin = 0, xmax = 0)
  if(col == "bw"){
    v = v + theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                               legend.key = element_blank(), legend.text = element_text(size = 10),
                               legend.key.size =  unit(0.5, "in"))
  }
  return(v)
}

