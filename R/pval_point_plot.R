#### P-value contour plot with points ####
pval_point_plot = function(prep, col){
  r1_df = prep$r1_df
  pvals = prep$pvals
  pval_lines = prep$pval_lines
  text_high = prep$text_high
  obs_cors = prep$obs_cors
  raw = prep$raw

  # Black and white or color
  color = ifelse(col == "color", list(c("red", "blue")),
                 ifelse(col == "bw", list(c("grey70", "black")),
                        stop("Please specify 'bw' or 'color'.")))

  ## Pvalue
  v2 <- ggplot(r1_df, aes(x=es_grid, y=rho_grid, z = p_val)) +
    ylim(0,max(r1_df$rho_grid)) + xlim(c(min(r1_df$es_grid)), max(r1_df$es_grid)) +
    geom_contour(col='black') + xlab("Association between unobserved confounder and treatment indicator\n(effect size scale)") +
    ylab("Absolute Correlation with Outcome (rho)") + ggtitle("Pvalue contours") +
    geom_text_contour(stroke=.2) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[1]),
                 color = color[[1]][1], breaks=pvals[1]) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[2]),
                 color = color[[1]][1], breaks=pvals[2]) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[3]),
                 color = color[[1]][1], breaks=pvals[3]) +
    annotation_custom(grob = textGrob(label = raw, vjust = 3,
                                      gp = gpar(cex = .75)),
                      ymin = 0, ymax = 0, xmax = 0) +
    scale_linetype_manual(name = 'P-value Threshold', values = pval_lines,
                          labels = pvals) +
    geom_point(data = obs_cors, col=color[[1]][2],
               aes(x = ES, y = Cor_Outcome, z = NULL)) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.key = element_blank(), legend.text = element_text(size = 10),
          legend.key.size =  unit(0.5, "in")) +
    geom_text_repel(data=obs_cors,
                    aes(x = ES, y = Cor_Outcome, z = NULL, label = cov),
                    box.padding = unit(0.45, "lines"), col=color[[1]][2])
  if(col=="bw"){
    v2 = v2 + theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                                 legend.key = element_blank(), legend.text = element_text(size = 10),
                                 legend.key.size =  unit(0.5, "in"))
  }

  return(v2)
}

