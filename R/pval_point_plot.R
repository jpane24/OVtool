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
  redregion = ifelse(col == "color", "white","gray85") #indianred2

  ## Pvalue
  v2 <- ggplot2::ggplot(r1_df, ggplot2::aes(x=es_grid, y=rho_grid, z = p_val)) +
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
    ggplot2::scale_y_continuous(limits = c(0,max(c(obs_cors$Cor_Outcome_Actual,
                                                   r1_df$rho_grid))),expand = c(0, 0)) +
    ggplot2::geom_contour(col='black') + ggplot2::xlab("Association between unobserved confounder and treatment indicator\n(effect size scale)") +
    ggplot2::ylab("Absolute Correlation with Outcome (rho)") + ggplot2::ggtitle("Pvalue contours") +
    metR::geom_text_contour(stroke=.2) +
    ggplot2::geom_contour(data = r1_df, ggplot2::aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[1]),
                 color = color[[1]][1], breaks=pvals[1]) +
    ggplot2::geom_contour(data = r1_df, ggplot2::aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[2]),
                 color = color[[1]][1], breaks=pvals[2]) +
    ggplot2::geom_contour(data = r1_df, ggplot2::aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[3]),
                 color = color[[1]][1], breaks=pvals[3]) +
    ggplot2::annotation_custom(grob = grid::textGrob(label = raw, vjust = 3,
                                                     gp = grid::gpar(cex = .75)),
                               ymin = .1, ymax = .1, xmax = max(r1_df$es_grid)+.1, xmin=max(r1_df$es_grid)+.05)  +
    ggplot2::scale_linetype_manual(name = 'P-value Threshold', values = pval_lines,
                          labels = pvals) +
    ggplot2::geom_point(data = obs_cors, col=color[[1]][2],
                        ggplot2::aes(x = ES, y = Cor_Outcome_Actual, z = NULL)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
          legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10),
          legend.key.size =  grid::unit(0.5, "in")) +
    ggrepel::geom_text_repel(data=obs_cors,
                    ggplot2::aes(x = ES, y = Cor_Outcome_Actual, z = NULL, label = cov),
                    box.padding = grid::unit(0.45, "lines"), col=color[[1]][2]) +
    ggplot2::theme_linedraw() +
    ggplot2::geom_hline(yintercept=max(rho_grid))

  if(col=="bw"){
    v2 = v2 + ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                 legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10),
                                 legend.key.size =  grid::unit(0.5, "in"))
  }
  return(v2)
}

