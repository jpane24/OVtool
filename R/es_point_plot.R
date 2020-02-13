#### Treatment effect plot with points ####
es_point_plotjp = function(prep, col){
  r1_df = prep$r1_df
  pvals = prep$pvals
  pval_lines = prep$pval_lines
  text_high = prep$text_high
  obs_cors = prep$obs_cors

  # Black and white or color
  color=rep(NA,2)
  color = ifelse(col == "color", list(c("red", "blue")),
                 ifelse(col == "bw", list(c("grey70", "black")),
                        stop("Please specify 'bw' or 'color'.")))

  # obs_cors = obs_cors %>%
  #   mutate(cov = case_when(cov == "ada_0" ~ "Days abstinent (Baseline)",
  #                               cov == "recov_0" ~ "In recovery (Baseline)",
  #                               cov == "tss_0" ~ "Traumatic stress scale (Baseline)",
  #                               cov == "eps7p_0" ~ "Emotional problem scale (Baseline)",
  #                               cov == "gender" ~ "Gender",
  #                               cov == "age" ~ "Age",
  #                               cov == "race" ~ "Race",
  #                          cov == "sati_0" ~ "Substance abuse tx index (Baseline)")) %>%
  #   filter(!is.na(cov)) %>%
  #   data.frame()
  raw = prep$raw

  v3 <- ggplot(r1_df, aes(es_grid, rho_grid, z = trt_effect)) +
    # xlim(0,max(r1_df$es_grid)) +
    ylim(0,max(r1_df$rho_grid)) +
    geom_contour(col='black') + xlab("Association between unobserved confounder and treatment indicator\n(effect size scale)") +
    ylab("Absolute Correlation with Outcome (rho)") + ggtitle("ES contours") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.key = element_blank(), legend.text = element_text(size = 10),
          legend.key.size =  unit(0.5, "in")) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[1]),
                 color = color[[1]][1], breaks=pvals[1], size=1.25) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[2]),
                 color = color[[1]][1], breaks=pvals[2], size=1.25) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype=pval_lines[3]),
                 color = color[[1]][1], breaks=pvals[3], size=1.25) +
    scale_linetype_manual(name = 'P-value Threshold', values = pval_lines,
                          labels = pvals) +
    geom_point(data = obs_cors, col=color[[1]][2],
               aes(x = ES, y = Cor_Outcome, z = NULL)) +
    geom_text_contour(aes(z=trt_effect), stroke=.2, check_overlap = T) +
    annotation_custom(grob = textGrob(label = raw, vjust = 3,
                                      gp = gpar(cex = .75)),
                      ymin = 0, ymax = 0, xmax = 0)  +
    geom_text_repel(data = obs_cors,
                    aes(x = ES, y = Cor_Outcome, z = NULL, label = cov),
                    box.padding = unit(0.45, "lines"), col=color[[1]][2])
  if(col == "bw"){
    v3 = v3 + theme_bw() + theme(plot.title = element_text(hjust = 0.5),
    legend.key = element_blank(), legend.text = element_text(size = 10),
    legend.key.size =  unit(0.5, "in"))
  }
  return(v3)
}

