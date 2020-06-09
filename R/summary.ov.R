#### summarize fn ####
summary.ov <- function(OVtool_results, model_results, sig_level=0.05, progress = TRUE){
  temp = prep_for_plots(OVtool_results)
  raw_treat = OVtool_results$trt_effect[which(OVtool_results$es_grid<.000000001 & OVtool_results$es_grid>-.000000001),
                            which(OVtool_results$rho_grid==0)]
  raw_pval = OVtool_results$p_val[which(OVtool_results$es_grid<.000000001 & OVtool_results$es_grid>-.000000001),
                      which(OVtool_results$rho_grid==0)]

  # Find effects and pvalues
  pvals = rep(NA, nrow(temp$obs_cors))
  trt_effect = rep(NA, nrow(temp$obs_cors))
  options(warn=-1)
  for(i in 1:nrow(temp$obs_cors)){
    calculate_exact = ov_sim(model_results = model_results,
                                     weight_covariates = OVtool_results$cov,
                                     rho_grid = temp$obs_cors$Cor_Outcome[i],
                                     es_grid=temp$obs_cors$ES[i],
                                     n_reps = OVtool_results$n_reps)
    trt_effect[i] = calculate_exact$trt_effect[[1]]
    pvals[i] = calculate_exact$p_val[[1]]
    if(progress == TRUE){
      print(paste0(round(i/nrow(temp$obs_cors)*100,0), "% Done!"))
    }
  }
  options(warn=1)

  # Effect Size cases
  effect_size_text = dplyr::case_when((raw_treat < 0 & all(trt_effect < 0)) |
                                 raw_treat >= 0 & all(trt_effect >= 0) ~ "no sign changes",
                               raw_treat < 0 & all(trt_effect >= 0) |
                                 raw_treat >=0 & all(trt_effect < 0) ~ "all sign changes",
                               TRUE ~ "some sign changes")

  if(effect_size_text == "no sign changes"){
    diff = diff(c(min(abs(trt_effect)), abs(raw_treat)))
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is expected to remain consistent when simulated unobserved confounders have the same strength of association with the treatment indicator and outcome that are seen in the observed confounders. In the most extreme observed case, the estimated effect size is reduced by ", percent_reduced, " percent.")
  } else if(effect_size_text == "all sign changes"){
    if(raw_treat < 0){
      diff = diff(c(max(trt_effect), raw_treat))
    } else{
      diff = diff(c(min(trt_effect), raw_treat))
    }
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is *not* expected to be robust to unobserved confounders that have the same strength of association with the treatment indicator and outcome that are seen in any of the observed confounders. In the most extreme observed case, the estimated effect size is ", percent_reduced, " percent of the original, but in the opposite direction")
  } else if(effect_size_text == "some sign changes"){
    # calculate sign changes
    if(raw_treat < 0){
      change = length(which(trt_effect >= 0))
      nochange = length(which(trt_effect < 0))
      total = nrow(temp$obs_cors)
      changes = temp$obs_cors$cov[which(trt_effect >=0)]
    } else{
      change = length(which(trt_effect < 0))
      nochange = length(which(trt_effect >= 0))
      total = nrow(temp$obs_cors)
      changes = temp$obs_cors$cov[which(trt_effect < 0)]
    }
    if(raw_treat < 0){
      diff = diff(c(max(trt_effect), raw_treat))
      mostextreme = temp$obs_cors$cov[which(trt_effect == max(trt_effect))]
    } else{
      diff = diff(c(min(trt_effect), raw_treat))
      mostextreme = temp$obs_cors$cov[which(trt_effect == min(trt_effect))]
    }
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is expected to remain consistent when simulated unobserved confounders have the same strength of associations with the treatment indicator and outcome that are seen in ", nochange, " of the ", total, " observed confounders. In the most extreme observed case, ", mostextreme, ", the estimated effect size is ", percent_reduced, " percent of the original, but in the opposite direction. The sign of the estimate would not be expected to be preserved for unobserved confounders that have the same strength of association with the treatment indicator and outcome as ", paste(changes, collapse=", "), ".")
  }

  # Pvalue Size cases

  if(raw_pval < sig_level & all(pvals < sig_level)){
    text_p = paste0("Statistical significance at the ", sig_level, " level is expected to be robust to unobserved confounders with strengths of associations with the treatment indicator and outcome that are seen in the observed confounders. In the most extreme observed case, the p-value would be expected to increase from ", format(round(raw_pval,3), nsmall=3), " to ", format(round(max(pvals),3), nsmall=3), ".")
  } else if(raw_pval < sig_level & all(pvals > sig_level)){
    text_p = paste0("Statistical significance at the ", sig_level, " level is *not* expected to be robust to unobserved confounders with strengths of associations with the treatment indicator and outcome that are seen in any of the observed confounders. In the most extreme observed case, the p-value would be expected to increase from  ", format(round(raw_pval,3), nsmall=3), " to ", format(round(max(pvals),3), nsmall=3), ".")
  } else if(raw_pval < sig_level & !(all(pvals<sig_level) | all(pvals >sig_level))){
    nonsig = temp$obs_cors$cov[which(pvals>sig_level)]
    total = nrow(temp$obs_cors)
    sig_count = total - length(nonsig)
    text_p = paste0("Statistical significance at the ", sig_level, " level is expected to be robust to unobserved confounders with strengths of associations with the treatment indicator and outcome that are seen in ", sig_count, " of the ", total, " observed confounders. In the most extreme observed case, the p-value would be expected to increase from ", format(round(raw_pval,3), nsmall=3), " to ", format(round(max(pvals),3), nsmall=3), ". Significance at the ", sig_level, " level would not be expected to be preserved for unobserved confounders that have the same strength of association with the treatment indicator and outcome as ",  paste(nonsig, collapse=", "),".")
  }
  print("Recommendation for reporting the sensitivity analyses")

  if(raw_pval < 0.05){
    print(text)
    print(text_p)
  } else{
    print(text)
  }
}
