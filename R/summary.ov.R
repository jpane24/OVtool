#### summarize fn ####
summary.ov <- function(r1, sig_level=0.05){
  temp = prep_for_plots(r1)
  raw_treat = r1$trt_effect[which(r1$es_grid<.000000001 & r1$es_grid>-.000000001),
                            which(r1$rho_grid==0)]
  raw_pval = r1$p_val[which(r1$es_grid<.000000001 & r1$es_grid>-.000000001),
                      which(r1$rho_grid==0)]
  # Effect Size cases
  effect_size_text = case_when((raw_treat < 0 & all(temp$obs_cors$ESTRUE < 0)) |
                                 raw_treat >= 0 & all(temp$obs_cors$ESTRUE >= 0) ~ "no sign changes",
                               raw_treat < 0 & all(temp$obs_cors$ESTRUE >= 0) |
                                 raw_treat >=0 & all(temp$obs_cors$ESTRUE < 0) ~ "all sign changes",
                               TRUE ~ "some sign changes")

  if(effect_size_text == "no sign changes"){
    diff = diff(c(min(abs(temp$obs_cors$ESTRUE)), abs(raw_treat)))
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is expected to be robust to unobserved confounders that have the same strength of association with the treatment indicator and outcome that are seen in the observed confounders. In the most extreme observed case, the estimated effect size is reduced by ", percent_reduced, " percent.")
  } else if(effect_size_text == "all sign changes"){
    if(raw_treat < 0){
      diff = diff(c(max(temp$obs_cors$ESTRUE), raw_treat))
    } else{
      diff = diff(c(min(temp$obs_cors$ESTRUE), raw_treat))
    }
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is *not* expected to be robust to unobserved confounders that have the same strength of association with the treatment indicator and outcome that are seen in any of the observed confounders. In the most extreme observed case, the estimated effect size is ", percent_reduced, " percent of the original, but in the opposite direction")
  } else if(effect_size_text == "some sign changes"){
    # calculate sign changes
    if(raw_treat < 0){
      change = length(which(temp$obs_cors$ESTRUE >= 0))
      nochange = length(which(temp$obs_cors$ESTRUE < 0))
      total = nrow(temp$obs_cors)
      changes = temp$obs_cors$cov[which(temp$obs_cors$ESTRUE >=0)]
    } else{
      change = length(which(temp$obs_cors$ESTRUE < 0))
      nochange = length(which(temp$obs_cors$ESTRUE >= 0))
      total = nrow(temp$obs_cors)
      changes = temp$obs_cors$cov[which(temp$obs_cors$ESTRUE < 0)]
    }
    if(raw_treat < 0){
      diff = diff(c(max(temp$obs_cors$ESTRUE), raw_treat))
      mostextreme = temp$obs_cors$cov[which(temp$obs_cors$ESTRUE == max(temp$obs_cors$ESTRUE))]
    } else{
      diff = diff(c(min(temp$obs_cors$ESTRUE), raw_treat))
      mostextreme = temp$obs_cors$cov[which(temp$obs_cors$ESTRUE == min(temp$obs_cors$ESTRUE))]
    }
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is expected to be robust to unobserved confounders with strengths of associations with the treatment indicator and outcome that are seen in ", nochange, " of the ", total, " observed confounders. In the most extreme observed case, ", mostextreme, ", the estimated effect size is ", percent_reduced, " percent of the original, but in the opposite direction. The sign of the estimate would not be expected to be preserved for unobserved confounders that have the same strength of association with the treatment indicator and outcome as ", paste(changes, collapse=", "), ".")
  }

  # Pvalue Size cases
  pvals=rep(NA, nrow(temp$obs_cors))
  for(i in 1:nrow(temp$obs_cors)){
    nearest.idx <- which.min(colSums((t(temp$r1_df[,c("es_grid", "rho_grid")]) - c(temp$obs_cors$ESTRUE[i] , temp$obs_cors$Cor_Outcome[i]))^2))
    pvals[i] = temp$r1_df$p_val[nearest.idx]
  }
  if(raw_pval < sig_level & all(pvals < sig_level)){
    text_p = paste0("Statistical significance at the ", sig_level, " level is expected to be robust to unobserved confounders with strengths of associations with the treatment indicator and outcome that are seen in the observed confounders. In the most extreme observed case, the p-value would be expected to increase from ", format(round(raw_pval,3), nsmall=3), " to ", format(round(max(pvals),3), nsmall=3), ".")
  } else if(raw_pval < sig_level & all(pvals > sig_level)){
    text_p = paste0("Statistical significance at the ", sig_level, " level is *not* expected to be robust to unobserved confounders with strengths of associations with the treatment indicator and outcome that are seen in any of the observed confounders. In the most extreme observed case, the p-value would be expected to increase from  ", format(round(raw_pval,3), nsmall=3), " to ", format(round(max(pvals),3), nsmall=3), ".")
  } else if(raw_pval < sig_level & !all(pvals<sig_level | pvals >sig_level)){
    nonsig = temp$obs_cors$cov[which(pvals>sig_level)]
    total = nrow(temp$obs_cors)
    nonsig_count = length(nonsig)
    text_p = paste0("Statistical significance at the ", sig_level, " level is expected to be robust to unobserved confounders with strengths of associations with the treatment indicator and outcome that are seen in ", nonsig_count, " of the ", total, " observed confounders. In the most extreme observed case, the p-value would be expected to increase from ", format(round(raw_pval,3), nsmall=3), " to ", format(round(max(pvals),3), nsmall=3), ". Significance at the ", sig_level, " level would not be expected to be preserved for unobserved confounders that have the same strength of association with the treatment indicator and outcome as ",  paste(nonsig, collapse=", "),".")
  }
  print("Recommendation for reporting the sensitivity analyses")

  if(raw_pval < 0.05){
    print(text)
    print(text_p)
  } else{
    print(text)
  }
}
