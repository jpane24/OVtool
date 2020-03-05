#### summarize fn ####
summary.ov <- function(r1){
  temp = prep_for_plots(r1)
  raw_treat = r1$trt_effect[which(r1$es_grid<.000000001 & r1$es_grid>-.000000001),
                            which(r1$rho_grid==0)]
  # Effect Size cases
  effect_size_text = case_when((raw_treat < 0 & all(temp$obs_cors$ES < 0)) |
                                 raw_treat >= 0 & all(temp$obs_cors$ES >= 0) ~ "no sign changes",
                               raw_treat < 0 & all(temp$obs_cors$ES >= 0) |
                                 raw_treat >=0 & all(temp$obs_cors$ES < 0) ~ "all sign changes",
                               TRUE ~ "some sign changes")

  if(effect_size_text == "no sign changes"){
    diff = diff(c(min(abs(temp$obs_cors$ES)), abs(raw_treat)))
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is expected to be robust to unobserved confounders that have the same strength of association with the treatment indicator and outcome that are seen in the observed confounders. In the most extreme observed case, the estimated effect size is reduced by ", percent_reduced, " percent.")
  } else if(effect_size_text == "all sign changes"){
    if(raw_treat < 0){
      diff = diff(c(max(temp$obs_cors$ES), raw_treat))
    } else{
      diff = diff(c(min(temp$obs_cors$ES), raw_treat))
    }
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is *not* expected to be robust to unobserved confounders that have the same strength of association with the treatment indicator and outcome that are seen in any of the observed confounders. In the most extreme observed case, the estimated effect size is ", percent_reduced, " percent of the original, but in the opposite direction")
  } else if(effect_size_text == "some sign changes"){
    # calculate sign changes
    if(raw_treat < 0){
      change = length(which(temp$obs_cors$ES >= 0))
      nochange = length(which(temp$obs_cors$ES < 0))
      total = nrow(temp$obs_cors)
      changes = temp$obs_cors$cov[which(temp$obs_cors$ES >=0)]
    } else{
      change = length(which(temp$obs_cors$ES < 0))
      nochange = length(which(temp$obs_cors$ES >= 0))
      total = nrow(temp$obs_cors)
      changes = temp$obs_cors$cov[which(temp$obs_cors$ES < 0)]
    }
    if(raw_treat < 0){
      diff = diff(c(max(temp$obs_cors$ES), raw_treat))
      mostextreme = temp$obs_cors$cov[which(temp$obs_cors$ES == max(temp$obs_cors$ES))]
    } else{
      diff = diff(c(min(temp$obs_cors$ES), raw_treat))
      mostextreme = temp$obs_cors$cov[which(temp$obs_cors$ES == min(temp$obs_cors$ES))]
    }
    percent_reduced = abs(round(diff/raw_treat*100))
    text = paste0("The sign of the estimated effect is expected to be robust to unobserved confounders with strengths of associations with the treatment indicator and outcome that are seen in ", nochange, " of the ", total, " observed confounders. In the most extreme observed case, ", mostextreme, ", the estimated effect size is ", percent_reduced, " percent of the original, but in the opposite direction. The sign of the estimate would not be expected to be preserved for unobserved confounders that have the same strength of association with the treatment indicator and outcome as ", paste(changes, collapse=", "), ".")
  }
  print(text)
}
