#### ov_sim fn ####
ov_sim <- function(model_results, weight_covariates,
                   es_grid = seq(-.4, .4, by = 0.05),
                   rho_grid = seq(0, .4, by = 0.05), n_reps = 50,
                   progress = TRUE){
  if(n_reps <= 1){"Please specify at least two (2) n_reps. For example: n_reps = 50"}
  if(!is.null(rho_grid)){
    if(any(rho_grid <0) | any(rho_grid > 1)){
      stop("Please specify a rho grid between 0 and 1. For example: rho_grid = seq(0, .45, by = 0.05)")
    }
  }
  if(!is.null(es_grid)){
    if(any(abs(es_grid) > 1)){
      stop("This tool only considers small to moderate correlations. An effect size of 1 corresponds to a correlation of 0.44. Please reduce the size of your grid. For example: rho_grid = seq(-.4, .4, by = 0.05)")
    }
  }

  tx = model_results$tx
  y = model_results$y
  data = model_results$data
  estimand = model_results$estimand

  # covariates
  if(typeof(weight_covariates) == "language"){
    cov = all.vars(weight_covariates)
  } else{
    cov = weight_covariates
  }

  # create formula
  formula = model_results$outcome_mod_fmla

  # checks
  if(!all(data[,tx] %in% c(0,1))) stop("Treatment variable `tx` must be only 0/1 values.")

  # determine reasonable grid to simulate over
  jdp_test=find_esgrid(my_data = data,
                       my_cov = cov,
                       treatment = tx,
                       outcome = y,
                       my_estimand = estimand)
  # SET final es_grid.
  if(!is.null(es_grid)){
    es_drop = jdp_test[which(jdp_test$ES > max(abs(es_grid))),'cov']
    if(max(abs(es_grid)) < max(abs(jdp_test$ES))){
      warning(paste0("You specified an effect size grid whose maximum in absolute value is less than the maximum absolute association at least one observed covariate has with the treatment indicator. The effect size grid was automatically expanded to include all weight_covariates specified in the relevant graphics. If you want the effect size grid range to remain from ", min(es_grid), " to ", max(es_grid), " then you must exclude the following variables from the weight_covariates argument: ", paste(es_drop, collapse=", "), "."))
      es_upper = .05 * ceiling(max(jdp_test$ES)/.05)
      es_lower = -es_upper
      es_grid = seq(es_lower, es_upper, by=0.05)
    }
  } else{
    es_upper = .05 * ceiling(max(jdp_test$ES)/.05)
    es_lower = -es_upper
    es_grid = seq(es_lower, es_upper, by=0.05)
  }

  if(!is.null(rho_grid)){
    rho_drop = jdp_test[which(jdp_test$Cor_Outcome > max(abs(rho_grid))),'cov']
    if(max(rho_grid) < max(jdp_test$Cor_Outcome)){
      warning(paste0("You specified a rho grid whose maximum value is less than the maximum absolute correlation at least one observed covariate has with the outcome. The rho grid was automatically expanded to include all weight_covariates specified in the relevant graphics. If you want the rho grid range to remain from ", min(rho_grid), " to ", max(rho_grid), " then you must exclude the following variables from the weight_covariates argument: ", paste(rho_drop, collapse=", "), "."))
      rho_upper = .05 * ceiling(max(jdp_test$Cor_Outcome)/.05)
      rho_grid = seq(0, rho_upper, by=0.05)
    }
  } else{
    rho_upper = .05 * ceiling(max(jdp_test$Cor_Outcome)/.05)
    rho_grid = seq(0, rho_upper, by=0.05)
  }

  trt_effect_nodr <- matrix(0,length(es_grid),length(rho_grid))
  p_val_nodr <- matrix(0,length(es_grid),length(rho_grid))
  pValHd <- esHd <- StdError <- rep(NA, n_reps)
  # create w_new and set it to the original weights for now
  data$w_new = data$w_orig

  for(i in 1:length(es_grid)){
    for(j in 1:length(rho_grid)){
      for(k in 1:n_reps){
        a_prep <- gen_a_start(y=data[,y], tx = data[,tx],
                              es = es_grid[i], rho = rho_grid[j])
        a <- gen_a_finish(a_prep)
        data$w_new <- data$w_orig * a
        design_u <- survey::svydesign(ids=~1, weights=~w_new, data=data)
        glm0_u_nodr <- survey::svyglm(formula, design=design_u)
        esHd[k] <- summary(glm0_u_nodr)$coefficients[tx,1]
        StdError[k] <- summary(glm0_u_nodr)$coefficients[tx,2]
      }
      combine = Amelia::mi.meld(q=data.frame(esHd), se=data.frame(StdError))
      melded_summary <- as.data.frame(cbind(t(combine$q.mi),
                                            t(combine$se.mi))) %>%
        purrr::set_names(c("estimate", "std.error")) %>%
        dplyr::mutate(term = rownames(.)) %>%
        dplyr::select(term, tidyselect::everything()) %>%
        dplyr::mutate(statistic = estimate / std.error,
               p.value = 2 * pnorm(abs(statistic),lower.tail = FALSE))
      p_val_nodr[i,j] <- melded_summary$p.value
      trt_effect_nodr[i,j] <- melded_summary$estimate
    }
    if((length(es_grid) >1) & progress == TRUE){
      print(paste0(round(i/length(es_grid)*100,0), "% Done!"))
    }
  }

  results = list(p_val = p_val_nodr, trt_effect = trt_effect_nodr,
                 es_grid = es_grid, rho_grid = rho_grid, cov = cov,
                 data = data, tx = tx, y = y, estimand = estimand,
                 n_reps = n_reps)
  class(results) <- "ov"
  return(results)
}
