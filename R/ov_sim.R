#### ov_sim fn ####
ov_sim <- function(model_results, weight_covariates,
                   es_grid = seq(-.4, .4, by = 0.05),
                   rho_grid = seq(0, .4, by = 0.05), n_reps = 50,
                   progress = TRUE){
  if(n_reps <= 1){"Please specify at least two (2) n_reps."}
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
  jdp_test=find_esgrid(my_data = data, my_cov = cov, treatment = tx, outcome = y, my_estimand = estimand)
  if(is.null(es_grid)){
    es_upper = round(max(jdp_test$ES) + 5*10^(-1-1), 2)
    es_lower = -es_upper
    es_grid = seq(es_lower, es_upper, by=0.05)
    }
  if(is.null(rho_grid) | (max(rho_grid) < max(jdp_test$Cor_Outcome))){
    if(length(es_grid) > 1){
      print("Note: The maximum rho value you specified is less than the maximum absolute correlation a covariate has with the outcome. The rho grid was automatically expanded.")
      rho_upper = round(max(jdp_test$Cor_Outcome) + 5*10^(-1-1), 2)
      rho_grid = seq(0, rho_upper, by=0.05)
    }
  }

  trt_effect_nodr <- matrix(0,length(es_grid),length(rho_grid))
  p_val_nodr <- matrix(0,length(es_grid),length(rho_grid))
  pValHd <- esHd <- StdError <- rep(NA, n_reps)
  # create w_new and set it to the original weights for now
  data$w_new = data$w_orig

  for(i in 1:length(es_grid)){
    for(j in 1:length(rho_grid)){
      for(k in 1:n_reps){
        a_prep <- gen_a_start2(y=data[,y], tx = data[,tx],
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
