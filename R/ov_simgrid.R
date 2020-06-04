#### ov_simgrid fn ####
ov_simgrid <- function(model_results, weight_covariates, es_grid=NULL,
                       rho_grid = NULL, n_reps = 50){
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
  if(max(table(data[,y])) > 1) warning("Ties in the outcome variable `y` may be problematic.")

  # determine reasonable grid to simulate over
  if(is.null(es_grid) | is.null(rho_grid)){
    jdp_test=find_esgrid(my_data = data, my_cov = cov, treatment = tx, outcome = y, my_estimand = estimand)
    if(is.null(es_grid)){
      es_upper = round(max(jdp_test$ES) + 5*10^(-1-1), 2)
      es_lower = -es_upper
      es_grid = seq(es_lower, es_upper, by=0.05)
    }
    if(is.null(rho_grid) | (max(rho_grid) < max(jdp_test$Cor_Outcome))){
      rho_upper = round(max(jdp_test$Cor_Outcome) + 5*10^(-1-1), 2)
      rho_grid = seq(0, rho_upper, by=0.05)
    }
  }

  # first generate b1
  b1_low_high = data.frame(expand.grid(es=es_grid, rho=rho_grid)) %>%
    dplyr::mutate(b1_low = NA,
                  b1_high = NA) %>%
    data.frame()
  for(i in 1:nrow(b1_low_high)){
    b1_low_high[i,3:4] = gen_b1(y=data[,y], tx = data[,tx],
                                es = b1_low_high$es[i], rho = b1_low_high$rho[i])
  }
  b1_low = max(b1_low_high$b1_low)
  b1_high = min(b1_low_high$b1_high)
  # for now, take mean of b1low, b1high.
  b1_final = mean(c(b1_low, b1_high))

  trt_effect_nodr <- matrix(0,length(es_grid),length(rho_grid))
  p_val_nodr <- matrix(0,length(es_grid),length(rho_grid))
  pValHd <- esHd <- StdError <- rep(NA, n_reps)
  # create w_new and set it to the original weights for now
  data$w_new = data$w_orig

  for(i in 1:length(es_grid)){
    for(j in 1:length(rho_grid)){
      for(k in 1:n_reps){
        if(k == 1){
          a_prep <- gen_a_start(y=data[,y], tx = data[,tx], es = es_grid[i], rho = rho_grid[j],
                                b1 = b1_final)
        }
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
    if(length(es_grid) >1){
      print(paste0(round(i/length(es_grid)*100,0), "% Done!"))
    }
  }
  results = list(p_val = p_val_nodr, trt_effect = trt_effect_nodr,
                 es_grid = es_grid, rho_grid = rho_grid, cov = cov,
                 data = data, tx = tx, y = y, estimand = estimand)
  class(results) <- "ov"
  return(results)
}
