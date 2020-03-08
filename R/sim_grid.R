#### ov_simgrid fn ####
ov_simgrid <- function(ps_object=NULL, stop.method, data, weights,
                     treatment, outcome, covariates,
                     es_grid, rho_grid,
                     n_reps = 101, estimand = "ATE", ...){
  set.seed(24)
  if(class(ps_object)!="ps"){
    if(missing(data) | missing(weights) | missing(treatment) | missing(outcome) | missing(covariates)){
      stop("Please supply either a ps class object, stop.method, and relevant column names for:
      outcome, and covariates OR the data and relevant column names for: weights, treatment, outcome, & covariates")
    } else{
      # weights
      data$w_orig = weights
      # outcome
      y = outcome
      # treamtment
      tx = treatment
      # covariates
      cov = all.vars(covariates)
      # survey design
      design_u <- svydesign(ids=~1, weights=~w_orig, data=data)
    }
  } else{
    if(is.null(stop.method) | is.null(outcome) | is.null(covariates)){
      stop("Please supply a stop.method to generate the weights (e.g. \"ks.max/")
    }
    # outcome
    y = outcome
    # treatment
    tx = ps_object$treat.var
    # covariates
    if(typeof(covariates) == "language"){
      cov = all.vars(covariates)
    }
    # data
    data = ps_object$data
    # weights
    data$w_orig = get.weights(ps_object, stop.method = stop.method, estimand = estimand)
    # survey design
    design_u <- svydesign(ids=~1, weights=~w_orig, data=data)
  }
  # create formula
  # formula = formula(paste(outcome,  "~", tx))
  formula = formula(paste0(outcome, " ~ ", glue_collapse(cov, sep=" + "), " + ", tx))

  # checks
  if(!all(data[,tx] %in% c(0,1))) stop("Treatment variable `tx` must be only 0/1 values.")
  if(max(table(data[,y])) > 1) warning("Ties in the outcome variable `y` may be problematic.")

  # pre-specify rho grid
  if(missing(rho_grid)){
    rho_grid = seq(0, .45, by=0.05)
  }

  # determine reasonable effect size grid
  if(missing(es_grid)){
    jdp_test=find_esgrid(my_data = data, my_cov = cov, treatment = tx, outcome = y, my_estimand = estimand)
    es_upper = round(max(jdp_test$ES) + 5*10^(-1-1), 1)
    es_lower = -es_upper
    es_grid = seq(es_lower, es_upper, by=0.1)
  }

  trt_effect_nodr <- matrix(0,length(es_grid),length(rho_grid))
  p_val_nodr <- matrix(0,length(es_grid),length(rho_grid))
  pValHd <- esHd <- rep(NA, n_reps)
  # create w_new and set it to the original weights for now
  data$w_new = data$w_orig
  # my_grid = expand.grid(rho_grid, es_grid)
  # my_grid = my_grid %>% slice(rep(1:n(), each = n_reps)) %>%
  #   rename(es = Var2,
  #          rho = Var1) %>%
  #   mutate(esHd = NA_real_, pValHd = NA_real_) %>%
  #   head()

  # jdp = apply(my_grid, MARGIN=1, FUN=get_new_weights)

  for(i in 1:length(es_grid)){
    for(j in 1:length(rho_grid)){
      for(k in 1:n_reps){
        a_prep <- gen_a_start(y=data[,y], tx = data[,tx], es = es_grid[i], rho = rho_grid[j])
        a <- gen_a_finish(a_prep)
        data$w_new <- data$w_orig * a
        design_u <- svydesign(ids=~1, weights=~w_new, data=data)
        glm0_u_nodr <- svyglm(formula, design=design_u)
        esHd[k] <- summary(glm0_u_nodr)$coefficients[tx,1]
        pValHd[k] <- summary(glm0_u_nodr)$coefficients[tx,4]
      }
      p_val_nodr[i,j] <- median(pValHd)
      trt_effect_nodr[i,j] <- median(esHd)
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
