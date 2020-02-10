#### sim_grid fn ####
sim_grid <- function(ps_object, stop.method, data, weights,
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
    if(missing(stop.method) | missing(outcome) | missing(covariates)){
      stop("Please supply a stop.method to generate the weights (e.g. \"ks.max/")
    }
    # outcome
    y = outcome
    # treatment
    tx = ps_object$treat.var
    # covariates
    cov = all.vars(covariates)
    # data
    data = ps_object$data
    # weights
    data$w_orig = get.weights(ps_object, stop.method = stop.method, estimand = estimand)
    # survey design
    design_u <- svydesign(ids=~1, weights=~w_orig, data=data)
  }
  # create formula
  formula = formula(paste(outcome,  "~", tx))
  #formula_scaled = formula(paste0("scale(", outcome, ") ~ ", tx))
  formula_scaled = formula(paste0("scale(", outcome, ")", covariates, " + ", tx))

  # checks
  if(!all(data[,tx] %in% c(0,1))) stop("Treatment variable `tx` must be only 0/1 values.")
  if(max(table(data[,y])) > 1) warning("Ties in the outcome variable `y` may be problematic.")

  # pre-specify rho grid
  if(missing(rho_grid)){
    rho_grid = seq(0, .45, by=0.05)
  }

  # determine reasonable effect size grid
  if(missing(es_grid)){
    jdp_test=find_esgrid(my_data = data, my_cov = cov, treatment = tx, my_estimand = estimand)
    es_upper = ceiling_dec(max(jdp_test$ES))
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
        glm0_u_nodr <- svyglm(formula_scaled, design=design_u)
        esHd[k] <- summary(glm0_u_nodr)$coefficients[tx,1]
        pValHd[k] <- summary(glm0_u_nodr)$coefficients[tx,4]
      }
      p_val_nodr[i,j] <- median(pValHd)
      trt_effect_nodr[i,j] <- median(esHd)
    }
    print(paste0(round(i/length(es_grid)*100,0), "% Done!"))
  }
  results = list(p_val = p_val_nodr, trt_effect = trt_effect_nodr,
                 es_grid = es_grid, rho_grid = rho_grid, cov = cov,
                 data = data, tx = tx, y = y, estimand = estimand)
  class(results) <- "ov"
  return(results)
}
#### ceiling_dec ####
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

#### gen_a_start fn ####
gen_a_start <- function(y, tx, es, rho){
  ind <- which(tx == 1)
  if(length(unique(y[ind])) <= 2){
    y[ind][which(y[ind]==1)] = runif(length(which(y[ind]==1)), min=1, max=2)
    y[ind][which(y[ind]==0)] = runif(length(which(y[ind]==0)), min=-1, max=0)

    y[-ind][which(y[-ind]==1)] = runif(length(which(y[-ind]==1)), min=1, max=2)
    y[-ind][which(y[-ind]==0)] = runif(length(which(y[-ind]==0)), min=-1, max=0)
  }

  cdf1 <- ecdf(y[ind])
  cdf0 <- ecdf(y[-ind])
  ystar1 <- qnorm(cdf1(y[ind]))
  ystar1 <- ifelse(ystar1==Inf, max(ystar1[which(ystar1 < Inf)]), ystar1)
  ystar0 <- qnorm(cdf0(y[which(tx==0)]))
  ystar0 <- ifelse(ystar0==Inf, max(ystar0[which(ystar0 < Inf)]), ystar0)
  n1 <- sum(tx)
  n0 <- sum(1-tx)
  n<- n1 + n0
  c1 <- cov(ystar1,y[ind])
  c0 <- cov(ystar0,y[-ind])

  pi <- mean(tx)

  v1 <- var(y[ind])
  v0 <- var(y[-ind])

  sd1 <- sqrt(v1)
  sd0 <- sqrt(v0)

  vU <- 1 + es^2*pi*(1-pi)

  Y <- y - mean(y)

  vY <- var(y)

  b1lim <- sd1/c1
  b0lim <- sd0/c0

  A <- rho * sqrt(vU * vY)

  Q <- es*(1-pi)*mean(y[ind])*pi - es*pi*mean(y[-ind])*(1-pi)

  #added by Joe, take b1
  alpha = (A - Q)/((1-pi)*c0)
  beta = (-c1*pi)/((1-pi)*c0)
  b1low <- max(-b1lim, ((-b0lim - alpha) / beta))
  b1high <- min(b1lim, ((b0lim - alpha) / beta))
  b1low_final = case_when(b1low > b1high ~ 0, TRUE ~ b1low)
  b1high_final = case_when(b1low > b1high ~ 0, TRUE ~ b1high)
  # #ensure less than abs of 1
  # b1low_final = case_when(b1low_final < -1 ~ -1,
  #                         TRUE ~ b1low_final)
  # b1high_final = case_when(b1high_final > 1 ~ 1,
  #                          TRUE ~ b1high_final)

  b1 = runif(1, min = b1low_final, max = b1high_final);

  b0 <- (A-b1*c1*pi - Q)/((1-pi)*c0)

  if(!(abs(b0) <= b0lim)) stop("b0 is too large in absolute value. Try reducing the size of the grid.")

  ve1 <- 1 - b1^2 * var(ystar1)
  ve0 <- 1 - b0^2 * var(ystar0)
  if(ve0 < 0) stop("b0 is too large in absolute value. Try reducing the size of the grid.")
  #print(c(b0, b1))

  return(a_res = list(n1 = n1, ve1 = ve1, b1 = b1, ystar1 = ystar1,
                      es = es, pi = pi, n0 = n0, ve0 = ve0, b0 = b0,
                      ystar0 = ystar0, n = n, ind = ind))
}

#### gen_a_finish fn ####
gen_a_finish <- function(a_res){

  U1 <- rnorm(a_res$n1)*sqrt(a_res$ve1) + a_res$b1*a_res$ystar1 +
    a_res$es*(1-a_res$pi)
  U0 <- rnorm(a_res$n0)*sqrt(a_res$ve0) + a_res$b0*a_res$ystar0 -
    a_res$es*a_res$pi

  U <- rep(NA, a_res$n)
  ind <- a_res$ind
  U[ind] <- U1
  U[-ind] <- U0

  a <- a_res$pi*(1+exp(-1 * a_res$es *U))
  return(a)
}


#### summarize fn ####
summarize <- function(r1){

  r1_jp$es_SML = case_when(abs(r1_jp$es_grid) < .01 ~ "es Small",
                           abs(r1_jp$es_grid) <= .1 & abs(r1_jp$es_grid) > .01 ~ "es Medium",
                           abs(r1_jp$es_grid) > .1 ~ "es Large")
  r1_jp$rho_SML = case_when(abs(r1_jp$rho_grid) < .2 ~ "rho Small",
                            abs(r1_jp$rho_grid) < .45 & abs(r1_jp$rho_grid) >=.2 ~ "rho Medium",
                            abs(r1_jp$rho_grid) >= .45 ~ "rho Large")

}

#### setup es grid ####
find_esgrid = function(my_data = data, my_cov = cov, treatment = tx, my_estimand = estimand){
  # y-axis of plot (correlation between covariates and treatment)
  obs_cors = rep(NA, length(my_data[,my_cov]))
  for(i in 1:length(obs_cors)){
    if(is.factor(my_data[,my_cov[i]])){
      obs_cors[i] = abs(cor(as.numeric(my_data[,my_cov[i]]),
                            my_data[,treatment],"pairwise.complete.obs"))
    } else {
      obs_cors[i] = abs(cor(as.numeric(as.character(my_data[,my_cov[i]])),
                            my_data[,treatment],"pairwise.complete.obs"))
    }
  }

  # x-axis of plot (effect size)
  mean_noNA = function(x){return(mean(x, na.rm=T))}
  sd_noNA = function(x){return(sd(x, na.rm=T))}
  mean_sd_bygroup = my_data %>%
    select(.data[[treatment]], my_cov) %>%
    mutate_if(is.factor, as.numeric) %>%
    group_by(.data[[treatment]]) %>%
    # adding in so if there are factors, we should make numeric.
    summarize_all(list(mean_noNA, sd_noNA)) %>%
    data.frame()

  es_cov = rep(NA, length(my_cov))
  if(my_estimand == "ATE"){
    for(i in 1:length(my_cov)){
      # denominator for ATE
      diff_means = diff(mean_sd_bygroup[,colnames(mean_sd_bygroup)[grep(paste0("^", my_cov[i], "_fn1$"), colnames(mean_sd_bygroup))]])
      denom_ATE = sqrt(sum(mean_sd_bygroup[,colnames(mean_sd_bygroup)[grep(paste0("^", my_cov[i], "_fn2$"), colnames(mean_sd_bygroup))]]^2)/2)
      es_cov[i] = abs(diff_means/denom_ATE)
    }
  } else if(my_estimand == "ATT"){
    for(i in 1:length(my_cov)){
      diff_means = diff(mean_sd_bygroup[,colnames(mean_sd_bygroup)[grep(paste0("^", my_cov[i], "_fn1$"), colnames(mean_sd_bygroup))]])
      treat_only = mean_sd_bygroup %>% filter(.data[[treatment]] == 1) %>% data.frame()
      denom_ATT = treat_only[,colnames(treat_only)[grep(paste0("^", my_cov[i], "_fn2$"),
                                                        colnames(treat_only))]]
      es_cov[i] = abs(diff_means/denom_ATT)
    }
  }
  obs_cors = cbind(obs_cors, es_cov)
  obs_cors = obs_cors %>%
    data.frame() %>%
    mutate(cov=my_cov)
  colnames(obs_cors) = c("Cor_Outcome", "ES", "cov")

  return(obs_cors)
}

#### prep_for_plots ####
prep_for_plots <- function(r1){
  #### setup for plotting ####
  r1_df = data.frame(matrix(nrow=length(r1$rho_grid)*length(r1$es_grid), ncol=4))
  colnames(r1_df) = c("es_grid", "rho_grid", "trt_effect", "p_val")
  # es_grid
  r1_df$es_grid = rep(r1$es_grid, length(r1$rho_grid))

  # rho_grid
  rho_grid_df = c()
  for(i in 1:length(r1$rho_grid)){rho_grid_df = c(rho_grid_df, rep(r1$rho_grid[i], length(r1$es_grid)))};
  r1_df$rho_grid = rho_grid_df

  # trt_effect
  r1_df$trt_effect = as.vector(r1$trt_effect)
  r1_df$trt_effect_plot = round(r1_df$trt_effect,2)

  # p_val
  r1_df$p_val = as.vector(r1$p_val)

  # max p-val -- get pvalues for plots:
  max_pval = max(r1_df$p_val)

  pvals = case_when(max_pval >= .10 ~ c(.05, .01, .1),
                    max_pval < .1 & max_pval >= .05 ~ c(.05, .01, NA),
                    max_pval < .05 & max_pval >= .01 ~ c(.01, .001, NA),
                    max_pval < .01 ~ c(.001, NA, NA)); pvals = pvals[complete.cases(pvals)]
  pvals = sort(pvals)

  pval_lines = case_when(max_pval >= .10 ~ c("dotdash", "dotted", "solid"),
                         max_pval < .1 & max_pval >= .05 ~ c('dotdash', 'dotted', NA),
                         max_pval < .05 & max_pval >= .01 ~ c('dotdash', 'dotted', NA),
                         max_pval < .01 ~ c('dotdash', NA, NA));

  #### make variable names shorter ####
  RHS_short = case_when(nchar(r1$cov) > 10 ~ substr(r1$cov, start = 1, stop = 10),
                        TRUE ~ r1$cov)

  # raw treatment effect
  raw_treat = r1$trt_effect[which(r1$es_grid<.00001 & r1$es_grid>-.00001),
                            which(r1$rho_grid==0)]
  raw_treat_txt = sprintf("%.3f", round(raw_treat,3))
  raw_pval = r1$p_val[which(r1$es_grid<.00001 & r1$es_grid>-.00001),
                      which(r1$rho_grid==0)];
  raw_pval_txt = sprintf("%.3f", round(raw_pval,3))
  raw = paste0("Raw Effect: ", raw_treat_txt, " (p-val: ", raw_pval_txt, ")")

  #### setup correlations ####
  # y-axis of plot (correlation between covariates and treatment)
  obs_cors = rep(NA, length(r1$data[,r1$cov]))
  for(i in 1:length(obs_cors)){
    obs_cors[i] = abs(cor(as.numeric(r1$data[,r1$cov[i]]),
                          r1$data[,r1$tx],"pairwise.complete.obs"))
  }

  # x-axis of plot (effect size)
  mean_noNA = function(x){return(mean(x, na.rm=T))}
  sd_noNA = function(x){return(sd(x, na.rm=T))}
  mean_sd_bygroup = r1$data %>%
    select(.data[[r1$tx]], r1$cov) %>%
    mutate_if(is.factor, as.numeric) %>%
    group_by(.data[[r1$tx]]) %>%
    # adding in so if there are factors, we should make numeric.
    summarize_all(list(mean_noNA, sd_noNA)) %>%
    data.frame()

  es_cov = rep(NA, length(r1$cov))
  if(r1$estimand == "ATE"){
    for(i in 1:length(r1$cov)){
      # denominator for ATE
      diff_means = diff(mean_sd_bygroup[,colnames(mean_sd_bygroup)[grep(paste0("^", r1$cov[i], "_fn1$"), colnames(mean_sd_bygroup))]])
      denom_ATE = sqrt(sum(mean_sd_bygroup[,colnames(mean_sd_bygroup)[grep(paste0("^", r1$cov[i], "_fn2$"), colnames(mean_sd_bygroup))]]^2)/2)
      if(raw_pval >= .05){
        es_cov[i] = (diff_means/denom_ATE)
      } else if(raw_pval < .05 & length(which(r1$p_val[r1$es_grid>0,]>.05)) >
                length(which(r1$p_val[r1$es_grid<0,]>.05))){
        es_cov[i] = abs(diff_means/denom_ATE)
      } else{
        es_cov[i] = -1*abs(diff_means/denom_ATE)
      }
    }
  } else if(r1$estimand == "ATT"){
    for(i in 1:length(r1$cov)){
      diff_means = diff(mean_sd_bygroup[,colnames(mean_sd_bygroup)[grep(paste0("^", r1$cov[i], "_fn1$"), colnames(mean_sd_bygroup))]])
      treat_only = mean_sd_bygroup %>% filter(.data[[r1$tx]] == 1) %>% data.frame()
      denom_ATT = treat_only[,colnames(treat_only)[grep(paste0("^", r1$cov[i], "_fn2$"),
                                                        colnames(treat_only))]]
      if(raw_pval >= .05){
        es_cov[i] = (diff_means/denom_ATT)
      } else if(raw_pval < .05 & length(which(r1$p_val[r1$es_grid>0,]>.05)) >
                length(which(r1$p_val[r1$es_grid<0,]>.05))){
        es_cov[i] = abs(diff_means/denom_ATT)
      } else{
        es_cov[i] = -1*abs(diff_means/denom_ATT)
      }
    }
  }

  obs_cors = cbind(obs_cors, es_cov)
  obs_cors = obs_cors %>%
    data.frame() %>%
    mutate(cov=r1$cov)
  colnames(obs_cors) = c("Cor_Outcome", "ES", "cov")

  cor_high = obs_cors[obs_cors$Cor_Outcome>.45,]
  if(nrow(cor_high)!=0){
    text_high = paste0("Covariates with absolute correlation greater than .45: ",
                       cor_high$cov, " (Actual: ", sprintf("%.3f", round(cor_high$Cor_Outcome,3)), ")")
  } else{
    text_high=""
  }

  obs_cors = obs_cors %>%
    mutate(Cor_Outcome = case_when(Cor_Outcome >.45 ~ .45, TRUE ~ Cor_Outcome))

  es_high = obs_cors[abs(obs_cors$ES)>max(r1$es_grid),]
  if(nrow(es_high)!=0){
    text_high = paste0("Covariates with effect size greater than max plot allows: ",
                       es_high$cov, " (Actual: ", sprintf("%.3f", round(es_high$ES,3)), ")")
  } else{
    text_high=""
  }

  obs_cors = obs_cors %>%
    mutate(ES = case_when(abs(ES) > max(r1$es_grid) ~ max(r1$es_grid)*(ES/abs(ES)), TRUE ~ ES),
           # temporary line of code
           cov = gsub('_.*','',cov))

  return(list(r1=r1, r1_df=r1_df, obs_cors=obs_cors, text_high=text_high,
              pvals=pvals, pval_lines=pval_lines, raw = raw))

}

#### Effect Size plot (no points) ####
es_plot = function(prep, col){
  r1_df = prep$r1_df
  pvals = prep$pvals
  pval_lines = prep$pval_lines
  raw = prep$raw

  # Black and white or color
  color = ifelse(col == "color", list(c("red", "blue")),
                 ifelse(col == "bw", list(c("grey70", "black")),
                        stop("Please specify 'bw' or 'color'.")))

  ## Parallel coordinates plot of cluster means
  #### ES with p-value overlay (no points) ####
  v <- ggplot(r1_df, aes(es_grid, rho_grid, z = trt_effect)) +
    ylim(0,max(r1_df$rho_grid)) +
    geom_contour(col="black") + xlab("Association between unobserved confounder and treatment indicator\n (effect size scale)") +
    ylab("Absolute Correlation with Outcome (rho)") + ggtitle("ES contours") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.key = element_blank(), legend.text = element_text(size = 10),
          legend.key.size =  unit(0.5, "in")) +
    scale_linetype_manual(name = 'P-value Threshold', values = pval_lines,
                          labels = pvals) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[1]),
                 color = color[[1]][1], breaks=pvals[1]) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[2]),
                 color = color[[1]][1], breaks=pvals[2]) +
    geom_contour(data = r1_df, aes(x = es_grid, y = rho_grid, z = p_val, linetype= pval_lines[3]),
                 color = color[[1]][1], breaks=pvals[3]) +
    geom_text_contour(stroke=.2) +
    annotation_custom(grob = textGrob(label = raw, vjust = 3,
                                      gp = gpar(cex = .75)),
                      ymin = 0, ymax = 0, xmin = 0, xmax = 0)
  if(col == "bw"){
    v = v + theme_bw()
  }
  return(v)
}

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
    annotation_custom(grob = textGrob(label = text_high, vjust = -1.5,
                                      gp = gpar(cex = .75)),
                      ymin = max(r1_df$rho_grid), ymax = max(r1_df$rho_grid), xmin = 0,
                      xmax = 0)  +
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
    v2 = v2 + theme_bw()
  }

  return(v2)
}

#### Treatment effect plot with points ####
es_point_plot = function(prep, col){
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
    annotation_custom(grob = textGrob(label = text_high, vjust = -1.5,
                                      gp = gpar(cex = .75)),
                      ymin = .45, ymax = .45, xmin = 0,
                      xmax = 0) +
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
    v3 = v3 + theme_bw()
  }
  return(v3)
}

#### Plotting Package for OV ####
plot.ov <- function(x, col="color", print_all="all") {
  prep_plots = prep_for_plots(x)

  #### some defaults ####
  .pardefault <- par(no.readonly=TRUE)

  par(ask = TRUE)

  plot_1 = es_plot(prep=prep_plots, col=col)
  plot_2 = pval_point_plot(prep=prep_plots, col=col)
  plot_3 = es_point_plot(prep=prep_plots,col=col)
  if(print_all == "all"){
    print(plot_1)
    print(plot_2)
    print(plot_3)
  }
  else if(print_all == "1"){
    par(.pardefault)
    print(plot_1)
  } else if(print_all == "2"){
    par(.pardefault)
    print(plot_2)
  } else if(print_all == "3"){
    par(.pardefault)
    print(plot_3)
  }
  par(.pardefault)
  grDevices::palette("default")
}
#### end of file ####
