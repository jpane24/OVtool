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

  pvals = dplyr::case_when(max_pval >= .10 ~ c(.05, .01, .1),
                    max_pval < .1 & max_pval >= .05 ~ c(.05, .01, NA),
                    max_pval < .05 & max_pval >= .01 ~ c(.01, .001, NA),
                    max_pval < .01 ~ c(.001, NA, NA)); pvals = pvals[complete.cases(pvals)]
  pvals = sort(pvals)

  pval_lines = dplyr::case_when(max_pval >= .10 ~ c("dotdash", "dotted", "solid"),
                         max_pval < .1 & max_pval >= .05 ~ c('dotdash', 'dotted', NA),
                         max_pval < .05 & max_pval >= .01 ~ c('dotdash', 'dotted', NA),
                         max_pval < .01 ~ c('dotdash', NA, NA));

  #### make variable names shorter ####
  RHS_short = dplyr::case_when(nchar(r1$cov) > 10 ~ substr(r1$cov, start = 1, stop = 10),
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
                          r1$data[,r1$y],"pairwise.complete.obs"))
  }

  # x-axis of plot (effect size)
  mean_noNA = function(x){return(mean(x, na.rm=T))}
  sd_noNA = function(x){return(sd(x, na.rm=T))}

  r1$data = r1$data[c(r1$tx, r1$cov)]
  trt = r1$tx

  mean_sd_bygroup = r1$data %>%
    dplyr::select(.data[[r1$tx]], r1$cov) %>%
    dplyr::mutate_if(is.factor, as.numeric) %>%
    dplyr::group_by(.data[[r1$tx]]) %>%
    # adding in so if there are factors, we should make numeric.
    dplyr::summarize_all(list(mean_noNA, sd_noNA)) %>%
    data.frame()

  es_cov = rep(NA, length(r1$cov))
  es_cov_actual = rep(NA, length(r1$cov))
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
      es_cov_actual[i] = (diff_means/denom_ATE)
    }
  } else if(r1$estimand == "ATT"){
    for(i in 1:length(r1$cov)){
      #trt = trt = r1$tx
      diff_means = diff(mean_sd_bygroup[,colnames(mean_sd_bygroup)[grep(paste0("^", r1$cov[i], "_fn1$"), colnames(mean_sd_bygroup))]])
      treat_only = mean_sd_bygroup %>% dplyr::filter(.data[[r1$tx]] == 1) %>% data.frame()
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
      es_cov_actual[i] = (diff_means/denom_ATT)
    }
  }

  obs_cors = cbind(obs_cors, es_cov, es_cov_actual)
  obs_cors = obs_cors %>%
    data.frame() %>%
    dplyr::mutate(cov=r1$cov)

  colnames(obs_cors) = c("Cor_Outcome", "ES", "ESTRUE", "cov")

  cor_high = obs_cors[obs_cors$Cor_Outcome>max(r1$es_grid),]
  if(nrow(cor_high)!=0){
    text_high = paste0(cor_high$cov, " (Actual: ", sprintf("%.3f", round(cor_high$Cor_Outcome,3)), ")")
    text_high = paste0("NOTE: Covariates with absolute correlation with outcome greater than ",
                       max(r1$es_grid), ": ", paste(text_high, collapse=", "))
  } else{
    text_high=""
  }

  obs_cors = obs_cors %>%
    dplyr::mutate(Cor_Outcome = dplyr::case_when(Cor_Outcome > max(r1$rho_grid) ~ max(r1$rho_grid), TRUE ~ Cor_Outcome))

  es_high = obs_cors[abs(obs_cors$ES)>max(r1$es_grid),]
  if(nrow(es_high)!=0){
    text_high_es = paste0(es_high$cov, " (Actual: ", sprintf("%.3f", round(es_high$ES,3)), ")")
    text_high_es = paste0("NOTE: Covariates with effect size greater than max plot allows: ",
                          paste(text_high_es, collapse=", "))
    #
  } else{
    text_high_es = ""
  }

  obs_cors = obs_cors %>%
    dplyr::mutate(ES = dplyr::case_when(abs(ES) > max(r1$es_grid) ~ max(r1$es_grid)*(ES/abs(ES)), TRUE ~ ES),
           # temporary line of code
           cov = gsub('_.*','',cov))

  return(list(r1=r1, r1_df=r1_df, obs_cors=obs_cors, text_high=text_high,
              text_high_es=text_high_es, pvals=pvals, pval_lines=pval_lines, raw = raw))

}

