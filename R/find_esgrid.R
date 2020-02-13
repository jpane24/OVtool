#### setup es grid ####
find_esgrid = function(my_data = data, my_cov = cov, treatment = tx, outcome = y, my_estimand = estimand){
  # y-axis of plot (correlation between covariates and outcome)
  obs_cors = rep(NA, length(my_data[,my_cov]))
  for(i in 1:length(obs_cors)){
    if(is.factor(my_data[,my_cov[i]])){
      obs_cors[i] = abs(cor(as.numeric(my_data[,my_cov[i]]),
                            my_data[,outcome],"pairwise.complete.obs"))
    } else {
      obs_cors[i] = abs(cor(as.numeric(as.character(my_data[,my_cov[i]])),
                            my_data[,outcome],"pairwise.complete.obs"))
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
