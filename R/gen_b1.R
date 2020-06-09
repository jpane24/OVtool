gen_b1 <- function(y, tx, es, rho){

  # first generate b1 -- to go in ov_sim
  # b1_low_high = data.frame(expand.grid(es=es_grid, rho=rho_grid)) %>%
  #   dplyr::mutate(negb1lim = NA,
  #                 boneg = NA,
  #                 b1lim=NA,
  #                 b0pos=NA) %>%
  #   data.frame()
  # for(i in 1:nrow(b1_low_high)){
  #   b1_low_high[i,3:6] = gen_b1(y=data[,y], tx = data[,tx],
  #                               es = b1_low_high$es[i], rho = b1_low_high$rho[i])
  # }
  # b1_low = max(b1_low_high$b1_low)
  # b1_high = min(b1_low_high$b1_high)
  # b1_final = mean(c(b1_low, b1_high))
  # # b1_final = runif(1, b1_low, b1_high); print(b1_final)

  ind <- which(tx == 1)
  if(length(unique(y[ind])) == 2){
    y[ind][which(y[ind]==1)] = runif(length(which(y[ind]==1)), min=1, max=2)
    y[ind][which(y[ind]==0)] = runif(length(which(y[ind]==0)), min=-1, max=0)

    y[-ind][which(y[-ind]==1)] = runif(length(which(y[-ind]==1)), min=1, max=2)
    y[-ind][which(y[-ind]==0)] = runif(length(which(y[-ind]==0)), min=-1, max=0)
  }

  cdf1 = EnvStats::ecdfPlot(y[ind], discrete = F, plot.it = F)
  cdf0 = EnvStats::ecdfPlot(y[-ind], discrete = F, plot.it = F)
  ystar1 = qnorm(cdf1$Cumulative.Probabilities[rank(y[ind], ties.method = 'random')])
  ystar0 = qnorm(cdf0$Cumulative.Probabilities[rank(y[-ind], ties.method = 'random')])

  n1 <- sum(tx)
  n0 <- sum(1-tx)
  n <- n1 + n0
  c1 <- cov(ystar1,y[ind])
  c0 <- cov(ystar0,y[-ind])

  pi <- mean(tx)

  v1 <- var(y[ind])
  v0 <- var(y[-ind])

  sd1 <- sqrt(v1)
  sd0 <- sqrt(v0)

  vU <- 1 + es^2*pi*(1-pi)
  #vU <- es^2*pi*(1-pi) In pdf, there is no 1 +

  Y <- y - mean(y)

  vY <- var(y)

  b1lim <- sd1/c1
  b0lim <- sd0/c0

  A <- rho * sqrt(vU * vY)

  Q <- es*(1-pi)*mean(y[ind])*pi - es*pi*mean(y[-ind])*(1-pi)

  # Take b1
  alpha = (A - Q)/((1-pi)*c0)
  beta = (-c1*pi)/((1-pi)*c0)
  boneg = ((-b0lim - alpha) / beta)
  b0pos = ((b0lim - alpha) / beta)

  if(beta > 0){
    b1low = max(-b1lim, ((-b0lim - alpha) / beta))
    b1high = min(b1lim, ((b0lim - alpha) / beta))
  }
  if(beta < 0){
    b1low = max(-b1lim, ((b0lim - alpha) / beta))
    b1high = min(b1lim, ((-b0lim - alpha) / beta))
  }

  return(c(-b1lim, boneg, b1lim, b0pos))
}
