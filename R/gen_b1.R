gen_b1 <- function(y, tx, es, rho){
  ind <- which(tx == 1)
  if(length(unique(y[ind])) == 2){
    y[ind][which(y[ind]==1)] = runif(length(which(y[ind]==1)), min=1, max=2)
    y[ind][which(y[ind]==0)] = runif(length(which(y[ind]==0)), min=-1, max=0)

    y[-ind][which(y[-ind]==1)] = runif(length(which(y[-ind]==1)), min=1, max=2)
    y[-ind][which(y[-ind]==0)] = runif(length(which(y[-ind]==0)), min=-1, max=0)
  }

  # # Old way:
  # cdf1 <- ecdf(y[ind])
  # cdf0 <- ecdf(y[-ind])
  # ystar1 <- qnorm(cdf1(y[ind]))
  # ystar1 <- ifelse(ystar1==Inf, max(ystar1[which(ystar1 < Inf)]), ystar1)
  # ystar0 <- qnorm(cdf0(y[which(tx==0)]))
  # ystar0 <- ifelse(ystar0==Inf, max(ystar0[which(ystar0 < Inf)]), ystar0)

  # New way:
  cdf1 = EnvStats::ecdfPlot(y[ind], discrete = F, plot.it = F)
  cdf0 = EnvStats::ecdfPlot(y[-ind], discrete = F, plot.it = F)
  ystar1 = qnorm(cdf1$Cumulative.Probabilities[rank(y[ind], ties.method = 'random')])
  ystar0 = qnorm(cdf0$Cumulative.Probabilities[rank(y[-ind], ties.method = 'random')])

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

  # Take b1
  alpha = (A - Q)/((1-pi)*c0)
  beta = (-c1*pi)/((1-pi)*c0)

  if(beta > 0){
    b1low <- max(-b1lim, ((-b0lim - alpha) / beta))
    b1high <- min(b1lim, ((b0lim - alpha) / beta))
  }

  if(beta < 0){
    b1low <- max(-b1lim, ((b0lim - alpha) / beta))
    b1high <- min(b1lim, ((-b0lim - alpha) / beta))
  }

  return(c(b1low, b1high))
}
