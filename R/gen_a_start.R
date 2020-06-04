#### gen_a_start fn ####
gen_a_start <- function(y, tx, es, rho, b1low, b1high){
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
  n <- n1 + n0
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

  b1 = runif(1, min = b1low, max = b1high);

  b0 <- (A-b1*c1*pi - Q)/((1-pi)*c0)

  ve1 <- 1 - b1^2 * var(ystar1)
  ve0 <- 1 - b0^2 * var(ystar0)

  # redraw b1 if ve0 < 0
  # while(ve0 < 0 | ve1 < 0){
  #   if(abs(b1 - b1high) >=  (b1 - b1low)){
  #     b1 = b1 + .01
  #   } else{
  #     b1 = b1 - .01
  #   }
  #   #b1 = runif(1, min = b1low, max = b1high);
  #   b0 <- (A-b1*c1*pi - Q)/((1-pi)*c0)
  #   ve1 <- 1 - b1^2 * var(ystar1)
  #   ve0 <- 1 - b0^2 * var(ystar0)
  # }
  if(!(abs(b0) <= b0lim)) stop("b0 is too large in absolute value. Try reducing the size of the grid.")

  return(a_res = list(n1 = n1, ve1 = ve1, b1 = b1, ystar1 = ystar1,
                      es = es, pi = pi, n0 = n0, ve0 = ve0, b0 = b0,
                      ystar0 = ystar0, n = n, ind = ind))
}

