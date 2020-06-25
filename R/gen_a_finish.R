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

  a <- (1+exp(-1 * a_res$es * U)) / a_res$pi
  # a_2 <- a_res$pi * (1 + exp( (-log(a_res$pi/(1-a_res$pi))) +
  #                              ( a_res$es^2 * (1-2*a_res$pi) / 2) -
  #                               ( a_res$es * U )))
  return(a)
}


