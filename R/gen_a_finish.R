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

  a <- rep(NA, a_res$n)
  # # if pi = 0.5
  # a[ind] <- a_res$pi * (1+exp(-1 * a_res$es * U[ind]))
  # a[-ind] <- (1-a_res$pi) * (1+exp(-1 * a_res$es * U[-ind])) / (exp(-1 * a_res$es * U[-ind]))

  # if pi doesn't equal 0.5
  expTx = exp(-log(a_res$pi / (1 - a_res$pi)) +
                 ((a_res$es^2 * (1 - 2*a_res$pi)) / 2) -
                 (a_res$es * U[ind]))
  expCntrl = exp(-log(a_res$pi / (1 - a_res$pi)) +
                   ((a_res$es^2 * (1 - 2*a_res$pi)) / 2) -
                   (a_res$es * U[-ind]))

  a[ind] <- a_res$pi * (1 + expTx)
  a[-ind] <- (1 - a_res$pi) * (((1 + expCntrl)) / (expCntrl))

  return(a)
}


