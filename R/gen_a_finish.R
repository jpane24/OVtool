#### gen_a_finish fn ####
gen_a_finish <- function(a_res, my_estimand){

  U1 <- stats::rnorm(a_res$n1)*sqrt(a_res$ve1) + a_res$b1*a_res$ystar1 +
    a_res$es*(1-a_res$pi)
  U0 <- stats::rnorm(a_res$n0)*sqrt(a_res$ve0) + a_res$b0*a_res$ystar0 -
    a_res$es*a_res$pi

  U <- rep(NA, a_res$n)
  ind <- a_res$ind
  U[ind] <- U1
  U[-ind] <- U0

  a <- rep(NA, a_res$n)

  if(my_estimand == "ATE"){
    expTx = exp(-log(a_res$pi / (1 - a_res$pi)) +
                  ((a_res$es^2 * (1 - 2*a_res$pi)) / 2) -
                  (a_res$es * U[ind]))
    expCntrl = exp(-log(a_res$pi / (1 - a_res$pi)) +
                     ((a_res$es^2 * (1 - 2*a_res$pi)) / 2) -
                     (a_res$es * U[-ind]))

    a[ind] <- a_res$pi * (1 + expTx)
    a[-ind] <- (1 - a_res$pi) * (((1 + expCntrl)) / (expCntrl))

  } else if(my_estimand == "ATT"){
    att_num = 1 + exp((log(a_res$pi / (1 - a_res$pi)))
                      - (a_res$es^2 * (1 - 2*a_res$pi) / 2 + (a_res$es * U[-ind])))
    att_denom = 1 + exp((-log(a_res$pi / (1 - a_res$pi)))
                        + (a_res$es^2 * (1 - 2*a_res$pi) / 2 - (a_res$es * U[-ind])))

    a[ind] = 1
    a[-ind] = ((1 - a_res$pi) / (a_res$pi)) * (att_num / att_denom)

  } else{
    stop("OVtool currently only works under the ATE or ATT setting.")
  }

  return(a)
}


