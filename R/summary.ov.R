#### summarize fn ####
summary.ov <- function(r1){

  r1_jp$es_SML = case_when(abs(r1_jp$es_grid) < .01 ~ "es Small",
                           abs(r1_jp$es_grid) <= .1 & abs(r1_jp$es_grid) > .01 ~ "es Medium",
                           abs(r1_jp$es_grid) > .1 ~ "es Large")
  r1_jp$rho_SML = case_when(abs(r1_jp$rho_grid) < .2 ~ "rho Small",
                            abs(r1_jp$rho_grid) < .45 & abs(r1_jp$rho_grid) >=.2 ~ "rho Medium",
                            abs(r1_jp$rho_grid) >= .45 ~ "rho Large")

}

