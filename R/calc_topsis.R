#topsis multicriteria
calc_topsis <- function(eachscenario, hazard.sc.data, vul.expo.dat, sp.cols){

  risk.data <- hazard.sc.data %>%
    dplyr::filter(escenario==eachscenario) %>%
    dplyr::left_join(vul.expo.dat, by=c("fip_name")) %>%
    dplyr::select(starts_with("tot_score"))

  #calculate topsis
  risk.matrix <- risk.data %>%
    as.matrix()

  topsis.w <- rep(1,ncol(risk.matrix)) #
  topsis.i <- rep("+",ncol(risk.matrix))
  topsis.res <- topsis::topsis(risk.matrix, topsis.w, topsis.i)

  topsis.res.sc <- hazard.sc.data %>%
    dplyr::filter(escenario==eachscenario) %>%
    dplyr::select(dplyr::where(is.character)) %>%
    dplyr::bind_cols(topsis.res)

  return(topsis.res.sc)
  #d <- matrix(rpois(12, 5), nrow = 4)
  #w <- c(1, 1, 2)
  #i <- c("+", "-", "+")
  #topsis(d, w, i)

}
