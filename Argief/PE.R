#' simulatePE
#'
#' @param data
#' @param ...
#' @return data.frame
#' @export

simulatePE <- function(data, ...){
  CP <- simulateCP(data) %>% pull(estimate)
  EF <- dfCOEF$COEF[[1]]
  fNRB <- dffNRB$fNRB[[1]]
  data.frame(estimate = CP * EF * fNRB)

}
