

simulateBE <- function(data, ...){

  CB <- simulateCB(data) %>% pull(estimate)
  EF <- dfCOEF$COEF[[1]]
  fNRB <- dffNRB$fNRB[[1]]
  data.frame(estimate = CB * EF * fNRB)
}

simulatePE <- function(data, ...){
  CP <- simulateCP(data) %>% pull(estimate)
  EF <- dfCOEF$COEF[[1]]
  fNRB <- dffNRB$fNRB[[1]]
  data.frame(estimate = CP * EF * fNRB)

}

simulateLE <- function(data, ...){

  Xl <- simulateXl(data) %>% pull(estimate)
  EF <- dfCOEF$COEF[[1]]
  fNRB <- dffNRB$fNRB[[1]]
  data.frame(estimate =  Xl * EF * fNRB)
}

simulateER <- function(data, ...){
  BE <- simulateBE(data) %>% pull(estimate)
  PE <- simulatePE(data) %>% pull(estimate)

  data.frame(estimate = BE - PE)
}


