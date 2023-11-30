#' calculateE
#' @description
#' Generec emission calculation function
#'
#' @param CBy Numeric. Result of calculateCBy()
#' @param COEF Numeric. CO2 emission factor in g/kg.  Default value 1560
#' @param fNRB numeric. Default value 0.3
#' @return numeric
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#' @export
#'
#' @examples calculateE(Cby)

#* @post /calculateE
#* @get /calculateE
#* @serializer switch

calculateE <- function(Cy,
                       COEF = dfCOEF ,
                       fNRB = dffNRB,
                       var = "CBi",
                       outcome = "BE",
                       onlyOutcomeAndGroups = TRUE,
                       format = NULL,
                       web3 = FALSE){

  Cy <- web3Sub(Cy)
  COEF <- web3Sub(COEF)
  fNRB <- web3Sub(fNRB)

  if (!(is_tibble(Cy) & is_tibble(COEF) & is_tibble(fNRB))) stop("Cy and COEF and fNRB must be tibbles")

  res <- left_join(Cy, COEF) %>%
    left_join(fNRB) %>%
    mutate(!!sym(outcome) := !!sym(var) * (COEF) * fNRB)

  if(onlyOutcomeAndGroups) res <- res %>% select({{outcome}})

  if (is.null(format) & !web3){
    return(res)
  }

  res %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}
