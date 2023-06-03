#' calculateE
#' @description
#' Generec emission calculation function
#'
#' @param CBy Numeric. Result of calculateCBy()
#' @param COEF Numeric. CO2 emission factor in g/kg.  Default value 1560
#' @param fNRB numeric. Default value 0.3
#' @return numeric
#' @export
#'
#' @examples calculateE(Cby)

calculateE<- function(Cy,
                       COEF = dfCOEF ,
                       fNRB = dffNRB,
                       var = "CB",
                       outcome = "BE",
                       onlyOutcomeAndGroups = TRUE){
  if (!(is_tibble(Cy) & is_tibble(COEF) & is_tibble(fNRB))) stop("Cy and COEF and fNRB must be tibbles")

  res <- left_join(Cy, COEF) %>%
    left_join(fNRB) %>%
    mutate(!!sym(outcome) := !!sym(var) * (COEF) * fNRB)

  if(onlyOutcomeAndGroups) res <- res %>% select({{outcome}})

  res
}
