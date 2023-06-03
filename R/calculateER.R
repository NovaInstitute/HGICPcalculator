#' Title
#'
#' @param BE tibble from calculateE(  ..., outcome = "BE")
#' @param PE tibble from calculateE(  ..., outcome = "PE")
#' @param LE tibble from calculateE(  ..., outcome = "LE")
#' @param onlyOutcomeAndGroups Logical
#'
#' @return
#' @export
#'
#' @examples

calculateER <- function(BE, PE, LE){
  res <- left_join(BE, PE) %>%
    left_join(LE) %>%
    mutate(ER = BE - PE - LE)

  res %>% mutate(ER = units::set_units(ER, t)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarise(ER = sum(ER, na.rm = TRUE))

}
