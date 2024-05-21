#' calculateER
#'
#' @param BE tibble from calculateE(  ..., outcome = "BE")
#' @param PE tibble from calculateE(  ..., outcome = "PE")
#' @param LE tibble from calculateE(  ..., outcome = "LE")
#' @param indexvar Character. Default value "place"
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#' @import dplyr
#' @return data.frame
#' @export
#'
#' @examples

#* @post /calculateER
#* @get /calculateER
#* @serializer switch

calculateER <- function(BE, PE, LE,
                        format = NULL,
                        web3 = FALSE,
                        indexvar = c("place", "year")[1]){

  # BE <- HGICPcalculator::web3Sub(BE)
  # PE <- HGICPcalculator::web3Sub(PE)
  # LE <- HGICPcalculator::web3Sub(LE)
if (!is.null(LE)){
  res <- dplyr::left_join(BE, PE) %>%
    dplyr::left_join(LE) %>%
    dplyr::mutate(ERi = BEi - PEi- LEi)

  res <- res %>% dplyr::mutate(ERi = units::set_units(ERi, t)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!syms({{indexvar}})) %>%
    dplyr::summarise(BE = sum(BEi, na.rm = TRUE),
                     PE = sum(PEi, na.rm = TRUE),
                     LE = sum(LEi, na.rm = TRUE),
                     ER = sum(ERi, na.rm = TRUE))
  if (is.null(format) & !web3){
    return(res)
  }
} else {
  res <- dplyr::left_join(BE, PE) %>%
    dplyr::mutate(ERi = BEi - PEi)

  res <- res %>% dplyr::mutate(ERi = units::set_units(ERi, t)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!syms({{indexvar}})) %>%
    dplyr::summarise(BE = sum(BEi, na.rm = TRUE),
                     PE = sum(PEi, na.rm = TRUE),
                     ER = sum(ERi, na.rm = TRUE))
  if (is.null(format) & !web3){
    return(res)
}

  res <- res %>% HGICPcalculator::switchify(format = format) %>%
    HGICPcalculator::web3lify(web3 = web3)
}
}

