#' calculateER
#'
#' @param BE tibble from calculateE(  ..., outcome = "BE")
#' @param PE tibble from calculateE(  ..., outcome = "PE")
#' @param LE tibble from calculateE(  ..., outcome = "LE")
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
                        web3 = FALSE){

  BE <- HGICPcalculator::web3Sub(BE)
  PE <- HGICPcalculator::web3Sub(PE)
  LE <- HGICPcalculator::web3Sub(LE)

  res <- dplyr::left_join(BE, PE) %>%
    dplyr::left_join(LE) %>%
    dplyr::mutate(ER = BE - PE - LE)

  res %>% dplyr::mutate(ER = units::set_units(ER, t)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(ER = sum(ER, na.rm = TRUE))  %>%
    HGICPcalculator::switchify(format = format) %>%
    HGICPcalculator::web3lify(web3 = web3)
}

#' simulateER
#'
#' @param data
#' @param ...
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return data.frame
#' @export


simulateER <- function(data,
                       format = NULL,
                       web3 = FALSE,
                       ...){
  BE <- simulateBE(data) %>% pull(estimate)
  PE <- simulatePE(data) %>% pull(estimate)

  data.frame(estimate = BE - PE)  %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}


