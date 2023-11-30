#' make_dfRRij_EEFij
#' @description Calculate replacement rate per household
#'
#' @param data
#' @param groupvar
#' @param frMijk
#' @param frBijk
#' @param frPijk
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return tibble with five column: <groupvar> meanfrBij meanfrMij meanfrPij    rr
#' @export

#* @post /calculateRRj
#* @get /calculateRRj
#* @serializer switch

make_dfRRij_EEFij <- function(data,
                         #groupvar = "households",
                         frBij. = "frBij.",
                         frMij. = "frMij.",
                         frBPij. = "frBPij.",
                         XBij = "XBij_m2",
                         XMij = "XMij_m2",
                         ndays_BKT = "ndays_BKT",
                         ndays_PKT = "ndays_PKT",
                         format = NULL,
                         web3 = FALSE){

  data %>% mutate(ad_frBij. = !!sym(frBij.)/!!sym(ndays_BKT) * !!sym(ndays_PKT), #adjust frB for different days in baseline and project phase
                  frPij. = !!sym(frBPij.) + !!sym(frMij.),
                  rr = (ad_frBij. - !!sym(frBPij.))/!!sym(frMij.),
                  #rr2 =
                  eef = (!!sym(XBij)*rr)/!!sym(XMij)
  ) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}


