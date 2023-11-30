#' calculateRRij
#' @description
#' Calculate replacement rate per household
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

#* @post /calculateRRij
#* @get /calculateRRij
#* @serializer switch

calculateRRij <- function(data,
                         #groupvar = "households",
                         frBij. = "frBij.",
                         frMij. = "frMijk",
                         frBPij. = "frBPij.",
                         format = NULL,
                         web3 = FALSE){

  data %>% mutate(ad_frBij. = frBij./ndays_BKT * ndays_PKT,
                  frPij. = frBPij. + frMij.,
                  rr = (ad_frBij. - frBPij.)/frMij.
  ) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}


