
#' calculateRRj
#' @description
#' Calculate replacement rate per household
#'
#' @param data
#' @param groupvar
#' @param frMij.
#' @param frBij.
#' @param frBPij.
#' @param ndays_BKT
#' @param ndays_PKT
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return tibble with five column: <groupvar> meanfrBij meanfrMij meanfrPij    rr
#' @export

#* @post /calculateRRj
#* @get /calculateRRj
#* @serializer switch

calculateRRj <- function(data,
         groupvar = "households",
         frBij. = "frBij.",
         frMij. = "frMijk",
         frBPij. = "frBPij.",
         ndays_BKT = "ndays_BKT",
         ndays_PKT = "ndays_PKT",
         format = NULL,
         web3 = FALSE){

  data %>% mutate(ad_frBij. = frBij./ndays_BKT * ndays_PKT,
                  frPij. = frBPij. + frMij.,
                  rr = (ad_frBij. - frBPij.)/frMij.
  ) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}


#' simulateRRj
#'
#' @param data
#' @param selection_statement Character. filter exporession: Default :"Z_kt == 1 &  Z_pw == 1"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

simulateRRj <- function(data, selection_statement = "Z_kt == 1 &  Z_pw == 1", ...){

  data1 <- data %>% filter(eval(rlang::parse_expr(selection_statement)))
  calculateRRj(data1, ...)


}
