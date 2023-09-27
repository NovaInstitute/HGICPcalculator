
#' makeDateRef
#' @description
#' Create a reference table with every day of a certain period
#'
#' @param startdate Date
#' @param enddate Date
#' @param format Character. Output format for API
#' @param web3 Return a web3storage address or not. Default FALSE
#'
#' @return tibble
#' @export
#'
#' @examples
#' makeDateRef(startdate = as.Date("2022-01-01"), enddate = as.Date("2022-12-31"))

#* @get /makeDateRef
#* @post /makeDateRef
#* @serializer switch

makeDateRef <- function(startdate, enddate, format = NULL, web3 = FALSE){
  tibble(
    date = seq.Date(startdate, to = enddate, by = 1)) %>%
    mutate(year = lubridate::year(date),
           week = lubridate::week(date),
           weekday = lubridate::wday(date, label = TRUE)) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)

}

