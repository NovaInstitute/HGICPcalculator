
#' makeDateRef
#' @description
#' Create a reference table with every day of a certain period
#' @param startdate Date
#' @param enddate Date
#'
#' @return tibble
#' @export
#'
#' @examples
#' makeDateRef(startdate = as.Date("2022-01-01"), enddate = as.Date("2022-12-31"))


makeDateRef <- function(startdate, enddate){
  tibble(
    date = seq.Date(startdate, to = enddate, by = 1)) %>%
    mutate(year = lubridate::year(date),
           week = lubridate::week(date),
           weekday = lubridate::wday(date, label = TRUE))
}

