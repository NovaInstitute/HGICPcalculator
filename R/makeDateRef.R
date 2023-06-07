
#' makeDateRef
#'
#' @param ERstartdate Date
#' @param ERenddate Date
#'
#' @return tibble
#' @export
#'
#' @examples
#'
makeDateRef <- function(ERstartdate = as.Date("2023-01-01"),
                        ERenddate = as.Date("2023-05-31")){
  tibble(
    date = seq.Date(ERstartdate, to = ERenddate, by = 1)) %>%
    mutate(year = lubridate::year(date),
           week = lubridate::week(date),
           weekday = lubridate::wday(date, label = TRUE))
}
