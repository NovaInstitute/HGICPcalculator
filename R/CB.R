

#' calculateCBy
#'
#' @param eef tibble
#' @param CPy tibble
#' @param approach Character. One of "frequency" or "weighing". Default: "frequency"
#' @return tibble
#' @export
#'
#' @examples

calculateCBy <- function (eef,
                          CPy,
                          approach =  c("frequency", "weighing")[1],
                          N = NULL,
                          enddate = "date_end_monitoring_period",
                          startdate = "date_start_monitoring_period")
{
  if (!(is_tibble(CPy) & is_tibble(eef)))
    stop("Both CPy and eef must be tibbles")

  if (approach == "weighing"){
    return(left_join(CPy,
              eef %>% select(eef)) %>%
      group_by(place, year,  fuel, assignment) %>%
      mutate(CB = kg_p_month_m2 * eef) %>%
      summarise(CB = sum(CB, na.rm = TRUE)))
  }

  if (approach == "frequency"){

    if (is.null(N)) "listen, this won't work without a population estimate: \n Please provide N"

    left_join(CPy,
              eef %>% select(eef) %>%  units::drop_units()) %>%
      group_by(place, year,  fuel, assignment) %>%
      mutate(CB = CPi * eef) %>%
      summarise(
        start = min(date_start_monitoring_period),
        end = max(date_end_monitoring_period),
        mean_days = mean(!!sym({{enddate}}) - !!sym({{startdate}})),
        sd_days = sd(!!sym({{enddate}}) - !!sym({{startdate}})),
        CPbar = mean(CPi, na.rm = TRUE),
        mean_eef = mean(eef),
        CBbar = mean(CB, na.rm = TRUE)
        ) %>%
      mutate(
        N = N,
        CB = CBbar * N * as.numeric(mean_days) ,
        CB = units::as_units(CB, "kg"),
        CB = units::set_units(CB, "tonne")
        )
  }

}

#' Title
#'
#' @param data
#' @param XPijk
#' @param XBijk
#' @param frMijk
#' @param frBPijk
#' @param frBijk
#' @param frPijk
#' @param groupvar
#' @param stratvar
#' @param NDPi
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

simulateCB <- function(data , XPijk = "XPijk",
                       XBijk = "XBijk",
                       frMijk = "frMijk",
                       frBPijk = "frBPijk",
                       frBijk = "frBijk",
                       frPijk = "frPijk",
                       groupvar = "households",
                       stratvar = "Z",
                       NDPi = NULL,
                       ...){
  CP <- simulateCP(data = data, ...)
  eef <- simulateEEFwithRR(data) %>% pull(eef)
  CB <- CP * eef
  data.frame(CB)
}
