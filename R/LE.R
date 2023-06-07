

#' calculateLPy
#' @param CPy tibble with summarise results of Project Kitchen test per household
#' @param CMsummary tibble with a summary of the Continued Monitoring data per household ¡¡¡
#' !!! consider creating internally from dfFreq
#' @param dfFreq tibble with the raw data of above
#' @param frr tibble with result from calculateFRR
#' @param group_vars Character. Grouping , typically c("place", "year", "fuel")
#' @param N Population size
#' @param qr Character. Variable in CPy with household ID or QR code
#' @param hhvar  Character.
#' @param startdate Character. Variable in CMsummary with start of monitoring period
#' @param enddate Character. Variable in CMsummary with end of monitoring period
#' @param frMijk Character.
#' @param frMij Character.
#' @param XBPijk Character.
#' @param XBPi Character.
#' @return tibble
#' @export


calculateLPy <- function (CPy = NULL,
                          CMsummary = CPfj_f,
                          dfFreq = NULL,
                          frr = NULL,
                          group_vars = c("place", "year", "fuel"),
                          N = NULL,
                          qr = "household_qr_code",
                          hhvar = "households",
                          enddate = "date_end_monitoring_period",
                          startdate = "date_start_monitoring_period",
                          frMijk = "frMijk",
                          frMij = "frMij",
                          XBPijk = "XBPijk",
                          XBPi = "XBPi"
)
{

  if (is.null(CPy)) stop("CPy cannot me NULL")
  if (!is_tibble(CPy)) stop("CPy must be a tibble")
  if (is.null(frr)) stop("frr cannot me NULL")
  if (!is_tibble(frr)) stop("frr must be a tibble")
  if (!is.numeric(N)) stop("N must be numerical")
  if (is.null(dfFreq)) stop("dfFreq cannot me NULL")
  if (is.null(N)) "Listen, this won't work without a population estimate: \n Please provide N"
  if (is.null(hhvar)) stop("hhvar cannot me NULL")

  dfMeanMass <- group_by(CPy, !!!syms(group_vars), assignment) %>%
    group_by(!!!syms(group_vars), !!sym(qr)) %>%
    summarise(XBPij = mean(XBPijk)) %>%
    summarise(XBPi = mean(XBPij) %>% units::as_units("kg")) %>%
    group_by(!!!syms(group_vars))

  dfCM <- CMsummary %>%
    group_by(!!!syms(group_vars), !!sym(qr)) %>%
    summarise(
      start = min(date_start_monitoring_period),
      end = max(date_end_monitoring_period),
      mean_days = mean(!!sym({{enddate}}) - !!sym({{startdate}})),
      frMi = mean(frMij, na.rm = TRUE)) %>%
    summarise(
      frMbar = mean(frMi),
      meandays = mean(mean_days))

  dfMeanMass %>%
    left_join(dfCM) %>%
    left_join(frr) %>%
    mutate(N = N,
           XBPi = units::set_units(XBPi, "tonne"),
           LP = as.numeric(meandays) * N * frrbar * frMbar * XBPi)


}

#' simulateLE
#'
#' @param data
#' @param ...
#' @return data.frame
#' @export

simulateLE <- function(data, ...){

  Xl <- simulateXl(data) %>% pull(estimate)
  EF <- dfCOEF$COEF[[1]]
  fNRB <- dffNRB$fNRB[[1]]
  data.frame(estimate =  Xl * EF * fNRB)
}
