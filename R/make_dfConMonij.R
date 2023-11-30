#' make_dfConMonij
#'
#' @param dfresP tibble. Df with the results from Project Kitchen Test
#' @param dfConMonFreq tibble. Df with the results from Continuous Monitoring phase
#' @param indexvars list with characters names. List of variables that uniquely identify subpopulation
#' @param groupvar character. household ID.
#' @param date. Date of continuous monitoring per household.
#' @param N number of households that was monitored during continuous monitoring phase
#' @param startdate
#' @param frMijk
#' @param XMijk
#' @param approach Character. One of "frequency" or "weighing". Default: "frequency"
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return tibble
#' @export
#'
#' @examples

#* @post /make_dfCPi
#* @get  /make_dfCPi
#* @serializer switch

make_dfConMonij <- function (dfConMonFreq = dfFreqRes_prep,
                        indexvars = c("place", "year",  "fuel"),
                        groupvar = "household_qr_code",
                        frMijk = "frMijk",
                        date = "date",
                        N = 1000,
                        enddate = "date_end_monitoring_period",
                        startdate = "date_start_monitoring_period",
                        format = NULL,
                        web3 = FALSE)

{ dfFreqijk <- dfConMonFreq %>% ungroup()
  dfFreqij <- dfFreqijk %>% select(!!groupvar, !!!indexvars, !!date, !!frMijk) %>%
    group_by(across(c(!!!syms(indexvars), !!sym(groupvar)))) %>%
    nest() %>%
    mutate(date_start_monitoring_period = as.Date(map_dbl(data, ~ min(.[[date]],na.rm = TRUE)),origin = "1970-01-01"),
           date_end_monitoring_period = as.Date(map_dbl(data, ~ max(.[[date]], na.rm = TRUE)),origin = "1970-01-01"),
           ndays_monitoring = as.integer(date_end_monitoring_period)-as.integer(date_start_monitoring_period),
           frMij = map_dbl(data, ~mean(.[[frMijk]], na.rm = TRUE))) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)

}
