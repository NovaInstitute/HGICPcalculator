#' make_dfCPi
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

make_dfCPi <- function (dfConMonFreq = dfFreqRes_prep,
                        KTresults = dfresBP,
                        indexvars = c("place", "year",  "fuel"),
                        groupvar = "household_qr_code",
                        frMijk = "frMijk",
                        XMij = "XMij_m2",
                        XBij = "XBij_m2",
                        date = "date",
                        N = 1000,
                        enddate = "date_end_monitoring_period",
                        startdate = "date_start_monitoring_period",
                        format = NULL,
                        web3 = FALSE)

  # {CPy <- web3Sub(CPy)
  # if (is.null(CPy)) stop("CPy cannot me NULL")
  # if (approach == "frequency"){
  #   if (is.null(dfFreq)) stop("dfFreq cannot me NULL")
  #   if (is.null(N)) "Listen, this won't work without a population estimate: \n Please provide N"
  #   if (is.null(groupvar)) stop("groupvar cannot me NULL")
  # }{
  #
  # if (!is_tibble(CPy))
  #   stop("CPy must be a tibble")
  # }
{
    dfFreq <- dfConMonFreq %>% ungroup()
    dfFreq <- dfFreq %>% select(!!groupvar, !!!indexvars, !!date, !!frMijk) %>%
      group_by(across(c(!!!syms(indexvars), !!sym(groupvar)))) %>%
    nest() %>%
    mutate(date_start_monitoring_period = as.Date(map_dbl(data, ~ min(.[[date]],na.rm = TRUE)),origin = "1970-01-01"),
           date_end_monitoring_period = as.Date(map_dbl(data, ~ max(.[[date]], na.rm = TRUE)),origin = "1970-01-01"),
           ndays_monitoring = as.integer(date_end_monitoring_period)-as.integer(date_start_monitoring_period),
           frMij = map_dbl(data, ~mean(.[[frMijk]], na.rm = TRUE))) %>%
           group_by(!!!syms(indexvars)) %>%
   summarise(date_start_monitoring_period = min(date_start_monitoring_period),
           date_end_monitoring_period = max(date_end_monitoring_period),
           project_days = sum(ndays_monitoring, na.rm = TRUE),
           mean_days = mean(ndays_monitoring, na.rm = TRUE),
           frMi = sum(frMij*ndays_monitoring,na.rm = TRUE)/project_days)

    dfKTresults <- KTresults %>% ungroup()
    dfKTresults <- dfKTresults %>%  select(!!XBij, !!XMij, !!!indexvars) %>%
      group_by(!!!syms(indexvars)) %>%
      summarise(XMi = mean(!!sym(XMij), na.rm = TRUE),
                XBi = mean(!!sym(XBij), na.rm = TRUE))

    dfCPi <- dfFreq %>% left_join(dfKTresults) %>%
        mutate(N = N,
              CPi = mean_days * N * frMi * XMi,
              CPi = units::as_units(CPi, "kg"),
              CPi = units::set_units(CPi, "tonne"))
    if (is.null(format) & !web3){
      return(dfCPi)
    }
    dfCPi %>% switchify(format = format) %>%
        web3lify(web3 = web3)

}


