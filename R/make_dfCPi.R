#' make_dfCPi
#'
#' @param dfConMon data.frame
#' @param KTresults data.frame
#' @param indexvars list with characters names. List of variables that uniquely identify subpopulation
#' @param startdate Character. Name of the column with the start date of the monitoring period
#' @param enddate Character. Name of the column with the end date of the monitoring period
#' @param N Numeric. Population estimate
#' @param frMijk Character. Default  "frMijk"
#' @param XMijk Character. Default "XMijk"
#' @param XBij Character. Default "XBij"
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

make_dfCPi <- function (dfConMon = dfConMon,
                        KTresults = dfresBP,
                        indexvars = c("place", "year",  "fuel"),
                        startdate = "date_start_monitoring_period",
                        enddate = "date_end_monitoring_period",
                        N = 19,
                        ndays_cm = "ndays_cm",
                        nfire_cm = "nfire_cm",
                        XMij = "XMij_m2",
                        XBij = "XBij_m2",
                        format = NULL,
                        web3 = FALSE)

{
dfConMon_sum <- dfConMon %>% group_by(!!!syms(indexvars)) %>%
    summarise(date_start_monitoring_period = min(!!sym(startdate)),
              date_end_monitoring_period = max(!!sym(enddate)),
              project_days = sum(!!sym(ndays_cm), na.rm = TRUE),
              av_days = mean(!!sym(ndays_cm), na.rm = TRUE),
              frMi. = sum(!!sym(nfire_cm),na.rm = TRUE),
              frMi = frMi./as.numeric(project_days))

  dfKTresults <- KTresults %>% ungroup()
  dfKTresults <- dfKTresults %>%  select(!!XBij, !!XMij, !!!indexvars) %>%
    group_by(!!!syms(indexvars)) %>%
    summarise(XMi = mean(!!sym(XMij), na.rm = TRUE),
              XBi = mean(!!sym(XBij), na.rm = TRUE))

  dfCPi <- dfConMon_sum %>% left_join(dfKTresults) %>%
    mutate(CPi = as.numeric(av_days)*N*frMi * XMi,
           CPi = units::as_units(CPi, "kg"),
           CPi = units::set_units(CPi, "tonne"))
  if (is.null(format) & !web3){
    return(dfCPi)
  }
  dfCPi %>% switchify(format = format) %>%
    web3lify(web3 = web3)

}


