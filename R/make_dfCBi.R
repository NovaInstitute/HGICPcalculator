#' make_dfCBi
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

make_dfCBi <- function (dfEEF = dfRR_EEFij,
                        dfCP = dfCPi,
                        frMij. = "frMij.",
                        indexvars = c("place", "year",  "fuel"),
                        eef = "eef",
                        CPi = "CPi",
                        minfr = 5,
                        N = 1000,
                        format = NULL,
                        web3 = FALSE)
{


  dfEEF <- RR_EEFij %>% ungroup()
  dfEEF <- dfEEF %>%  select(!!!indexvars, eef, !!frMij.) %>%
    filter(!!sym(frMij.) > minfr) %>%
    group_by(!!!syms(indexvars)) %>%
    summarise(mean_eef = mean(!!sym(eef), na.rm = TRUE))

  dfCBi <- dfCP %>% left_join(dfEEF) %>%
    mutate(N = N,
           CBi = mean_eef * !!sym(CPi),
           CBi = units::set_units(CBi, "tonne"),
           CPi = units::set_units(CPi, "tonne"))
  if (is.null(format) & !web3){
    return(dfCBi)
  }
  dfCBi %>% switchify(format = format) %>%
    web3lify(web3 = web3)

}


