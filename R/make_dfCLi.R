#' make_dfCLi
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

#* @post /make_dfCLi
#* @get  /make_dfCLi
#* @serializer switch

make_dfCLi <- function (dfFrR = dfFrRij,
                        dfCP = dfCPi,
                        indexvars = c("place", "year",  "fuel"),
                        project_days = "project_days",
                        FrR = "FrRij",
                        CPi = "CPi",
                        frMi = "frMi",
                        XBi = "XBi",
                        N = 1000,
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


  dfFrR <- dfFrR %>% ungroup()
  dfFrR <- dfFrR %>%  select(!!!indexvars, !!FrR) %>%
    group_by(!!!syms(indexvars)) %>%
    summarise(mean_FrR = mean(!!sym(FrR), na.rm = TRUE))

  dfCLi <- dfCP %>% left_join(dfFrR) %>%
      mutate(N = N,
           CLi = mean_days * N* mean_FrR * !!sym(frMi) * !!sym(XBi),
           CLi = units::as_units(CLi, "kg"),
           CLi = units::set_units(CLi, "tonne"))

  if (is.null(format) & !web3){
    return(dfCLi)
  }
  dfCLi %>% switchify(format = format) %>%
    web3lify(web3 = web3)

}


