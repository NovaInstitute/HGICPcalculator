
#' calculateCPy
#'
#' @param CPy tibble
#' @param N
#' @param enddate
#' @param startdate
#' @param frMijk
#' @param XMijk
#' @param approach Character. One of "frequency" or "weighing". Default: "frequency"
#'
#' @return tibble
#' @export
#'
#' @examples

calculateCPy <- function (CPy = NULL,
                          dfFreq = NULL,
                          datevars = c("year", "week", "weekday"),
                          groupvar = "households",
                          approach =  c("frequency", "weighing")[1],
                          indexvars = c("place", "year",  "fuel"),
                          N = NULL,
                          enddate = "date_end_monitoring_period",
                          startdate = "date_start_monitoring_period",
                          frMijk = "frMijk",
                          frMij = "frMij",
                          XMijk = "XMijk",
                          XMi = "XMi"
                          )
{

  if (is.null(CPy)) stop("CPy cannot me NULL")
  if (approach == "frequency"){
    if (is.null(dfFreq)) stop("dfFreq cannot me NULL")
    if (is.null(N)) "Listen, this won't work without a population estimate: \n Please provide N"
    if (is.null(groupvar)) stop("groupvar cannot me NULL")
  }

  if (!is_tibble(CPy))
    stop("CPy must be a tibble")

  if (approach == "weighing"){
    return(CPy %>%
             group_by(!!!syms(indexvars), assignment) %>%
             mutate(CP = !!sym(frMijk) * !!sym(XMijk)) %>%
             summarise(CP = sum(CP, na.rm = TRUE)))
  }

  if (approach == "frequency"){

    dfMeanFreq <- dfFreq %>%
      group_by(!!!syms(indexvars), !!sym(groupvar)) %>%
      select(!!datevars, !!frMijk) %>%
      summarise(frMij = mean(!!sym(frMijk))) %>%
      summarise(frMi = mean(frMij))

    dfMeanMass <- group_by(CPy, !!!syms(indexvars), assignment) %>%
      mutate(CPij =  !!sym(frMij) * !!sym(XMi)) %>%
      summarise(
        start = min(date_start_monitoring_period),
        end = max(date_end_monitoring_period),
        mean_days = mean(!!sym({{enddate}}) - !!sym({{startdate}})),
        sd_days = sd(!!sym({{enddate}}) - !!sym({{startdate}})),
        CPi = mean(CPij, na.rm = TRUE))


    dfMeanMass %>% left_join(dfMeanFreq) %>%
      mutate(N = N,
             CP = as.numeric(mean_days) * N * CPi * frMi,
             CP = units::as_units(CP, "kg"),
             CP = units::set_units(CP, "tonne"))
  }

}

#' calculateCP
#' Oorweeg of hierdie funksie korrek en nodig is !!!!!!!!!!! d
#' @param data
#' @param XPijk
#' @param XBijk
#' @param frMijk
#' @param frBPijk
#' @param groupvar
#' @param N
#'
#' @return
#' @export
#'
#'

calculateCP <- function(data ,
                       XPijk = "XPijk",
                       XBijk = "XBijk",
                       frMijk = "frMijk",
                       frBPijk = "frBPijk",
                       groupvar = "households",
                       N = NULL, ...){

  if (is.null(N)) stop("please provice N")

  #data <- data %>% mutate(Z = sample(c("KT", "CM"), size = nrow(.), replace = TRUE))

  KPTValue <- calculateXBi(data, groupvar = groupvar, ...) %>% pull()

  XMi <- calculateXMi(data, KPTValue = KPTValue, groupvar = groupvar, ...) %>% pull()  %>% units::as_units("kg")

  data.frame(estimate = N * XMi)
}

#' Title
#'
#' @param data
#' @param XPijk
#' @param XBijk
#' @param frMijk
#' @param frBPijk
#' @param groupvar
#' @param N
#'
#' @return
#' @export
#'
#' @examples

simulateCP <- function(data ,
                       XPijk = "XPijk",
                       XBijk = "XBijk",
                       frMijk = "frMijk",
                       frBPijk = "frBPijk",
                       groupvar = "households",
                       N = NDP,... ){

  if (is.null(N)) N = nrow(data)

  #data <- data %>% mutate(Z = sample(c("KT", "CM"), size = nrow(.), replace = TRUE))

  KPTValue <- simulateXBi(data, selection_statement = "Z_kt == 1 &  Z_bw == 1") %>% pull()

  XMi <- simulateXMi(data, selection_statement = "Z_cm == 1 &  Z_cmw == 1" ) %>% pull()  %>% units::as_units("kg")

  data.frame(estimate = N * XMi)
}
