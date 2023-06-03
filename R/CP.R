
calculateCP <- function(data ,
                       XPijk = "XPijk",
                       XBijk = "XBijk",
                       frMijk = "frMijk",
                       frBPijk = "frBPijk",
                       groupvar = "households",
                       NDPi = NDP){

  if (is.null(NDPi)) NDPi = nrow(data)

  #data <- data %>% mutate(Z = sample(c("KT", "CM"), size = nrow(.), replace = TRUE))

  KPTValue <- calculateXBi(data, ...) %>% pull()

  XMi <- calculateXMi(data, KPTValue = KPTValue, ...) %>% pull()  %>% units::as_units("kg")

  data.frame(estimate = NDPi * XMi)
}

#' Title
#'
#' @param data
#' @param XPijk
#' @param XBijk
#' @param frMijk
#' @param frBPijk
#' @param groupvar
#' @param NDPi
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
                       NDPi = NDP,... ){

  if (is.null(NDPi)) NDPi = nrow(data)

  #data <- data %>% mutate(Z = sample(c("KT", "CM"), size = nrow(.), replace = TRUE))

  KPTValue <- simulateXBi(data, selection_statement = "Z_kt == 1 &  Z_bw == 1") %>% pull()

  XMi <- simulateXMi(data, selection_statement = "Z_cm == 1 &  Z_cmw == 1" ) %>% pull()  %>% units::as_units("kg")

  data.frame(estimate = NDPi * XMi)
}
