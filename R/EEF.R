
#' calculatEEFwithRR
#'
#' @param data tibble or character
#' @param web3 Logical. Use a web3 content address or a tibble (default)
#' @param indexvars Character. One of c("place", "year", "fuel")
#' @param groupvar Character. Default "households"
#' @param XBijk Character. Default "XBijk"
#' @param XMijk Character. Default "XMijk"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

#* calculatEEFwithRR
#* @param data
#* @post /calculateEEFwithRR
#*

calculateEEFwithRR <- function(data,
                               web3 = FALSE,
                               indexvars = c("place", "year", "fuel"),
                               groupvar = "households",
                               XBijk = "XBijk",
                               XMijk = "XMijk", ...){
  # xBi
  # XBij

  if (web3) {
    data <- HGICPcalculator::wweb3S2R(data)
  }

  meanrr <- calculateRRj(data, groupvar = c(groupvar, indexvars), ...) %>%
    pull(rr) %>%
    `[`(is.finite(.)) %>%
    mean()

  calculateXMB(data,
               groupvar = c(indexvars),
               XBijk = XBijk,
               XMijk = XMijk,
               ...) %>%
    mutate(rrbar =  meanrr,
           eef = (xBi * rrbar) / xMi )
}

simulateEEFwithRR <- function(data, ...){
  meanrr <- simulateRRj(data, selection_statement = "Z_kt == 1 &  Z_pw == 1", ...) %>%
    pull(rr) %>%
    `[`(is.finite(.)) %>%
    mean()

  simulateXMB(data, selection_statement = "Z_kt == 1 &  Z_pw == 1", ...)  %>%
    mutate(rrbar =  meanrr,
           eef = (xBi * rrbar) / xMi )

}
