
#' calculatEEFwithRR
#'
#' @param data tibble or character
#' @param web3 Logical. Use a web3 content address or a tibble (default)
#' @param indexvars Character. One of c("place", "year", "fuel")
#' @param groupvar Character. Default "households"
#' @param XBijk Character. Default "XBijk"
#' @param XMijk Character. Default "XMijk"
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

#* calculatEEFwithRR
#* @param data
#* @get /calculateEEFwithRR
#* @post /calculateEEFwithRR
#*

calculateEEFwithRR <- function(data,
                               indexvars = c("place", "year", "fuel"),
                               groupvar = "households",
                               XBijk = "XBijk",
                               XMijk = "XMijk",
                               format = NULL,
                               web3 = FALSE,
                               ...){
  # xBi
  # XBij
  if (is.character(data))  {
    data <- HGICPcalculator::web3S2R(data)
    if (!is.data.frame(data)) {stop("\nI was expecting data to be the web3storrage address of a dataframe\n")}
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
           eef = (xBi * rrbar) / xMi )  %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
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
