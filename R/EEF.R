
#' calculatEEFwithRR
#'
#' @param data tibble
#' @param groupvar Character. Default "households"
#' @param XBijk Character. Default "XBijk"
#' @param XMijk Character. Default "XMijk"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
calculatEEFwithRR <- function(data,
                              groupvar = "households",
                              XBijk = "XBijk",
                              XMijk = "XMijk", ...){
  # xBi
  # XBij

  meanrr <- calculateRRj(data, groupvar = groupvar, ...) %>%
    pull(rr) %>%
    `[`(is.finite(.)) %>%
    mean()

  calculateXMB(data,
               groupvar = groupvar,
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
