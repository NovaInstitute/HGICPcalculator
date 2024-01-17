
#' calculatEEFfromRR
#'
#' @param data tibble or character
#' @param web3 Logical. Use a web3 content address or a tibble (default)
#' @param indexvars Character. One of c("place", "year", "fuel")
#' @param XBij Character. Average fuel use (in kg) per fire in baseline KT. Default "XBij_m3"
#' @param XMijk Character. Average fuel use (in kg) per brickstar fire in project KT. Default "XMij_m3"
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

#* calculatEEFfromRR
#* @param data
#* @get /calculateEEFwithRR
#* @post /calculateEEFwithRR
#*

calculateEEFfromRR <- function(data,
                               indexvars = c("place", "year", "fuel"),
                               XBij = "XBij_m2",
                               XMij = "XMij_m2",
                               frMij. = "frMij.",
                               minfr = 5,
                               rr = "rr",
                               format = NULL,
                               web3 = FALSE,
                               ...){
    if (is.character(data))  {
    data <- HGICPcalculator::web3S2R(data)
    if (!is.data.frame(data)) {stop("\nI was expecting data to be the web3storrage address of a dataframe\n")}
  }

  dfRReff <- data %>%
    filter(!!sym(frMij.) > minfr) %>%
    mutate(eef = (!!sym(XBij)*!!sym(rr)/!!sym(XMij))) %>%
    group_by(across(all_of(indexvars))) %>%
    summarise(eef = mean(eef, na.rm = TRUE))
}


