
#' calculateXBi
#' @description
#' Function to calculate baseline wood consumption from the baseline technologies per household
#'
#' @param data tibble
#' @param XBijk Character. Variable with wood use from baseline technologies per day. Default "XBijk"
#' @param groupvar Character. Grouping variable. Default "households"
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return data.frame
#' @export

#* @post /calculateXBi
#* @get /calculateXBi
#* @serializer switch

calculateXBi <- function(data , XBijk = "XBijk", groupvar = "households",
                         format = NULL,
                         web3 = FALSE){
  data %>%
    group_by(!!sym(groupvar)) %>%
    summarise(XBi = mean(!!sym(XBijk), na.rm = TRUE)) %>%
    summarise(estimate = mean(XBi)) %>%
    as.data.frame() %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}

#' simulateXBi
#' @description
#' Function to use in simulation instead of calculateXBi.
#' Filters data for kitchen test group (Z_kt == 1) and  baseline week (Z_bw == 1)
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' est_XBi <- declare_estimator(handler = label_estimator(simulateXBi), label = "XBi", inquiry = "XBi")

simulateXBi <- function(data, selection_statement = "Z_kt == 1 &  Z_bw == 1", ...){
  data2 <- data %>% filter(eval(rlang::parse_expr(selection_statement)))

  calculateXBi(data2, ...)

}
