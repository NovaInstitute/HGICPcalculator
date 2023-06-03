

#' calculateXBi
#' @description
#' Function to calculate baseline wood consumption from the baseline technologies per household
#'
#' @param data tibble
#' @param XBijk Character. Variable with wood use from baseline technologies per day. Default "XBijk"
#' @param groupvar Character. Grouping variable. Default "households"
#'
#' @return data.frame
#' @export


calculateXBi <- function(data , XBijk = "XBijk", groupvar = "households"){
  data %>%
    group_by(!!sym(groupvar)) %>%
    summarise(XBi = mean(!!sym(XBijk), na.rm = TRUE)) %>%
    summarise(estimate = mean(XBi)) %>%
    as.data.frame()
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
