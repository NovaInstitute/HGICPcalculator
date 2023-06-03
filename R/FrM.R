

#' calculateFrM
#' @description
#' calculate the mean frequency of use of the project measures by households
#' @param data tibble
#' @param frMijk Character. Variable with frequency of fire from project technology
#' by household group, household and day. Default "frMijk"
#' @param groupvar Character. Variable for grouping. Default "households"
#'
#' @return data.frame
#' @export


calculateFrM <- function(data , frMijk = "frMijk", groupvar = "households"){
  data %>%
    group_by(!!sym(groupvar)) %>%
    summarise(frM = mean(!!sym(frMijk), na.rm = TRUE)) %>%
    summarise(estimate = mean(frM)) %>%
    as.data.frame()
}


#' simulateFrM
#' @description
#' Simulation function for calculateFrM. Filters for kitchen test group and project scenario week
#'
#' @param data tibble
#' @param ... passed to calculateFrM
#'
#' @return data.frame
#' @export
#'
#' @examples
#' est_Frm <- declare_estimators(handler = label_estimator(simulateFrM), label = "frM", inquiry = "aveFrM")

simulateFrM <- function(data , selection_statement = "Z_kt == 1 & Z_pw == 1", ...){

  data2 <- data %>% filter(eval(rlang::parse_expr(selection_statement)))  # kitchen test, project scenario

  calculateFrM(data2, ...)
}
