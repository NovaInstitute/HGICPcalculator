
#' calculateFrM
#' @description
#' calculate the mean frequency of use of the project measures by households
#' @param data tibble
#' @param frMijk Character. Variable with frequency of fire from project technology
#' by household group, household and day. Default "frMijk"
#' @param groupvar Character. Variable for grouping. Default "households"
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return data.frame
#' @export

#* @post /calculateFrM
#* @get /calculateFrM
#* @serializer switch

calculateFrM <- function(data , frMijk = "frMijk", groupvar = "households",
                         format = NULL,
                         web3 = FALSE){
  data %>%
    group_by(!!sym(groupvar)) %>%
    summarise(frM = mean(!!sym(frMijk), na.rm = TRUE)) %>%
    summarise(estimate = mean(frM)) %>%
    as.data.frame() %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
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
