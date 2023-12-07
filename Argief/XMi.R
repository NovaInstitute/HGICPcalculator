
#' calculateXMi
#' @description
#' Function to calculate the mean wood use by the project measures by household
#'
#' @param data tibble
#' @param XMijk Character. Variable with daily wood use per household. Default "XMijk"
#' @param groupvar Character. Grouping variable. Default "households"
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return data.frame
#' @export

#* @post /calculateXMi
#* @get /calculateXMi
#* @serializer switch

calculateXMi <- function(data , XMijk = "XMijk", groupvar = "households",
                         format = NULL,
                         web3 = FALSE){
  data %>%
    group_by(!!sym(groupvar)) %>%
    summarise(XMi = mean(!!sym(XMijk), na.rm = TRUE)) %>%
    summarise(estimate = mean(XMi)) %>%
    as.data.frame() %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}

#' simulateXMi
#' @description
#' Function for use with simulated data selecting the kitchen test group (Z_kt == 1)
#' and the project scenario test phase (Z_pw == 1)
#' @param data tibble
#' @param selection_statement Character string of expression to be evaluated to select data.
#' Dafault "Z_kt == 1 &  Z_pw == 1"
#' @param ... passed to calculateXMi
#'
#' @return data.frame
#' @export
#'
#' @examples
#' est_XMi <- declare_estimator(handler = label_estimator(simulateXMi), label = "XMi", inquiry = "XMi")

simulateXMi <- function(data , selection_statement = "Z_kt == 1 &  Z_pw == 1", ...){

  data2 <- data %>% filter(eval(rlang::parse_expr(selection_statement)))

  calculateXMi(data2, ...)
}
