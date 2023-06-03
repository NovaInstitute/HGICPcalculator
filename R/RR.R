





#' calculateRRj
#' @description
#' Calculate replacement rate per household
#'
#' @param data
#' @param groupvar
#' @param frMijk
#' @param frBijk
#' @param frPijk
#'
#' @return tibble with five column: <groupvar> meanfrBij meanfrMij meanfrPij    rr
#' @export

calculateRRj <- function(data,
                        groupvar = "households",
                        frMijk = "frMijk",
                        frBijk = "frBijk",
                        frPijk = "frPijk"){

  data %>% group_by(!!sym(groupvar)) %>%
    summarise(meanfrBij = mean(frBijk, na.rm = TRUE),
              meanfrMij = mean(frMijk, na.rm = TRUE),
              meanfrPij = mean(frPijk, na.rm = TRUE),
              rr = (meanfrBij - (meanfrPij - meanfrMij)) / meanfrMij
    )
}


#' simulateRRj
#'
#' @param data
#' @param selection_statement Character. filter exporession: Default :"Z_kt == 1 &  Z_pw == 1"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

simulateRRj <- function(data, selection_statement = "Z_kt == 1 &  Z_pw == 1", ...){

  data1 <- data %>% filter(eval(rlang::parse_expr(selection_statement)))
  calculateRRj(data1, ...)


}
