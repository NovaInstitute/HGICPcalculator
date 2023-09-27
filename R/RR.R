

#' calculateRRj
#' @description
#' Calculate replacement rate per household
#'
#' @param data
#' @param groupvar
#' @param frMijk
#' @param frBijk
#' @param frPijk
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return tibble with five column: <groupvar> meanfrBij meanfrMij meanfrPij    rr
#' @export

#* @post /calculateRRj
#* @get /calculateRRj
#* @serializer switch

calculateRRj <- function(data,
                        groupvar = "households",
                        frMijk = "frMijk",
                        frBijk = "frBijk",
                        frPijk = "frPijk",
                        format = NULL,
                        web3 = FALSE){

  data %>% group_by(!!!syms(groupvar)) %>%
    summarise(frBij = mean(frBijk, na.rm = TRUE),
              frMij = mean(frMijk, na.rm = TRUE),
              frPij = mean(frPijk, na.rm = TRUE),
              rr = (frBij - (frPij - frMij)) / frMij
    )  %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
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
