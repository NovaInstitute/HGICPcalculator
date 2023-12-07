
#' calculateXMB
#' @description
#' Calculate xBi xMi
#'
#' @param data
#' @param groupvar Character. Grouping variable. Default: "households"
#' @param XBijk Character. Default: "XBijk"
#' @param XMijk Character. Default: "XMijk"
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#'
#' @return tibble with one row and two columns namely "xBi" and "xMi"
#' @export
#'
#' @examples

#* @post /calculateXMB
#* @get /calculateXMB
#* @serializer switch

calculateXMB <- function(data,
                         groupvar = "households",
                         XBijk = "XBijk",
                         XMijk = "XMijk",
                         format = NULL,
                         web3 = FALSE){
  data %>% group_by(!!!syms(groupvar)) %>%
    summarise(xBij = mean(XBijk, na.rm = TRUE),
              xMij = mean(XMijk, na.rm = TRUE)) %>%
    summarise(xBi = mean(xBij, na.rm = TRUE),
              xMi = mean(xMij, na.rm = TRUE)) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}

#' simulateXMB
#'
#' @param data
#' @param selection_statement. Character representation of filtering expression:
#' Default:  "Z_kt == 1 &  Z_pw == 1"
#' @param ...
#'
#' @return tibble
#' @export
#'
#' @examples
simulateXMB <- function(data, selection_statement = "Z_kt == 1 &  Z_pw == 1", ...){

  # XBi kom uit baseline week en XMi kom uit project week?
  data1 <- data %>% filter(eval(rlang::parse_expr(selection_statement)))

  calculateXMB(data1, ...)
}


