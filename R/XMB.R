
#' calculateXMB
#' @description
#' Calculate xBi xMi
#'
#' @param data
#' @param groupvar Character. Grouping variable. Default: "households"
#' @param XBijk Character. Default: "XBijk"
#' @param XMijk Character. Default: "XMijk"
#'
#' @return tibble with one row and two columns namely "xBi" and "xMi"
#' @export
#'
#' @examples
calculateXMB <- function(data,
                         groupvar = "households",
                         XBijk = "XBijk",
                         XMijk = "XMijk"){
  data %>% group_by(!!sym(groupvar)) %>%
    summarise(xBij = mean(XBijk, na.rm = TRUE),
              xMij = mean(XMijk, na.rm = TRUE)) %>%
    summarise(xBi = mean(xBij, na.rm = TRUE),
              xMi = mean(xMij, na.rm = TRUE))
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


