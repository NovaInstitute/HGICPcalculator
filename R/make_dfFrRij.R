#' make_dfFrRij
#' @description
#' Calculates the average frequency ratio of baseline fires to project measures fires in the project phase of the KT
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

#* @post /make_dfFrRij
#* @get /make_dfFrRij
#* @serializer switch

make_dfFrRij <- function(df = dfresP,
                         indexvars = c("place", "year",  "fuel"),
                         frMij. = "frMij.",
                         frBPij. = "frBPij.",
                         format = NULL,
                         web3 = FALSE){

  df <- df %>% ungroup() %>% select(!!!indexvars, !!frMij., !!frBPij.)
  df %>% mutate(FrRij = !!sym(frBPij.)/!!sym(frMij.)
  ) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}


