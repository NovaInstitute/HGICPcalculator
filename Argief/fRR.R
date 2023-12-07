#' fRR
#'
#' @param df data.frame
#' @param frPij Character
#' @param fRMij Character
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#' @return
#' @export

calculateFRR <- function(df = NULL,
                frBPij = "frBPij",
                frMij = "frMij",
                group_vars = c("place", "year", "fuel"),
                format = NULL,
                web3 = FALSE
                ){
  df %>%
    group_by(!!!syms(group_vars)) %>%
    select(!!frBPij, !!frMij) %>%
    mutate(frr = !!sym(frBPij) / !!sym(frMij)) %>%
    summarise(frrbar = mean(frr))  %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)

}
