#' fRR
#'
#' @param df data.frame
#' @param frPij Character
#' @param fRMij Character
#' @return
#' @export


calculateFRR <- function(df = NULL,
                frBPij = "frBPij",
                frMij = "frMij",
                group_vars = c("place", "year", "fuel")
                ){
  df %>%
    group_by(!!!syms(group_vars)) %>%
    select(!!frBPij, !!frMij) %>%
    mutate(frr = !!sym(frBPij) / !!sym(frMij)) %>%
    summarise(frrbar = mean(frr))

}
