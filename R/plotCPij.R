

#' plotCPij
#'
#' @param CPij tibble
#' @param place Character. Name of variable indicating the place
#' @param year Character. Name of variable indicating the year
#' @param fuel Character. Name of variable indicating the fuel type
#' @param assignment Character. Name of variable indicating the Kitchen test phase
#' @param household_qr_code Character. Name of variable indicating the household ID
#' @param kgvars Character. Name of variable measured in kg per day (XMij", "XBPijk)
#' @param monthvars Character. Name of variable indicating the monthly wood use
#' @param freqvars Character. Name of variable measured in frequency per day
#'
#' @return
#' @export
#'
#' @examples
plotCPij <- function(CPij,
                     place = "place",
                     year = "year",
                     fuel = "fuel",
                     assignment = "assignment",
                     household_qr_code = "household_qr_code" ,
                     kgvars =  c("XMij", "XBPijk"),
                     monthvars = c("kg_p_month_m2k"),
                     freqvars = c("frBPij", "frMij")){
  p <- CPij %>% units::drop_units() %>%
    pivot_longer(cols = -c(!!sym(place), !!sym(year), !!sym(fuel), !!sym(assignment), !!sym(household_qr_code)),
                 names_to = "variable") %>%
    mutate(unit = case_when(variable %in% kgvars ~ "kg/day",
                            variable %in% monthvars ~ "kg/month",
                            variable %in% freqvars ~ "frequency per day"
                            )) %>%
    ggplot(aes(x = !!sym(household_qr_code), y = value, group = variable, fill = variable)) +
      geom_col(aes(y = value)) +
      facet_wrap(unit~., scales = "free", nrow = 3) +
    theme(legend.position = "bottom")

    plotly::ggplotly(p)
}
