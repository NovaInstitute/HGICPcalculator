
#' resultsFromBKT
#'
#' @param baselineKT tibble or character. If character assumes it is a web3storage address of a tibble
#' @param place Character
#' @param year Character
#' @param households Character
#' @param XBijk Character
#' @param frBijk Character
#' @param fuelval Character
#' @param assval Character
#' @param household_qr_code Character
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#' @return tibble
#' @export
#'
#' @examples
#' x <- "https://bafkreib7u7afwxo4gwyn46nlpuxuh7zzacqcgsy5p6nop75xskmrqlnbpe.ipfs.w3s.link/"
#' resultsFromBKT(x, format = "htmlTable")

#* @post /resultsFromBKT
#* @get /resultsFromBKT
#* @serializer switch

resultsFromBKT <- function(baselineKT = NULL,
                           place = "place",
                           year = "year",
                           households = "households",
                           XBijk = "XBijk",
                           frBijk = "frBijk",
                           fuel = "fuel",
                           fuelval = "wood",
                           assignment = "assignment",
                           assignmentval = "baselineKT",
                           format = NULL,
                           web3 = FALSE
                    ){
  if (is.character(baselineKT))  {
    baselineKT <- HGICPcalculator::web3S2R(baselineKT)
    if (!is.data.frame(baselineKT)) {stop("\nI was expecting baselineKT to be the web3storrage address of a dataframe\n")}
  }

  out <- baselineKT %>%
    select(!!place, !!year, !!households, !!XBijk, !!frBijk) %>%
    # mutate(fuel = fuelval,
    #        assignment = assval) %>%
    # rename("household_qr_code" = !!households) %>%
    mutate(!!sym(fuel) := fuelval,
           !!sym(assignment) := assignmentval) %>%
    rename(!!household_qr_code := !!households) %>%
    group_by(place, year, fuel, assignment, household_qr_code) %>%
    mutate(XBij = units::as_units(!!sym(XBijk) / !!sym(frBijk), "kg")) %>%
    summarise(
      XBij = mean(!!sym(XBijk), na.rm = TRUE),
      frBij = mean(!!sym(frBijk), na.rm = TRUE)) %>%
    group_by(place, year, fuel, assignment, household_qr_code) %>%
    #group_by(!!!syms(place), !!sym(year), !!sym(fuel), !!sym(assignment), household_qr_code) %>%
    mutate(kg_p_month_m2 = units::as_units(XBij * 364.25/12, "kg")) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)
}

