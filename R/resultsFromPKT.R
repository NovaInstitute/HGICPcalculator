#' Title
#'
#' @param projectKT tibble or character. If character assumes it is a web3storage address of a tibble
#' @param place Character. Column name in projectKT with place
#' @param year Character. Column name in projectKT with year
#' @param households Character. Column name in projectKT with initial household ID
#' @param frPijk Character. Column name in projectKT with frequency of wood use in project scenario
#' @param frMijk Character. Column name in projectKT with frequency of wood use in project devices in the project scenario
#' @param frBPijk Character. Column name in projectKT with frequency of wood use in baseline devices project scenario
#' @param XMijk Character. Column name in projectKT with mean wood use in project devices in the project scenario
#' @param XBPijk Character. Column name in projectKT with mean wood use in baseline devices in the project scenario
#' @param XPijk Character. Column name in projectKT with mean wood use in ???? all ???? devices in the project scenario
#' @param fuel Character. Column name to be created for fuel type
#' @param fuelval Character. Value to set fuel to. Default "wood"
#' @param assignment Character. Column name to be created for assignment
#' @param assignmentval Character. Value to set fuel to. Default "projectKT"
#' @param household_qr_code Character. Name to use for the ID column. Default household_qr_code
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Logical. Return web3storage address or not
#'
#' @return
#' @export
#'
#' @examples
#' x2 <- "https://bafkreihpanak7mlugp6ee2imisckpwrssc4qnyw2xwpgglvfuejhj3nsxi.ipfs.w3s.link/""
#' resultsFromPKT(x2, format = "htmlTable")

#* @get /resultsFromPKT
#* @serializer switch


resultsFromPKT <- function(projectKT,
                           place = "place",
                           year = "year",
                           households = "households",
                           frPijk = "frPijk",
                           frMijk = "frMijk",
                           frBPijk = "frBPijk",
                           XMijk = "XMijk",
                           XBPijk = "XBPijk",
                           XPijk = "XPijk",
                           fuel = "fuel",
                           fuelval = "wood",
                           assignment = "assignment",
                           assignmentval = "projectKT",
                           household_qr_code = "household_qr_code",
                           format = NULL,
                           web3 = FALSE) {

  if (is.character(projectKT))  {
    projectKT <- HGICPcalculator::web3S2R(projectKT)
    if (!is.data.frame(projectKT)) {stop("\nI was expecting projectKT to be the web3storrage address of a dataframe\n")}
  }

  out <- projectKT %>%
    select(!!place, !!year, !!households, !!frPijk, !!frMijk, !!frBPijk, !!XMijk, !!XBPijk, !!XPijk) %>%
    mutate(!!sym(fuel) := fuelval,
           !!sym(assignment) := assignmentval) %>%
    rename(!!household_qr_code := !!households) %>%
    group_by(place, year, fuel, assignment, household_qr_code) %>%
    mutate(XMij = units::as_units(XMijk / frMijk, "kg")) %>%
    summarise(
      XMij = mean(!!sym(XMijk), na.rm = TRUE),
      frMij = mean(!!sym(frMijk), na.rm = TRUE),
      frBPij = mean(!!sym(frBPijk), na.rm = TRUE),
      XBPijk = mean(!!sym(XBPijk), na.rm = TRUE)) %>%
    group_by(place, year, fuel, assignment, household_qr_code) %>%
    mutate(kg_p_month_m2 = units::as_units(XMij * 364.25/12, "kg")) %>%
    switchify(format = format) %>%
    web3lify(web3 = web3)

}
