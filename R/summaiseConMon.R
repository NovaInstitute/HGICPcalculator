


#' summariseConMon
#' @description
#' Summarise a tibble with daily fire counts for project measures to a total and average count for the monitoring period
#' @param dfFreqRes_prep tibble or character. Assumes a web3Storage address if a character
#' @param household_qr_code Character. Column in dfFreqRes_prep with household ID
#' @param date Character. Column in dfFreqRes_prep with date
#' @param frMijk Character. Column in dfFreqRes_prep with fires per day from project measures
#' @param place Character. Column in dfFreqRes_prep with place
#' @param year Character. Column in dfFreqRes_prep with year
#' @param fuel Character. Column in dfFreqRes_prep with fuel
#' @param assignment Character. Column in dfFreqRes_prep with assignment (i.e. monitoring phase)
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @return
#' @export
#'
#' @examples
#' summariseConMon("https://bafkreiasme5plzoyimo3k2r33tpbtjndsf7ykch7ihkctx5em23fffskza.ipfs.w3s.link/")

#* @apiTitle summariseConMon
#* @get /summariseConMon
#* @serializer switch

summariseConMon <- function(dfFreqRes_prep = NULL,
                            household_qr_code = "household_qr_code",
                            date = "date",
                            frMijk = "frMijk",
                            place = "place",
                            year = "year",
                            fuel = "fuel",
                            assignment = "assignment",
                            format = NULL) {

  if (is.character(dfFreqRes_prep))  {
    dfFreqRes_prep <- HGICPcalculator::web3S2R(dfFreqRes_prep)
    if (!is.data.frame(dfFreqRes_prep)) {stop("\nI was expecting baselineKT to be the web3storrage address of a dataframe\n")}
  }

  dfFreqRes_prep %>%
    filter(!is.na(!!sym(household_qr_code))) %>%
    summarise(ndays = length(!!sym(date)),
              total_frMij = sum(!!sym(frMijk)),
              date_start_monitoring_period = min(!!sym(date)),
              date_end_monitoring_period = max(!!sym(date))) %>%
    mutate(frMij = total_frMij / ndays) %>%
    select(!!place, !!year, !!fuel, !!assignment, !!household_qr_code,
           ndays, total_frMij, frMij,
           date_start_monitoring_period, date_end_monitoring_period) %>%
    switchify(format = format)
}
