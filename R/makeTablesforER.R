#' makeTablesforER
#' @description Calculates the tables that are included in the ER report
#' @param dfConMon df. Data frame with continuous monitoring data
#' @param dfresBP df. Data frame with combined results for the Baseline Kitchen Test and the Project Kitchen Test
#' @param startdate date. Start date for continuous monitoring
#' @param enddate date. End date for continuous monitoring
#' @param dfCOEF
#' @param dffNRB
#' @return
#' @export
#' @import dplyr
#' @examples


makeTablesforER <- function(dfConMon = NULL,
                            dfresBP = NULL,
                            startdate = lubridate::ymd("2023-06-22"),
                            enddate = lubridate::ymd("2023-08-08"),
                            dfCOEF = tibble(fuel = c("wood", "charcoal"), COEF = c(units::as_units(1560, "g/kg"), units::as_units(2860, "g/kg"))),
                            dffNRB = tibble(place = "Lwandlamuni", year = c(2023, 2024), fNRB = c(0.3, 0.3)),
                            path = "C:/Users/User/Documents/NOVA/HGICPcalculator/"
                            ){
if (is.null(dfConMon)) stop("You have to provide dfConMon")

if (is.null(dfresBP)) stop("You have to provide dfresBP")

  ##Check whether dfConMon is for correct dates
  if (min(dfConMon$date_start_monitoring_period) != startdate) {
    message("Error: The minimum start date of the monitoring period in dfConMon is not equal to the specified startdate.")
    return()
  }

  # Check if maximum date in df is equal to enddate
  if (max(dfConMon$date_end_monitoring_period) != enddate) {
    message("Error: The maximum end date of the monitoring period in dfConMon is not equal to the specified enddate.")
    return()
  }

## Calculation of C(P)i
dfCPi <- make_dfCPi(dfConMon = dfConMon, KTresults = dfresBP)

## Calculation of rrij and eefij
dfRR_EEFij <- make_dfRRij_EEFij(dfresBP)
RR <- calculateRRij(data = dfresBP)

## Calculation of C(B)
dfCBi <- make_dfCBi(dfEEF = dfRR_EEFij,dfCP = dfCPi, minfr = 5)

## Calculation of frr_bar
dfFrRij <- make_dfFrRij(dfresBP)

dfCy <- dfCPi %>%
  select(place, year, fuel, date_start_monitoring_period, date_end_monitoring_period, CPi) %>%
  left_join(dfCBi %>% select(place, year, fuel, date_start_monitoring_period, date_end_monitoring_period, CBi)) %>%
  group_by(place, year, fuel)
PE <- calculateE(Cy = dfCy, COEF = dfCOEF, fNRB = dffNRB, var = "CPi", outcome = "PEi", onlyOutcomeAndGroups = TRUE)

BE <- calculateE(Cy = dfCy, COEF = dfCOEF, fNRB = dffNRB,var = "CBi", outcome = "BEi", onlyOutcomeAndGroups = TRUE)

ER <- calculateER(BE, PE, LE = NULL)

file_to_save = gsub("-", "", paste0(path,"Rda/ERTables",startdate, "_", enddate,".Rda"))

save(dfConMon, dfresBP, dfCPi, dfRR_EEFij,dfCBi, dfFrRij, dfCy,
     PE,BE,  ER, dfCOEF, dffNRB, file = file_to_save)
return(file_to_save)
}

