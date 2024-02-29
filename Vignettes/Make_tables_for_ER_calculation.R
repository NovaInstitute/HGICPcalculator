library(knitr)
library(tidyverse)
library(kableExtra)
library(units)
library(HGICPcalculator)
library(jellyfi3shR)

#Load data from package KitchenTests
path1 = "C:/Users/User/Documents/NOVA/KitchenTests/Rda/"
path2 = "C:/Users/User/Documents/NOVA/ContinuousCookstoves/Data/"

local = TRUE

if (local){
  load(paste0(path1, "AllResults.Rda"))
  load(paste0(path1, "AllCombinedResults.Rda"))
  dfConMon <- readRDS(paste0(path2,"dfCM.Rds"))
}

# Set the parameters
dfCOEF <- tibble(fuel = c("wood", "charcoal"), COEF = c(units::as_units(1560, "g/kg"), units::as_units(2860, "g/kg")))
dffNRB <- tibble(place = "Lwandlamuni", year = c(2023, 2024), fNRB = c(0.3, 0.31))

# Calculation of C(P) the total fuel consumption in the project scenario for subpopulation i

## Calculation of xbar(M)i

## Calculation of fr*bar(M)i
#ConMonij <- make_dfConMonij() %>% select(-data)

## Calculation of dbar i

## Calculation of C(P)i

dfCPi <- make_dfCPi(dfConMon = dfConMon, KTresults = dfresBP)


# Calculation of baseline CO~2~ emissions

## Calculation of rrij and eefij
RR_EEFij <- make_dfRRij_EEFij(dfresBP)
RR <- calculateRRij(data = dfresBP)
#EEF <- calculateEEFfromRR(data = RR, minfr = 5)

## Calculation of xbar(B)i

## Calculation of eefij

## Calculation of C(B)
dfCBi <- make_dfCBi(minfr = 5)


#Calculation of C(L)

## Calculation of frr_bar
dfFrRij <- make_dfFrRij()

## Calculation of L(P)i
#dfCLi <- make_dfCLi()

#Create table dfCy with Project Emissions PE, Baseline Emissions BE, and Leakage Emissions LE
dfCy <- dfCPi %>%
  select(place, year, fuel, date_start_monitoring_period, date_end_monitoring_period, CPi) %>%
  left_join(dfCBi %>% select(place, year, fuel, date_start_monitoring_period, date_end_monitoring_period, CBi)) %>%
#  left_join(dfCLi %>% select(place, year, fuel, date_start_monitoring_period, date_end_monitoring_period, CLi)) %>%
  group_by(place, year, fuel)
PE <- calculateE(Cy = dfCy, var = "CPi", outcome = "PEi", onlyOutcomeAndGroups = TRUE)
BE <- calculateE(Cy = dfCy, var = "CBi", outcome = "BEi", onlyOutcomeAndGroups = TRUE)
#LE <- calculateE(Cy = dfCy, var = "CLi", outcome = "LEi", onlyOutcomeAndGroups = TRUE)

ER <- calculateER(BE, PE, LE = NULL)

#save(dfCPi, ConMonij,RR_EEFij,dfCBi, dfCLi,dfFrRij, dfCy,
#     PE,BE, LE, ER, file = "Rda/Tables_for_ER_calculation.Rda")

save(dfConMon, dfCPi, RR_EEFij,dfCBi, dfFrRij, dfCy,
     PE,BE,  ER, file = "Rda/Tables_for_ER_calculation.Rda")

