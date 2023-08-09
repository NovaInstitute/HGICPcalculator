library(CTOWWS)
library(tidyverse)
library(DBI)
library(units)
library(openxlsx)

load("data/SimulatedSampleResults.Rda")
ERstartdate <- as.Date("2023-01-01")
ERenddate <- as.Date("2023-05-31")

# HASH_baselineKT <- HGICPcalculator::R2web3S(baselineKT)
# HASH_projectKT <- HGICPcalculator::R2web3S(projectKT)

dfresB <- HGICPcalculator::resultsFromBKT(baselineKT)
dfresP <- HGICPcalculator::resultsFromPKT(projectKT)

# Make reference data
dfCOEF <- tibble(fuel = c("wood", "charcoal"), COEF = c(units::as_units(1560, "g/kg"), units::as_units(2860, "g/kg")))
dffNRB <- tibble(place = "Lwandlamuni", year = c(2023, 2024), fNRB = c(0.3, 0.31))
dfDateRef <- makeDateRef(startdate = ERstartdate, enddate = ERenddate)

# inspect
# ggplot(data = ConMon %>% rowwise() %>% mutate(date =as.Date(paste(year, week, weekday ), "%Y %U %a")), mapping = aes(x = date, y = frPijk)) + geom_point(size = I(0.1)) + facet_wrap(households~.)

# get simulated in correct format
dfFreqRes_prep <- ConMon %>%
  mutate(fuel = "wood",
         assignment = "ConMon") %>%
  rename("household_qr_code" = "households") %>%
  select(place, year, week, weekday, fuel, assignment, household_qr_code, frMijk) %>%
  mutate(frMijk = round(frMijk)) %>%
  right_join(dfDateRef) %>%
  filter(date >= ERstartdate & date <= ERenddate) %>%
  group_by(place, year, fuel, assignment, household_qr_code)

# HASH_dfFreqRes_prep <- R2web3S(dfFreqRes_prep)

dfFreqRes <- summariseConMon(dfFreqRes_prep)

# Frequency approach ------------------------------------------------------
## Kg per fire for each place, fuel type and season
dfKg.p.f_B <- dfresB %>% group_by(place, year, fuel, assignment) %>% summarise(XBi = mean(XBij, na.rm = TRUE))
dfKg.p.f_P <- dfresP %>% group_by(place, year, fuel, assignment) %>% summarise(XMi = mean(XMij, na.rm = TRUE))

# Save simulated data artefacts -------------------------------------------

save(dfFreqRes, dfKg.p.f_B, dfKg.p.f_P, dfresB, dfresP, dfCOEF, dffNRB, file = "data/SimulatedMonitoringData.Rda")

