library(CTOWWS)
library(tidyverse)
library(DBI)
library(units)
library(openxlsx)

load("data/SimulatedSampleResults.Rda")
ERstartdate <- as.Date("2023-01-01")
ERenddate <- as.Date("2023-05-31")

dfresB <- baselineKT %>%
  select(place, year, households, XBijk, frBijk) %>%
  mutate(fuel = "wood",
         assignment = "baselineKT") %>%
  rename("household_qr_code" = "households") %>%
  group_by(place, year, fuel, assignment, household_qr_code) %>%
  mutate(XBij = units::as_units(XBijk / frBijk, "kg")) %>%
  summarise(
    XBij = mean(XBijk),
    frBij = mean(frBijk, na.rm = TRUE)) %>%
  group_by(place, year, fuel, assignment, household_qr_code) %>%
  mutate(kg_p_month_m2 = units::as_units(XBij * 364.25/12, "kg"))

dfresP <- projectKT %>%
  select(place, year, households, frPijk, frMijk, frBPijk, XMijk, XBPijk, XPijk) %>%
  mutate(fuel = "wood",
         assignment = "projectKT") %>%
  rename("household_qr_code" = "households") %>%
  group_by(place, year, fuel, assignment, household_qr_code) %>%
  mutate(XMij = units::as_units(XMijk / frMijk, "kg")) %>%
  summarise(
    XMij = mean(XMijk, na.rm = TRUE),
    frMij = mean(frMijk, na.rm = TRUE),
    frBPij = mean(frBPijk, na.rm = TRUE),
    XBPijk = mean(XBPijk, na.rm = TRUE)) %>%
  group_by(place, year, fuel, assignment, household_qr_code) %>%
  mutate(kg_p_month_m2 = units::as_units(XMij * 364.25/12, "kg"))

dfCOEF <- tibble(fuel = c("wood", "charcoal"), COEF = c(units::as_units(1560, "g/kg"), units::as_units(2860, "g/kg")))
dffNRB <- tibble(place = "Lwandlamuni", year = c(2023, 2024), fNRB = c(0.3, 0.31))

dfDateRef <- tibble(
  date = seq.Date(as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = 1)) %>%
  mutate(year = lubridate::year(date),
         week = lubridate::week(date),
         weekday = lubridate::wday(date, label = TRUE))

dfFreqRes <- ConMon %>%
  mutate(fuel = "wood",
         assignment = "ConMon") %>%
  rename("household_qr_code" = "households") %>%
  select(place, year, week, weekday, fuel, assignment, household_qr_code, frMijk) %>%
  mutate(frMijk = round(frMijk)) %>%
  right_join(dfDateRef) %>%
  filter(date >= ERstartdate & date <= ERenddate) %>%
  group_by(place, year, fuel, assignment, household_qr_code) %>%
  summarise(ndays = length(date),
            total_frMij = sum(frMijk),
            date_start_monitoring_period = min(date),
            date_end_monitoring_period = max(date)) %>%
  mutate(frMij = total_frMij / ndays)

# Frequency approach ------------------------------------------------------
## Kg per fire for each place, fuel type and season
dfKg.p.f_B <- dfresB %>% group_by(place, year, fuel, assignment) %>% summarise(XBi = mean(XBij, na.rm = TRUE))
dfKg.p.f_P <- dfresP %>% group_by(place, year, fuel, assignment) %>% summarise(XMi = mean(XMij, na.rm = TRUE))

# Save simulated data artefacts -------------------------------------------

save(dfFreqRes, dfKg.p.f_B, dfKg.p.f_P, dfresB, dfresP, dfCOEF, dffNRB, file = "data/SimulatedMonitoringData.Rda")

