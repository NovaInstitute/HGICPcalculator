library(tidyverse)
library(CTOWWS)
library(DeclareDesign)
library(units)

load(file = "Rda/Simulation/parameters.Rda")

source("R/Simulation/FrM.R")
source("R/Simulation/CB.R")
source("R/Simulation/CP.R")
source("R/Simulation/EEF.R")
source("R/Simulation/Leakage.R")
source("R/Simulation/RR.R")
source("R/Simulation/XBi.R")
source("R/Simulation/XMB.R")
source("R/Simulation/XMi.R")
source("R/Simulation/calculateBE.R")
source("R/Simulation/calculateER.R")
source("R/Simulation/simulationFunctions.R")

Nhh = 50
Bfrq = nfireparm$estimate
BKg_shape = kgparm_B$estimate["shape"]
BKg_scale = kgparm_B$estimate["scale"]
MKg_shape = 1.16 #  kgparm_M$estimate["shape"]  MOET NOG BEREKEN WORD
MKg_scale = 3.62  # kgparm_M$estimate["shape"] MOET NOG BEREKEN WORD
Nexp = 20 # N of experimental group
Pfrq = 1.2
PKg_shape = 2/3*kgparm$estimate["shape"]
PKg_scale = kgparm$estimate["scale"]
PropP = 0.9
test_duration = 2 # in weeks

dffNRB = tibble(place =  "Lwandlamuni", year = 2023, fNRB = 0.5)
dfCOEF = tibble(fuel = "wood", COEF = units::as_units(1560, "g/kg"))

model <- declare_model(
  households = fabricatr::add_level(
    N = Nhh,
    fuel = "wood",
    place = "Lwandlamuni",
    year = 2023
     ),
  week = fabricatr::add_level(
    N = 52,
    week = 1:52
  ),
  wday = fabricatr::add_level(
    N = 7,
    weekday = lubridate::wday(1:7, label = TRUE),
    frBijk = sample(firelog_B, size = 18200, replace = TRUE),
    gam = rgamma(n = 18200, shape = BKg_shape  , scale = BKg_scale),
    XBijk = frBijk * gam,
    frPijk = sample(firelog_M, size = 18200, replace = TRUE),
    frMijk  = rbeta(n = Nhh, shape1 = PropP, shape2 = 1-PropP) * frPijk,
    frBPijk = frPijk - frMijk,
    XMijk = frMijk * rgamma(n = 18200, shape = MKg_shape  , scale = MKg_scale),
    XBPijk = frBPijk * gam,
    XPijk = XMijk + XBPijk,
    aveXBPijk = XBPijk / frBPijk,
    aveXMijk = XMijk / frMijk,
    total_effect = XPijk - XBijk,
    eefijk = XBijk / XPijk
  )
)

inq_ER <- declare_inquiries(
  CByf = round(sum(XBijk), 2) ,
  CPyf = round(sum(XMijk), 2),
  NDPi = length(households),
  XMi = mean(XMijk),
  XBi = mean(XBijk),
  aveFrM = mean(frMijk),
  CLf = 0,#round(sum(XBPijk * frBPijk), 2),
  BE = calculateE(Cy = dffNRB %>% mutate(fuel = "wood", B = as_units(CByf, "kg")),
                  var = "B",
                  outcome = "BE" ) %>% pull(BE) %>% as.numeric(),
  PE = calculateE(Cy = dffNRB %>% mutate(fuel = "wood", P = as_units(CPyf, "kg")),
                  var = "P",
                  outcome = "PE" ) %>% pull(PE) %>% as.numeric() ,
  LE = calculateE(Cy = dffNRB %>% mutate(fuel = "wood", L = as_units(CLf, "kg")),
                  var = "L",
                  outcome = "LE" ) %>% pull(LE) %>% as.numeric() ,
  ER = calculateER(BE = dffNRB %>% mutate(fuel = "wood", BE = as_units(BE, "kg")),
                   PE = dffNRB %>% mutate(fuel = "wood", PE = as_units(PE, "kg")),
                   LE = dffNRB %>% mutate(fuel = "wood", LE = as_units(LE, "kg"))
  ) %>%  pull(ER)
)

#ass <- declare_assignment(Z = conduct_ra(N = Nhh, clusters = households, m_each = c(20, 20, 10)))

ass <- declare_assignment(
  Z_kt = conduct_ra(N = Nhh, clusters = households, m_each = c(Nhh-20, 20)),
  Z_cm = conduct_ra(N = Nhh, clusters = households, m_each = c(Nhh-20, 20)),
  Z_bw = ifelse(week %in% 1:test_duration, 1, 0),
  Z_pw = ifelse(week %in% (1+test_duration):(test_duration + test_duration), 1, 0),
  Z_cmw = ifelse(week > (test_duration + test_duration), 1, 0)
)

# smp <- declare_sampling(S = cluster_rs(clusters = households, n = 20))

NDP <- nrow(model())

est_Frm <- declare_estimators(handler = label_estimator(simulateFrM), label = "frM", inquiry = "aveFrM")
est_XMi <- declare_estimator(handler = label_estimator(simulateXMi), label = "XMi", inquiry = "XMi")
est_XBi <- declare_estimator(handler = label_estimator(simulateXBi), label = "XBi", inquiry = "XBi")
est_CP <- declare_estimator(handler = label_estimator(simulateCP), label = "CP", inquiry = "CPyf")
est_CB <- declare_estimator(handler = label_estimator(simulateCB), label = "CB", inquiry = "CByf")
est_BE <- declare_estimator(handler = label_estimator(simulateBE), label = "BE_", inquiry = "BE")
est_PE <- declare_estimator(handler = label_estimator(simulatePE), label = "PE_", inquiry = "PE")
est_LE <- declare_estimator(handler = label_estimator(simulateLE), label = "LE_", inquiry = "LE")
est_ER <- declare_estimator(handler = label_estimator(simulateER), label = "ER_", inquiry = "ER")

model + ass + inq_ER + est_Frm + est_XMi + est_CP + est_CB + est_XBi + est_BE + est_PE + est_LE + est_ER
# Sampling for baseline KPT
# The baseline KPT observes FreqB and frBijk

# Create data
all <- model() %>% ass() %>% tibble()
baselineKT <- all %>% filter(Z_bw == 1 & Z_kt == 1)
projectKT  <- all %>% filter(Z_pw == 1 & Z_kt == 1) %>%
ConMon <- all %>% filter(Z_cmw == 1 & Z_cm == 1)

save(all, baselineKT, projectKT, ConMon, file = "Rda/Simulation/SimulatedSampleResults.Rda")

