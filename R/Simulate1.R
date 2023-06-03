
library(tidyverse)
library(CTOWWS)
library(DeclareDesign)
library(units)
source("~/GuardianCookstovePolicy/CookstoveGuardian/calculateFrM.R")
load(file = "R/Data/parameters.Rda")

# 1 year
# 1 subpopulation

# j = households 1:100
# k = yday 1:365

Nhh = 50
Bfrq = nfireparm$estimate
BaveKg = 8 # mean of household averages (x bar bar)
BavekgSD = 4 # SD of household averages
BSDkgSD = 2 #  SD of household SD
obsmin = 2 # observed minimum
obsmax = 16 # observed ma
Nexp = 20 # N of experimental group

Pfrq = 1
PaveKg = 6 # mean of household averages (x bar bar)
PavekgSD = 2 # SD of household averages
PSDkgSD = 1 #  SD of household SD
PropP = 0.9

test_duration = 2 # in weeks

dffNRB = tibble(place =  "Lwandlamuni", year = 2023, fNRB = 0.5)
dfCOEF = tibble(fuel = "wood", COEF = units::as_units(1560, "g/kg"))

model <- declare_model(
   households = add_level(
     N = Nhh,
     fuel = "wood",
     place = "Lwandlamuni",
     year = 2023,
     aveFrBij = replicate(Nhh, mean(rpois(n = 364, lambda = Bfrq))),
     aveXBij  =  EnvStats::rlnormTrunc(n = Nhh,
                                        meanlog = log(BaveKg),
                                        sdlog = log(BavekgSD),
                                        min = obsmin,
                                        max = obsmax),
     sdXBij = sqrt( (rchisq(n = Nhh, df = (Nexp - 1)) * BSDkgSD*BSDkgSD ) / (Nexp - 1)),
     aveFrPij  = replicate(Nhh, mean(rpois(n = 364, lambda = (Pfrq) ))), # ave freq fire in P scenario
     aveFrMij  = rbeta(n = Nhh, shape1 = PropP, shape2 = 1-PropP) * aveFrPij,
     aveFrBPij = aveFrPij - aveFrMij, # laat dalk uit
     aveXPij = EnvStats::rlnormTrunc(n = Nhh,  # mean kg p f from project measures
                                       meanlog = log(PaveKg),
                                       sdlog = log(PavekgSD),
                                       min = obsmin,
                                       max = obsmax),
     sdXPij = sqrt( (rchisq(n = Nhh, df = (Nexp - 1)) * BSDkgSD*BSDkgSD ) / (Nexp - 1))
     ),
   week = add_level(
     N = 52,
     week = 1:52
   ),
   wday = add_level(
     N = 7,
     weekday = lubridate::wday(1:7, label = TRUE),
     frBijk = rpois(n = 364 * Nhh, lambda = aveFrBij),
     XBijk = frBijk * EnvStats::rlnormTrunc(n = Nhh,
                                         meanlog = log(aveXBij ),
                                         sdlog = log(sdXBij),
                                         min = obsmin,
                                         max = obsmax),
     #kg_p_fire_B = XBijk / frBijk,
     frMijk = rpois(n = 364 * Nhh, lambda = aveFrMij),
     frBPijk = rpois(n = 364 * Nhh, lambda = aveFrBPij),
     frPijk = frMijk + frBPijk,
     XMijk = frMijk * EnvStats::rlnormTrunc(n = Nhh,
                                                meanlog = log(aveXPij),
                                                sdlog = log(sdXPij),
                                                min = obsmin,
                                                max = obsmax),
     XBPijk = frBPijk * EnvStats::rlnormTrunc(n = Nhh,
                                              meanlog = log(aveXBij ),
                                              sdlog = log(sdXBij),
                                              min = obsmin,
                                              max = obsmax),
     XPijk = XMijk + XBPijk,
     aveXBPijk = XBPijk / frBPijk,
     aveXMijk = XMijk / frMijk,
     total_effect = XPijk - XBijk,
     eefijk = XBijk / XPijk
     )
   )

# skep 'n estimand or inquiry met die waardes wat gerapporteer word
# CBfj : kg_p_month in the baseline scenario. kan ons sommer net frBijk (per dag) gebruik
# CPfj : kg_p_month in the project scenario kan ons sommer net XPijk (per dag) gebruik
# eef :  paired: eef = base_kg / project_kg, independent: calculatEEF(CBfj = CBfj, CPfj = CPfj)

# CB,f,y = The total baseline consumption of fuel f for year y across all project participants

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
  Z_cmw = ifelse(week > (test_duration + test_duration), 1, 0),
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




