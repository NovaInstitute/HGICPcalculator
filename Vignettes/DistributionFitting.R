#library(fitdistrplus)
library(tidyverse)
library(logspline)

if (Sys.info()[["user"]] == "christiaanpauw") {
  load("Rda/BaselineKitchenTest/BKTres.Rda")
} else {
  load("R/Data/res.Rda")}

#x <- res %>% select(data) %>% unnest(data) %>%
#  pull(qr_code_household,kg_p_fire) %>% na.omit()

#Create firelog to get number of fires per day per hh

firelog_base <- res %>% select(household) %>% unnest(household) %>%
  select(qr_code_household, instanceid, date) %>%
  mutate(date = case_when(date == as.Date("2022-04-25") ~ as.Date("2023-04-25"), TRUE ~ date)) %>%
  group_by(qr_code_household, date) %>% tally()

qrtibble <- tibble(qr_code_household = rep(unique(firelog_base$qr_code_household),
                         each = max(firelog_base$date)-min(firelog_base$date)+1),
                   date = rep(seq.Date(min(firelog_base$date),
                                       to = max(firelog_base$date), by = 1),20))

firelog <- qrtibble %>% left_join(firelog_base) %>% replace_na(list(n = 0))

#Create dfkg to get kg per fire

dfkg_base <-  res %>% select(data) %>% unnest(data)  %>%
  mutate(kg_p_fire = ifelse(is.infinite(kg_p_fire), NA, kg_p_fire))

dfkg <- dfkg_base %>%
  mutate(kg_p_fire = ifelse(is.infinite(kg_p_fire), NA, kg_p_fire)) %>%
  group_by(qr_code_household) %>%
  filter(!is.na(kg_p_fire) & kg_p_fire > 0 )

#Graph of kg per fire for whole group
kgg1 <- ggplot(data = dfkg, aes(x = kg_p_fire))+
  geom_density()
kgg1

#Graph of kg per fire per household
kgg2 <- ggplot(data = dfkg, aes(x = kg_p_fire))+
  geom_density()+
  facet_wrap(vars(qr_code_household), nrow = 5, scales = "free_x")
kgg2

#Graph of number of fire for whole group
fg1 <- ggplot(data = firelog, aes(x = n))+
  geom_bar()
fg1

#Graph of kg per fire per household
fg2 <- ggplot(data = firelog, aes(x = n))+
  geom_bar()+
  facet_wrap(vars(qr_code_household), nrow = 5)
fg2

#Distribution fitting for kg_per _fire

fitdistrplus::descdist(dfkg$kg_p_fire, discrete = FALSE)

fit.gamma <- fitdistrplus::fitdist(dfkg$kg_p_fire, "gamma")
plot(fit.gamma, sub = "Gamma")

fit.weibull <- fitdistrplus::fitdist(dfkg$kg_p_fire, "weibull")
plot(fit.weibull, sub = "Weibul")

fit.lognormal <- fitdistrplus::fitdist(dfkg$kg_p_fire, "lnorm")
plot(fit.lognormal, sub = "LogNorm")

fit.normal <- fitdistrplus::fitdist(dfkg$kg_p_fire, "norm")
plot(fit.normal, sub = "Normal")

fit.gamma$aic
fit.weibull$aic
fit.lognormal$aic
fit.normal$aic

kgparm_B <- MASS::fitdistr(dfkg$kg_p_fire, "weibull")
kgparm <- MASS::fitdistr(dfkg$kg_p_fire, "gamma")

#Distribution fitting for number of fires per day

#fitdistrplus::descdist(dfnfires$number_entires_firelog, discrete = TRUE)

fit.Poisson <- fitdistrplus::fitdist(firelog$n, "pois")
plot(fit.Poisson)

fit.negbin <- fitdistrplus::fitdist(firelog$n, "nbinom")
plot(fit.negbin)

fit.normal <- fitdistrplus::fitdist(firelog$n, "norm")
plot(fit.normal)

fit.Poisson$aic
fit.negbin$aic
fit.normal$aic

nfireparm <- MASS::fitdistr(firelog$n, "Poisson")
MASS::fitdistr(firelog$n, "negative binomial")

firelog_B <- firelog$n
firelog_M <- rpois(n = 420, lambda = 1.35) # moet nog uit data bereken word

fn = "Rda/Simulation/parameters.Rda"

save(kgparm, kgparm_B, nfireparm, firelog_B, firelog_M, file = fn)

#As data inkom vir project kitchen test, bereken frM/frP en pas beta verdeling. Gebruik in
#simulation om frMijk te kry.
