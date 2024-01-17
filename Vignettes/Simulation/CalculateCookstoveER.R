# Libraries ---------------------------------------------------------------
rm(list = ls())
library(Guardener)
library(tidyverse)
library(HGICPcalculator)
library(readr)
library(ipfs)

# Load Demo Monitoring Data  ----------------------------------------------
load("data/SimulatedSampleResults.Rda")
load("data/SimulatedMonitoringData.Rda")
rm(all)

ERstartdate <- as.Date("2023-01-01")
ERenddate <- as.Date("2023-05-31")
dfDateRef <- makeDateRef(ERstartdate, ERenddate)
N = 1000

CBij <- dfresB %>% filter(assignment == "baselineKT") %>% select(kg_p_month_m2)
CPij <- dfresP %>% filter(assignment == "projectKT")
eef <- CTOWWS::calculatEEF(CBfj = CBij, CPfj = CPij) %>% filter(year == "2023")
# eef <- calculatEEFwithRR(data = projectKT, groupvar = "households") # !!! ons moet hierdie weer deurgaan
# !!! watter benaderting volg ons vir eef?

# Frequency Approach ------------------------------------------------------
### Calculate Project fuel use by multiplying mean fuel per fire with frequency
CPij_f <- dfFreqRes %>%
  filter(assignment == "ConMon") %>%
  select(-assignment) %>%
  left_join(dfKg.p.f_P %>% filter(assignment == "projectKT") %>% select(-assignment)) %>%
  mutate(CPi = frMij * XMi)

# Calculate ER ------------------------------------------------------------

CPy <- calculateCPy(CPy = CPij_f, dfFreq = ConMon, approach = "frequency", N = N)
CBy <- calculateCBy(eef = eef, CPy = CPij_f, approach = "frequency", N = N) # !!!!! hierdie het 'n globale eef gebruik -
# eintlik moet mens eef_i gebruik

frr <- calculateFRR(df = dfresP)
CLy <- calculateLPy(CPy = CPij, CMsummary = CPij_f, dfFreq = dfFreqRes, frr = frr, N = N)

BE <- calculateE(CBy, onlyOutcomeAndGroups = TRUE, COEF = dfCOEF, fNRB = dffNRB)
PE <- calculateE(CPy, var = "CP", outcome = "PE", onlyOutcomeAndGroups = TRUE)
LE <- calculateE(CLy, var = "LP", outcome = "LE", onlyOutcomeAndGroups = TRUE)
ER <- calculateER(BE, PE, LE)

ipfs::ipfs_daemon()
dfIPFS <- ls2ipfs() # add to IPFS

save(ER, BE, PE, LE, CLy, frr, CBy, CPy, CPij_f, eef, CPij, CBij, N, ERstartdate, ERenddate,
     dfCOEF, dffNRB, dfFreqRes, dfresP, dfresB,
     file = "data/SimulationERcalcs.Rda")

save(dfIPFS, file = "data/SimulationERcalcsIPFS.Rda")


# Make Guardian Ready -----------------------------------------------------
ATL <- Glogin(un = "StandardRegistry", pw =  "test")
AT <- ATL$accessToken
dfSchemas <- GgetSchemas(AT,  returndf = TRUE)
dfTemplates <- dfSchemas %>%
  GmakeSchemaTemplate() %>%
  mutate(templ = map(data, ~names2tibble(.) )) %>%
  ungroup() %>%
  select(-data)
dfPolicies <- GgetPolicies(AT, returndf = TRUE)
policyids <- dfPolicies$id
dfBlocks <- GgetPolicyBlocks(AT, policyId = policyids)

dfM <- dfTemplates %>% filter(name == "Monitoring Report (MR)") %>% unnest(templ)

dmt <- tibble(
  id_project = "Simulation",
  date_start_monitoring_period = CPij_f$date_start_monitoring_period %>% unique(),
  date_end_monitoring_period = CPij_f$date_end_monitoring_period %>% unique(),
  reductions_emissions = ER$ER[[1]] %>% units::drop_units(),
  datapar_monitored = tibble(
  data_parameter = "ER",
  data_unit = "tCO2eq",
  description = sprintf("ER %s to %s",
                        CPij_f$date_start_monitoring_period %>% unique(),
                        CPij_f$date_end_monitoring_period %>% unique()
                        ),
  source_of_data = "WWS and CM",
  values_applied = ER$ER[[1]] %>% units::drop_units(),
  methods_and_procedures = " ",
  monitoring_frequency  = "daily",
  qa_qc_procedures  = " ",
  purpose = " ",
  comment = " ")
)

# test
all(names(dmt) %in% names(dfM))

topicID <- dfSchemas %>% filter(uuid == dfM$uuid[[1]]) %>% select(topicId) %>% distinct() %>% pull()
contextURL <- dfSchemas %>% filter(uuid == dfM$uuid[[1]]) %>% select(-description) %>% unnest(document) %>% select(contextURL) %>% distinct() %>% pull()

blockInfo <- whichBlocksHasSchema(dfBlocks = dfBlocks, schemaId = dfM$uuid[[1]]) %>%
  filter(blockType == "requestVcDocumentBlock")

# inspect
blockInfo$tag
blockID <- blockInfo$id[[1]]
blockPolicy <- blockInfo$policyId[[1]]

Gpost2block(accessToken = Glogin()$accessToken,
            document = dmt,
            schemaID = dfM$uuid[[1]],
            schemaIPFSurl = ifelse(is.na(contextURL), " ", contextURL),
            policyId = blockPolicy,
            BlockId = blockID)

# Display
dfBPL <- left_join(eef, CBy) %>% left_join(CLy) %>% left_join(dfCOEF) %>% left_join(dffNRB)
BPL <- left_join(BE, PE) %>% left_join(LE)
