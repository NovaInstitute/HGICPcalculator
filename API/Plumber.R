# plumber.R

library(plumber)

#* @apiTitle  Plumber API for HGICPcalculator
#* HGICPcalculator


# sien https://www.rplumber.io/articles/rendering-output.html
#' @get /echo
function() {
  Sys.time()
}

plumber::register_serializer("switch", HGICPcalculator::serializer_switch)
# get("serializers", envir = plumber:::.globals)

#* Return a data frame of random values
#* @param n size of data frame
#* @param format one of "json", "csv", "rds", "html" or "htmlTable
#* @serializer switch
#* @get /switchtest

function(n = 10, format = "json") {
  out <- data.frame(value = rnorm(n))
  attr(out, "serialize_format") <- format
  out %>% round(2)
}

#' @plumber
function(pr) {
  pr %>%
    # pr_mount("/HGICPcalculator/FrM", plumb("R/FrM.R")) %>%
    # pr_mount("/HGICPcalculator/Xbi", plumb("R/XBi.R")) %>%
    # pr_mount("/HGICPcalculator/Xmi", plumb("R/XMi.R")) %>%
    # pr_mount("/HGICPcalculator/XL", plumb("R/XL.R")) %>%
    # pr_mount("/HGICPcalculator/BE", plumb("R/BE.R")) %>%
    # pr_mount("/HGICPcalculator/ER", plumb("R/ER.R")) %>%
    # pr_mount("/HGICPcalculator/fRR", plumb("R/fRR.R")) %>%
    # pr_mount("/HGICPcalculator/CP", plumb("R/CP.R")) %>%
    # pr_mount("/HGICPcalculator/CB", plumb("R/CB.R")) %>%
    # pr_mount("/HGICPcalculator/LE", plumb("R/LE.R")) %>%
    # pr_mount("/HGICPcalculator/RR", plumb("R/RR.R")) %>%
    # pr_mount("/HGICPcalculator/XMB", plumb("R/XMB.R")) %>%
    # pr_mount("/HGICPcalculator/resultsFromBKT", plumb("R/resultsFromBKT.R")) %>%
    # pr_mount("/HGICPcalculator/resultsFromPKT", plumb("R/resultsFromPKT.R")) %>%
    # pr_mount("/HGICPcalculator/makeDateRef", plumb("R/makeDateRef.R")) %>%
    # pr_mount("/HGICPcalculator/summaiseConMon", plumb("R/summaiseConMon.R")) %>%
    # pr_mount("/HGICPcalculator/PE", plumb("R/PE.R")) %>%
  pr_post("/HGICPcalculator/calculateER", HGICPcalculator::calculateER)
    #pr_mount("/HGICPcalculator/calculateER", plumb(HGICPcalculator::calculateER)) #%>%
    #pr_mount("/HGICPcalculator/calculateCP", plumb(HGICPcalculator::calculateCP)) %>%
    #pr_mount("/HGICPcalculator/calculateE", plumb(HGICPcalculator::calculateE))

}

