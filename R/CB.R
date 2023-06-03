

simulateCB <- function(data , XPijk = "XPijk",
                       XBijk = "XBijk",
                       frMijk = "frMijk",
                       frBPijk = "frBPijk",
                       frBijk = "frBijk",
                       frPijk = "frPijk",
                       groupvar = "households",
                       stratvar = "Z",
                       NDPi = NULL,
                       ...){
  CP <- simulateCP(data = data, ...)
  eef <- simulateEEFwithRR(data) %>% pull(eef)
  CB <- CP * eef
  data.frame(CB)
}
