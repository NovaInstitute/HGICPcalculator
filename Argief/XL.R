

#' calculateXl
#' @description
#' Function to calculate leakage wood use, i.e. wood use from baseline technologies in the
#' project scenario. This is done by applying the fuel per fire derived from the baseline
#' kitchen test and the frequency of fires using the baseline technologies in the project scenario.
#' @param data tibble
#' @param XPijk Character. . Default "XPijk"
#' @param XBijk Character. . Default "XBijk"
#' @param frMijk Character. . Default "frMijk"
#' @param frPijk Character. . Default "frPijk"
#' @param groupvar Character. Grouping variable. Default "households"
#' @param KPTValue Numeric or NULL. If NULL it is calculated from the same dataset
#' @param format Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"
#' @param web3 Logical. Return web3storage address or not
#' @param ... Arguments passed to simulateXBi
#'
#' @return data.frame
#' @export

calculateXl <- function(data,
                        XPijk = "XPijk",
                        XBijk = "XBijk",
                        frMijk = "frMijk",
                        frPijk = "frPijk",
                        groupvar = "households",
                        KPTValue = NULL,
                        format = NULL,
                        web3 = FALSE,
                        ...){

if (is.null(KPTValue)){ KPTValue <-  simulateXBi(data, ...) %>% pull() }

data %>%
  group_by(!!sym(groupvar)) %>%
  summarise(frMij = mean(!!sym(frMijk)),
            frPij = mean(!!sym(frPijk))
  ) %>%
  rowwise() %>%
  mutate(XBi = KPTValue,
         XLij = (XBi * (frPij - frMij))
  ) %>%
  ungroup() %>%
  summarise(estimate = mean(XLij, na.rm = TRUE)) %>%
  as.data.frame()  %>%
  switchify(format = format) %>%
  web3lify(web3 = web3)
}

#' simulateXl
#' @description
#' Function to simulate Leakage use for instead of calculateXl with simulated data.
#' Isolates baseline kitchen test households and period for the calculation of baseline wood
#' consumption per fire and then isolates the continuous monitoring households in continuous
#' monitoring phase for calculation fo the frequency of fires in the project scenario
#'
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
simulateXl <- function(data,
                       selection_statement1 = "Z_kt == 1 & Z_bw == 1",
                       selection_statement2 = "Z_cm == 1 & Z_cmw == 1",
                       ...){

  data1 <- data %>% filter(eval(rlang::parse_expr(selection_statement1))) # Baseline phase of the kitchen test for kitchen test households
  data2 <- data %>% filter(eval(rlang::parse_expr(selection_statement2))) # Continuous monitoring households in continuous monitoring phase

  KPTValue <-  simulateXBi(data1, ...) %>% pull()

  calculateXl(data2, KPTValue = KPTValue)

}



simulateCL <- function(data,
                       NDPi = NDP,... ){

  if (is.null(NDPi)) NDPi = nrow(data)

  XMi <- simulateXl(data) %>% pull()  %>% units::as_units("kg")

  data.frame(estimate = NDPi * XMi)
}


