#' makedfConMonSD
#' @description Get output of readSD into a dataframe called dfNest with colums: sensor_location, qr_code_household, data
#' data has columns:  date <dttm>, temp <dbl>,  ibutton_id <chr> file_id <chr>
#'
#' @param df tibble. Output of readSD
#' @param datevar Character. Name of variable with date variable
#' @param fileidvar Character. Name of variable with file ID
#' @param id_var Character. Name of variable with household ID
#' @param dfHouse tibble with house - instrument pairings
#' @param ignorevalue Numeric. Value to ignore indicating sensor not working. Default is 0
#' @param sensor_id Character. Name of variable with sensor ID. Default is "sensor_id"
#' @param placevar Character. Name of variable with place. Default is "mainplace"
#' @param sensor_locationvar Character. Name of variable with sensor location. Default is "sensor_location"
#' @param valuevar Character. Name of variable with temperature. Default is "stove_temp"
#' @param refvar Character. Name of variable with reference temperature. Default is "box_temp"
#' @param minobs Numeric. Minimum number of observations to consider a day. Default is 24
#' @param startdatetime Daretime. Start date for the data. Default is 2023-06-22 00:00:00
#' @param returnFull Logical. If TRUE, returns the full dataframe with all columns. If false, returns only dfConMon format. Default is FALSE
#' @param fuelvar Character. Name of variable with fuel. Default is "fuel"
#'
#' @return tibble
#' @export
#' @examples
#' dft <- decodeSDcards(path = "Data/SDcards")
#' dfTemp <- dft %>% select(file_id, sensor_id, data12) %>% unnest(data12)
#' makedfConMonSD(df = dfTemp, dfHouse = dfSDhouse %>% filter(!is.na(mainplace)),datevar = "date", fileidvar = "file_id", sensor_location = "sensor_location", id_var = "qr_code_household", valuevar = "stove_temp")
#'
makedfConMonSD <- function(df = NULL,
                    dfHouse = NULL,
                    ignorevalue = 0,
                    minobs = 24,
                    startdatetime = ymd_hms("2023-06-22 00:00:00"),
                    returnFull = FALSE,
                    valuevar = "stove_temp",
                    sensor_id = "sensor_id",
                    datevar = "date",
                    placevar = "mainplace",
                    fileidvar = "file_id",
                    sensor_locationvar = "sensor_location",
                    id_var = "qr_code_household",
                    refvar = "box_temp",
                    fuelvar = "fuel"){
  if (is.null(dft)) stop("Please provide a tibble for argument df containing the temperature readings")
  if (is.null(dfHouse)) stop("Please provide a tibble for argument dfHouse containing the household info")

  dfFull <- df %>%
    mutate(day = yday(!!sym({{datevar}})),
           cycle = case_when( (day - lag(day) ) > 1 ~ "new",  TRUE ~ "same"),
           cycle = cumsum(cycle == "new")
           ) %>%
    left_join(dfHouse) %>%
    select({{placevar}},{{datevar}},{{valuevar}},{{sensor_id}},{{fileidvar}},{{sensor_locationvar}},{{id_var}},{{refvar}}, {{fuelvar}}, cycle) %>%
    arrange({{id_var}}, {{datevar}}) %>%
    mutate(year = lubridate::year(!!sym({{datevar}})),
           {{valuevar}} := as.numeric(!!sym({{valuevar}})),
           {{refvar}} := as.numeric(!!sym({{refvar}})),
           class = case_when(stove_temp >= 50 ~"fire",
                             stove_temp < 50 ~ "no_fire",
                             stove_temp == 0 ~ "malfunction")
           ) %>%
    filter(!!sym({{valuevar}}) != ignorevalue) %>%
    group_by(!!sym({{placevar}}), year, fuel, !!sym({{sensor_locationvar}}), !!sym({{id_var}}), cycle) %>%
    nest() %>%
    mutate(CMdata = map(data, ~filter(., date > startdatetime))) %>%
    mutate(fireplot = map(CMdata, ~ ggplot(data = ., aes(x = date, y = !!sym({{valuevar}}), colour = class)) +
                            geom_point() +
                            theme(axis.text.x = element_text(angle=90))),
           nobs = map_int(CMdata, ~nrow(.)),
           date_start_monitoring = as.Date.POSIXct(map_dbl(CMdata, ~ min(.[["date"]],na.rm = TRUE)),origin = "1970-01-01"),
           date_end_monitoring = as.Date.POSIXct(map_dbl(CMdata, ~ max(.[["date"]],na.rm = TRUE)),origin = "1970-01-01"),
           nfire_cm = map_dbl(CMdata, ~countfire(.)),
           ndays_cm = date_end_monitoring - date_start_monitoring +1 ,
           ndays_obs = map_dbl(CMdata, ~length(unique(as.Date(.$date)))),
           nfire_pday_cm = map2_dbl(nfire_cm, ndays_obs, ~ .x / .y)
           ) %>%
    filter(nobs >= minobs) %>%
    select(-cycle) %>%
    rename("qr_code_household" = {{id_var}},
           "sensor_location" = {{sensor_locationvar}},
           "place" = {{placevar}})

  if (returnFull)  return(dfFull)

  dfFull %>%
    select(place, year, fuel, qr_code_household, ndays_cm, ndays_obs, nfire_cm,
           date_start_monitoring, date_end_monitoring, nfire_pday_cm,
           nfire_pday_cm)

}
