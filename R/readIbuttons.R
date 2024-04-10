library(CTOWWS)
library(DBI)
library(tidyverse)
library(HGICPcalculator)

con <- dbConnect_wws(db_name = "ibuttons2")

tbls <- DBI::dbListTables(con)

tbl_households <- tbl(con, "tbl_households") %>%
  filter(fieldworker_id == "stephennyathi@nova.org.za") %>%
  collect()

dfQrCode <- tbl_households %>% select(qr_code_household)

tbl_locations <- tbl(con, "tbl_locations") %>%
  right_join(dfQrCode, copy = TRUE) %>%
  collect()

dfButtonIDs <- tbl_locations %>% select(ibutton_id)

dfReadings <- tbl(con, "tbl_readings") %>%
  select(date, temp, ibutton_id, file_id) %>%
right_join(tbl_locations %>% select(ibutton_location, ibutton_id, qr_code_household), copy = TRUE) %>%
  collect() %>%
  filter(date > lubridate::ymd_hms("2023-05-21 00:00:00", tz = "Africa/Johannesburg"))

dfReadingsFuture <- dfReadings %>% filter(date > lubridate::ymd_hms("2023-08-09 00:00:00", tz = "Africa/Johannesburg"))

dfReadings <- dfReadings %>%
  filter(date < lubridate::ymd_hms("2023-08-09 00:00:00", tz = "Africa/Johannesburg"))

dfNestFile <- dfReadings %>%
  group_by(file_id, ibutton_location, qr_code_household) %>%
  arrange(file_id, date) %>%
  nest() %>%
  mutate(data = map(data, ~{mutate(., lag1 = lag(temp),
                                   lag2 = lag(temp, 2),
                                   lag3 = lag(temp, 3),
                                   diff = temp - lag1,
                                   diff1 = temp - lag2,
                                   diff1 = temp - lag3
  )}))

dfNest <- dfReadings %>% group_by(ibutton_location, qr_code_household) %>% nest()

ggplot(data = dfNest %>% unnest(cols = c(data)), mapping = aes(x = date, y = temp)) +
  geom_point(size = I(1/9)) +
  facet_wrap(qr_code_household ~ .)
