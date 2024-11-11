library(dplyr)
library(lubridate)

flights_data <- readRDS("../data/cleaned_data/flights_data.rds")

summary_by_hour <- flights_data %>%
  filter(month(CRS_DEP_TIMESTAMP) %in% c(11, 12, 1)) %>%
  mutate(
    YEAR = year(CRS_DEP_TIMESTAMP),
    MONTH = month(CRS_DEP_TIMESTAMP),
    DAY = day(CRS_DEP_TIMESTAMP),
    HOUR = hour(CRS_DEP_TIMESTAMP)
  ) %>%
  group_by(ORIGIN, MONTH, DAY, HOUR) %>%
  summarise(
    avg_cancellation_rate = mean(CANCELLED, na.rm = TRUE),
    avg_delay_time = mean(DELAY_TIME, na.rm = TRUE)
  ) %>%
  ungroup()

saveRDS(summary_by_hour, "./flight_app/cancel_delay.rds")
