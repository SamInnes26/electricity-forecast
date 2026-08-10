library(httr2)
library(jsonlite)
library(tidyverse)


##### NESO API FORECAST & HISTORIC WIND DATA DOWNLOAD #####
# DAY AHEAD WIND FORECASTS

# DESCRIPTION:
# Publication frequency: daily.
# Publication time: between 09:00 and 09:15 (clock time).
# This is the NESO's day ahead operational metered wind forecast; the wind power 
# forecast is in megawatts (MW).

wind_da <- fromJSON(
  "https://api.neso.energy/api/3/action/datastore_search?resource_id=b2f03146-f05d-4824-a663-3a4f36090c71&limit=10000"
)$result$records


# 14 DAYS AHEAD WIND FORECAST

# DESCRIPTION:
# This resource contains the national 0-14 day ahead wind forecasts for all the 
# windfarms which are used to produce NESO's national day ahead incentive wind 
# forecasts. This is not a complete list of BMU's within NESO systems and 
# excludes licence-exempt windfarms.

wind_2_14_da <- fromJSON(
  "https://api.neso.energy/api/3/action/datastore_search?resource_id=93c3048e-1dab-4057-a2a9-417540583929&limit=10000"
)$result$records

# MONTHLY OPERATIONAL METERED WIND OUTPUT

# DESCRIPTION:
# Metered wind output in MW for Scotland, England, and Wales.
# Output is provided per date per settlement period. Please note that this data 
# is from operational metering and is therefore expected to be different to the
# data published as part of the wind forecasting metrics, as that data uses 
# settlement metering which will be subject to reconciliation.

# NOTES:
# Will need to be updated annually
# Written to only download most recent 1600 (i.e. last month + buffer) rows of the data

wind_monthly_operational_metered_output <- fromJSON(
  "https://api.neso.energy/api/3/action/datastore_search?resource_id=c9bc94c4-6d8c-49ff-afff-67c0030bec05&limit=1600&sort=Sett_Date%20desc"
)$result$records


##### NESO API FORECAST & HISTORIC DEMAND DATA DOWNLOAD #####


# DAY AHEAD DEMAND FORECAST

# DESCRIPTION:
# Publication frequency: twice daily.
# Publication time: between 09:00 & 09:15 and between 12:00 & 12:15 (clock time).
# This is our day ahead national demand forecast. National Demand is the Great 
# Britain generation requirement and is the sum of metered generation based on 
# National Grid's operational generation metering, but excludes generation 
# required to meet station load, pump storage pumping and interconnector exports.

demand_da <- fromJSON(
  "https://api.neso.energy/api/3/action/datastore_search?resource_id=aec5601a-7f3e-4c4c-bf56-d8e4184d3c5b&limit=10000"
)$result$records

# # 2 DAY AHEAD DEMAND FORECAST
# 
# # DESCRIPTION:
# # Publication frequency: daily.
# # Publication time: between 16:15 and 17:00 (clock time).
# # This is our 2 days ahead national demand forecast. National Demand is the 
# # Great Britain generation requirement and is the sum of metered generation 
# # based on National Grid's operational generation metering, but excludes 
# # generation required to meet station load, pump storage pumping and 
# # interconnector exports.
# 
# demand_2_da <- fromJSON(
#   "https://api.neso.energy/api/3/action/datastore_search?resource_id=cda26f27-4bb6-4632-9fb5-2d029ca605e1&limit=10000"
# )$result$records

# 2-14 DAY AHEAD DEMAND FORECAST

# DESCRIPTION:
# This is our 2-14 days ahead national demand forecast, updated twice daily. 
# National Demand is the Great Britain generation requirement and is the sum of 
# metered generation based on the operational generation metering from the 
# Transmission Network Operators, but excludes generation required to meet 
# station load, pump storage pumping and interconnector exports.

demand_2_14_da <- fromJSON(
  "https://api.neso.energy/api/3/action/datastore_search?resource_id=7c0411cd-2714-4bb5-a408-adb065edf34d&limit=10000"
)$result$records

# DEMAND DATA

# DESCRIPTION:
# This publication is similar to "Historic Demand Data" dataset but updated on a
# daily basis with approximately ten days lag in receiving settlement data. In 
# the dataset, embedded forecast for the upcoming next 7 days is also made 
# available.

demand_historic_data <- fromJSON(
  "https://api.neso.energy/api/3/action/datastore_search?resource_id=177f6fa4-ae49-4182-81ea-0c6b35f26ca6&limit=10000"
)$result$records


##### OCTOPUS API GAS & ELECTRIC DOWNLOAD #####

# elec <- request(
#   "https://api.octopus.energy/v1/products/SILVER-26-04-01/electricity-tariffs/E-1R-SILVER-26-04-01-K/standard-unit-rates/"
#   ) |>
#   req_perform() |>
#   resp_body_json()

elec <- fromJSON(
  "https://api.octopus.energy/v1/products/SILVER-26-04-01/electricity-tariffs/E-1R-SILVER-26-04-01-K/standard-unit-rates/"
)

today <- Sys.Date()

elec_today <- subset(
  elec$results,
  as.Date(valid_from) <= today &
    as.Date(valid_to) >= today
)

elec_rate <- elec_today$value_inc_vat[1]

elec_sc <- fromJSON(
  "https://api.octopus.energy/v1/products/SILVER-26-04-01/electricity-tariffs/E-1R-SILVER-26-04-01-K/standing-charges/"
)

elec_standing_charge <- elec_sc$results$value_inc_vat[1]


gas <- fromJSON(
  "https://api.octopus.energy/v1/products/SILVER-26-04-01/gas-tariffs/G-1R-SILVER-26-04-01-K/standard-unit-rates/"
)

gas_today <- subset(
  gas$results,
  as.Date(valid_from) <= today &
    as.Date(valid_to) >= today
)

gas_rate <- gas_today$value_inc_vat[1]

gas_sc <- fromJSON(
  "https://api.octopus.energy/v1/products/SILVER-26-04-01/gas-tariffs/G-1R-SILVER-26-04-01-K/standing-charges/"
)

gas_standing_charge <- gas_sc$results$value_inc_vat[1]

##### DATA CLEANING #####

demand_2_14_da <- demand_2_14_da %>%
  mutate(
    date = ymd_hms(GDATETIME)
  ) %>% 
  select(date, NATIONALDEMAND) %>% 
  rename(demand = NATIONALDEMAND)

wind_2_14_da <- wind_2_14_da %>%
  mutate(
    date = ymd_hms(Datetime)
  ) %>% 
  select(date, Wind_Forecast) %>% 
  rename(wind = Wind_Forecast)


##### ENERGY COST INDEX CALCULATION #####

residual_2_14_da <- demand_2_14_da %>% 
  left_join(wind_2_14_da, by = "date") %>% 
  drop_na() %>% 
  mutate(
    residual = demand - wind,
    date_ymd = as.Date(date)
  ) %>% 
  group_by(date_ymd) %>% 
  summarise(
    avg_residual = mean(residual),
    .groups = "drop"
  ) %>% 
  mutate(
    rank_14_day = rank(avg_residual, ties.method = "min")
  )

first_7_days <- residual_2_14_da %>%
  arrange(date_ymd) %>% 
  slice(1:7) %>%
  mutate(
    rank_7_day = rank(avg_residual, ties.method = "min")
  ) %>%
  select(date_ymd, rank_7_day)

residual_2_14_da <- residual_2_14_da %>% 
  left_join(first_7_days, by = "date_ymd")


###### CSV WRITING ######

write.csv(
  residual_2_14_da,
  "data/latest_forecast.csv",
  row.names = FALSE
)

residual_2_14_da <- residual_2_14_da %>% 
  mutate(forecast_date = today) %>% 
  select(c("forecast_date", "date_ymd", "avg_residual", "rank_14_day", "rank_7_day"))

write.table(
  residual_2_14_da,
  "data/forecast_history.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE,
  append = TRUE
)

today_prices <- data.frame(
  date = Sys.Date(),
  elec_rate = elec_rate,
  elec_standing_charge = elec_standing_charge,
  gas_rate = gas_rate,
  gas_standing_charge = gas_standing_charge
)

write.table(
  today_prices,
  "data/tracker_price_history.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE,
  append = TRUE
)

demand_wind_2_14_da <- demand_2_14_da %>% 
  full_join(wind_2_14_da, by = "date") %>% 
  mutate(forecast_date = today) %>% 
  arrange(date) %>% 
  select(c("forecast_date", "date", "demand", "wind"))

write.table(
  demand_wind_2_14_da,
  "data/demand_wind_2_14_forecast_history.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE,
  append = TRUE
)
