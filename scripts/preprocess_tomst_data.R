
library(tidyverse)
library(lubridate)
library(zoo)

d1 <- read_csv("/projappl/project_2003061/repos/microclim_hyytiala/output/tomst_data.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  mutate(area = "HYY")
d2 <- read_csv("/projappl/project_2003061/repos/microclim_tiilikka/output/tomst_data.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  mutate(area = "TII")
d3 <- read_csv("/projappl/project_2003061/repos/microclim_varrio/output/tomst_data.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  mutate(area = "VAR")

d <- bind_rows(d1,d2,d3); rm(d1,d2,d3)

# Add date column
d %>% mutate(date = as_date(datetime)) %>% 
  relocate(date, .after = datetime) -> d

# Change bad data to NA
d %>% mutate(T1 = ifelse(probl %in% c(1,2,4,9), NA, T1),
             T2 = ifelse(probl %in% c(1,2,7,8), NA, T2),
             T3 = ifelse(probl %in% c(1,2,3,4,5,7,8), NA, T3),
             moist = ifelse(probl %in% c(1,2,6,8,9), NA, moist),
             moist = ifelse(T1 < 1, NA, moist)) -> d

# Calibration function for the moisture count values for unknown soils from Kopecky et al. 2020
cal_funNA <- function(x) {((-1.34e-8) * (x^2) + (2.50e-4) * x + (-1.58e-1))*100 }

# Calibrate the moisture values
d %>% mutate(moist = round(cal_funNA(moist),1)) -> d

# Fill missing observations (max length of three consecutive NAs) with running average
# Linear interpolation
d %>% group_by(area, site) %>% 
  mutate(across(T1:moist, ~na.approx(.x, datetime, maxgap = 3, na.rm = F))) -> d

d %>% 
  filter(probl != 1) %>%
  filter(!all(is.na(moist), is.na(T1), is.na(T2), is.na(T3))) -> d

d %>% 
  relocate(probl, .after = date) %>% 
  relocate(area) -> d

# Write data
write_csv(d %>% select(-date), "output/tomst_data.csv")

########################################################################
# AGGREGATE TO DAILY VALUES

q025 <- function(x, na.rm = T){as.numeric(quantile(x, 0.025, na.rm))}
q975 <- function(x, na.rm = T){as.numeric(quantile(x, 0.975, na.rm))}

d %>% group_by(area, site, date) %>%
  summarise(across(moist:T3, ~sum(is.na(.x))/96, 
                   na.rm = F, .names = "{.col}_prop"),
            across(moist:T3, list(mean = mean, min = min, max = max, sd = sd,
                                  q025 = q025, q975 = q975), 
                   na.rm = T, .names = "{.col}_{.fn}"),
            across(moist, list(median = median), 
                   na.rm = T, .names = "{.col}_{.fn}"),
            probl = max(probl, na.rm = T)) %>% 
  ungroup() -> daily

daily <- daily %>% 
  filter(moist_prop < 0.05 | T1_prop < 0.05 | T2_prop < 0.05 | T3_prop < 0.05)

# Change Inf and -Inf to NA
infmutate <- function(x) ifelse(is.infinite(x),NA,x)
daily %>% mutate(across(moist_prop:probl, infmutate)) -> daily

# Write daily data
write_csv(daily, "output/tomst_data_daily.csv")



