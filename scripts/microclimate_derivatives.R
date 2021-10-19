########################################################
# Calculates microclimate variables from the time series
########################################################

library(tidyverse) # Handles everything more nicely than base R
library(lubridate) # Handles time objects nicely
library(zoo) # For rolling functions
library(data.table)

d <- read_csv("output/tomst_data.csv")

# Change the datetime to Finnish time
# This takes some time...
d %>% mutate(datetime = ymd_hms(datetime, tz = "UTC")) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) -> d

# Add date column
d %>% mutate(date = as_date(datetime)) %>% 
  relocate(date, .after = datetime) -> d

# Calibration function for the moisture count values for unknown soils from Kopecky et al. 2020
cal_funNA <- function(x) {((-1.34e-8) * (x^2) + (2.50e-4) * x + (-1.58e-1))*100 }

# Calibrate the moisture values
d %>% mutate(moist = round(cal_funNA(moist),1)) %>% 
  mutate(moist = ifelse(T1 < 1 | probl == 1, NA, moist)) -> d

# Fill missing observations (max length of three consecutive NAs) with running average
# Linear interpolation
d %>% group_by(site) %>% 
  # mutate(T1 = ifelse(site == 1 & datetime == "2019-07-01 00:30:00", NA, T1)) %>% 
  mutate(across(T1:moist, ~na.approx(.x, datetime, maxgap = 3, na.rm = F))) -> d

########################################################################
# AGGREGATE TO DAILY VALUES

notNA_prop <- function(x){ round(sum(is.finite(x)/length(x))*100,1) }

d %>% group_by(site, date) %>%
  summarise(across(T1:moist, ~notNA_prop(.x), 
                   na.rm = F, .names = "{.col}_prop"),
            across(T1:moist, list(mean = mean, min = min, max = max), 
                   na.rm = T, .names = "{.col}_{.fn}"),
            probl = max(probl, na.rm = T)) %>% 
  ungroup() -> daily

# Change Inf and -Inf to NA
infmutate <- function(x) ifelse(is.infinite(x),NA,x)
daily %>% mutate(across(T1_prop:probl, infmutate)) -> daily

# Write daily data
write_csv(daily, "output/tomst_data_daily.csv")

########################################################################
# MONTHLY DATA

# Create a matrix of number of days in each calender month
daycount <- data.frame(date = c(as_date(as_date("2018-01-01"):as_date("2021-12-31")))) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(year, month) %>% 
  summarise(ndaysmax = n())


# Aggregation to monthly values is done separately for each sensors 
# due to different filtering based on error codes

# First T1
daily %>% 
  filter(T1_prop == 100,
         probl %in% c(0,3),
         is.finite(T1_mean)) %>% 
  mutate(month = month(date),
                 year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T1_mean = round(mean(T1_mean, na.rm = T),2),
            T1_absmax = round(max(T1_max, na.rm = T),2),
            T1_absmin = round(min(T1_min, na.rm = T),2),
            T1_meanmax = round(mean(T1_max, na.rm = T),2),
            T1_meanmin = round(mean(T1_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T1 = ndays/ndaysmax) %>% 
  relocate(day_frac_T1, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T1

# T2
daily %>% 
  filter(T2_prop == 100,
         probl %in% c(0,3,4),
         is.finite(T2_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T2_mean = round(mean(T2_mean, na.rm = T),2),
            T2_absmax = round(max(T2_max, na.rm = T),2),
            T2_absmin = round(min(T2_min, na.rm = T),2),
            T2_meanmax = round(mean(T2_max, na.rm = T),2),
            T2_meanmin = round(mean(T2_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T2 = ndays/ndaysmax) %>% 
  relocate(day_frac_T2, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T2

# T3
daily %>% 
  filter(T3_prop == 100,
         probl %in% c(0),
         is.finite(T3_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T3_mean = round(mean(T3_mean, na.rm = T),2),
            T3_absmax = round(max(T3_max, na.rm = T),2),
            T3_absmin = round(min(T3_min, na.rm = T),2),
            T3_meanmax = round(mean(T3_max, na.rm = T),2),
            T3_meanmin = round(mean(T3_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T3 = ndays/ndaysmax) %>% 
  relocate(day_frac_T3, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T3

# moist
daily %>% 
  filter(moist_prop == 100,
         probl %in% c(0,3,4),
         is.finite(moist_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            moist_sd = round(sd(moist_mean, na.rm = T),2),
            moist_cv = round(sd(moist_mean, na.rm = T)/mean(moist_mean, na.rm = T),2),
            moist_med = round(median(moist_mean, na.rm = T),1),
            moist_mean = round(mean(moist_mean, na.rm = T),1),
            moist_absmax = round(max(moist_max, na.rm = T),1),
            moist_absmin = round(min(moist_min, na.rm = T),1)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_moist = ndays/ndaysmax) %>% 
  relocate(day_frac_moist, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_moist

full_join(dm_T1, dm_T2) %>% 
  full_join(., dm_T3) %>% 
  full_join(., dm_moist) %>% 
  mutate(across(starts_with("day_f"), ~ifelse(!is.finite(.x), 0, .x))) %>% 
  relocate(starts_with("day_f"), .after = month) -> dm
rm(dm_T1, dm_T2, dm_T3, dm_moist)

# Monthly thermal sums

# First T1
daily %>% 
  filter(T1_prop == 100,
         probl %in% c(0,3),
         is.finite(T1_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T1_TDD = round(sum(ifelse(T1_mean < 0, 0, T1_mean), na.rm = T),2),
            T1_GDD3 = round(sum(ifelse(T1_mean < 3, 0, T1_mean), na.rm = T),2),
            T1_GDD5 = round(sum(ifelse(T1_mean < 5, 0, T1_mean), na.rm = T),2),
            T1_FDD = round(sum(ifelse(T1_mean > 0, 0, T1_mean), na.rm = T),2),
            T1_TDD_days = round(sum(ifelse(T1_mean < 0, 0, 1), na.rm = T),2),
            T1_GDD3_days = round(sum(ifelse(T1_mean < 3, 0, 1), na.rm = T),2),
            T1_GDD5_days = round(sum(ifelse(T1_mean < 5, 0, 1), na.rm = T),2),
            T1_FDD_days = round(sum(ifelse(T1_mean > 0, 0, 1), na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T1 = ndays/ndaysmax) %>% 
  relocate(day_frac_T1, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T1

# T2
daily %>% 
  filter(T2_prop == 100,
         probl %in% c(0,3,4),
         is.finite(T2_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T2_TDD = round(sum(ifelse(T2_mean < 0, 0, T2_mean), na.rm = T),2),
            T2_GDD3 = round(sum(ifelse(T2_mean < 3, 0, T2_mean), na.rm = T),2),
            T2_GDD5 = round(sum(ifelse(T2_mean < 5, 0, T2_mean), na.rm = T),2),
            T2_FDD = round(sum(ifelse(T2_mean > 0, 0, T2_mean), na.rm = T),2),
            T2_TDD_days = round(sum(ifelse(T2_mean < 0, 0, 1), na.rm = T),2),
            T2_GDD3_days = round(sum(ifelse(T2_mean < 3, 0, 1), na.rm = T),2),
            T2_GDD5_days = round(sum(ifelse(T2_mean < 5, 0, 1), na.rm = T),2),
            T2_FDD_days = round(sum(ifelse(T2_mean > 0, 0, 1), na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T2 = ndays/ndaysmax) %>% 
  relocate(day_frac_T2, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T2

# T3
daily %>% 
  filter(T3_prop == 100,
         probl %in% c(0),
         is.finite(T3_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T3_TDD = round(sum(ifelse(T3_mean < 0, 0, T3_mean), na.rm = T),2),
            T3_GDD3 = round(sum(ifelse(T3_mean < 3, 0, T3_mean), na.rm = T),2),
            T3_GDD5 = round(sum(ifelse(T3_mean < 5, 0, T3_mean), na.rm = T),2),
            T3_FDD = round(sum(ifelse(T3_mean > 0, 0, T3_mean), na.rm = T),2),
            T3_TDD_days = round(sum(ifelse(T3_mean < 0, 0, 1), na.rm = T),2),
            T3_GDD3_days = round(sum(ifelse(T3_mean < 3, 0, 1), na.rm = T),2),
            T3_GDD5_days = round(sum(ifelse(T3_mean < 5, 0, 1), na.rm = T),2),
            T3_FDD_days = round(sum(ifelse(T3_mean > 0, 0, 1), na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T3 = ndays/ndaysmax) %>% 
  relocate(day_frac_T3, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T3

full_join(dm_T1, dm_T2) %>% 
  full_join(., dm_T3) %>% 
  mutate(across(starts_with("day_f"), ~ifelse(!is.finite(.x), 0, .x))) %>% 
  relocate(starts_with("day_f"), .after = month) -> dm_dd
rm(dm_T1, dm_T2, dm_T3)

dm <- full_join(dm, dm_dd)

write_csv(dm, "output/tomst_data_monthly.csv")

###################################################################################
# SNOW COVER DURATION

# Here I calculate stuff to determine which dates have been under snow
daily %>% group_by(site) %>% 
  select(site:date, T2_min, T2_max) %>% 
  mutate(center_t = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
         center_tmin = rollapply(T2_min, width=10, FUN=min, fill = NA, partial = T, align = "center"),
         lead_t = rollapply(T2_max, width=10, FUN=max, fill = NA, partial = T, align = "right"),
         lead_tmin = rollapply(T2_min, width=10, FUN=min, fill = NA, partial = T, align = "right"),
         lag_t = rollapply(T2_max, width=10, FUN=max, fill = NA, partial = T, align = "left"),
         lag_tmin = rollapply(T2_min, width=10, FUN=min, fill = NA, partial = T, align = "left")) %>% 
  mutate(center_range = center_t-center_tmin,
         lead_range = lead_t-lead_tmin,
         lag_range = lag_t-lag_tmin,
         center_t = ifelse(center_t < 1 & center_range < 10, 1, 0),
         lead_t = ifelse(lead_t < 1 & lead_range < 10, 1, 0),
         lag_t = ifelse(lag_t < 1 & lag_range < 10, 1, 0)) %>% 
  mutate(lead_c = rollapply(center_t, width=5, FUN=max, fill = NA, partial = T, align = "right"),
         lag_c = rollapply(center_t, width=5, FUN=max, fill = NA, partial = T, align = "left")) %>% 
  rowwise() %>% mutate(center_c = max(c(lead_c, lag_c))) -> dd

# Plot an example

id_to_plot <- "AIL155"

dd %>% filter(site == id_to_plot) %>%
  ggplot(aes_string(x="date")) +
  geom_hline(yintercept = 0)+
  geom_ribbon(aes(ymin=T2_min, ymax=T2_max), fill = "gray10")+
  geom_line(aes(y = lead_t), col = "white", size = 0.8) +
  geom_line(aes(y = lag_t), col = "blue", size = 0.8) +
  geom_line(aes(y = center_c), col = "red", size = 0.8) +
  theme_dark() +
  ylab("Temperature") + xlab("Date")+
  ggtitle(id_to_plot)

# Determine the length of snow periods
dd %>% mutate(hydro_year = year(date + days(122))) %>% 
  group_by(site, hydro_year) %>% 
  mutate(grp = rleid(center_c)) %>% 
  group_by(site, grp, hydro_year) %>% 
  summarise(n = n(),
            snow = mean(center_c)) %>% 
  filter(snow == 1) %>% 
  group_by(site, hydro_year) %>% 
  summarise(snow_days = sum(n),
            max_snow_period = max(n),
            n_snow_period = n()) -> d1

# Determine first and last dates with snow cover
dd %>% mutate(hydro_year = year(date + days(122))) %>% 
  group_by(site, hydro_year) %>% 
  filter(center_c == 1) %>% 
  summarise(first_snow_date = min(date),
            last_snow_date = max(date)) %>% 
  mutate(last_snow_doy = yday(last_snow_date)) -> d2

# Number of freeze-thaw cycles in Tomst data
d %>% mutate(hydro_year = year(date + days(122))) %>% 
  group_by(site, hydro_year) %>% 
  mutate(across(T1:T3, ~ifelse(.x >= 0, 1, -1))) %>% 
  mutate(T1_ft = ifelse(T1*lag(T1) == -1, 1, 0),
         T2_ft = ifelse(T2*lag(T2) == -1, 1, 0),
         T3_ft = ifelse(T3*lag(T3) == -1, 1, 0)) %>% 
  summarise(T1_ft = sum(T1_ft, na.rm = T),
            T2_ft = sum(T2_ft, na.rm = T),
            T3_ft = sum(T3_ft, na.rm = T)) -> d3

###############################################################################################
# SIMILAR CALCULATIONS BUT EXCUDING VERY SHALLOW SNOW WITH POOR INSULATING CAPACITY

# Here I calculate stuff to determine which dates have been under snow
daily %>% group_by(site) %>% 
  dplyr::select(site:date, T2_min, T2_max, T3_min, T3_max) %>% 
  mutate(T2_range = T2_max-T2_min,
         T3_range = T3_max-T3_min) %>% 
  mutate(center_t2 = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
         center_t2min = rollapply(T2_min, width=10, FUN=min, fill = NA, partial = T, align = "center")) %>% 
  mutate(center_range2 = center_t2-center_t2min) %>% 
  mutate(center_t3 = rollapply(T3_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
         center_t3min = rollapply(T3_min, width=10, FUN=min, fill = NA, partial = T, align = "center")) %>% 
  mutate(center_range3 = center_t3-center_t3min) %>% 
  mutate(T2_range = rollapply(T2_range, width=9, FUN=mean, fill = NA, partial = T, align = "center"),
         T3_range = rollapply(T3_range, width=9, FUN=mean, fill = NA, partial = T, align = "center"),
         T2_max = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
         T3_max = rollapply(T3_max, width=9, FUN=max, fill = NA, partial = T, align = "center")) %>% 
  mutate(T2_snow = ifelse(T2_max < 1 & T2_range < 1 & center_range2 < 2, 1, 0),
         T3_snow = ifelse(T3_max < 1 & T3_range < 1 & center_range3 < 2, 1, 0)) %>% 
  mutate(lead_T2 = rollapply(T2_snow, width=5, FUN=max, fill = NA, partial = T, align = "right"),
         lag_T2 = rollapply(T2_snow, width=5, FUN=max, fill = NA, partial = T, align = "left"),
         lead_T3 = rollapply(T3_snow, width=5, FUN=max, fill = NA, partial = T, align = "right"),
         lag_T3 = rollapply(T3_snow, width=5, FUN=max, fill = NA, partial = T, align = "left")) %>% 
  rowwise() %>% mutate(T2_snow = max(c(lead_T2, lag_T2)),
                       T3_snow = max(c(lead_T3, lag_T3))) -> dd


# Plot an example site
id_to_plot <- "AIL155"

dd %>% filter(site == id_to_plot) %>%
  ggplot(aes_string(x="date")) +
  geom_hline(yintercept = 0)+
  geom_ribbon(aes(ymin=T2_min, ymax=T2_max), fill = "gray10")+
  geom_line(aes(y = T2_snow), col = "white", size = 0.8) +
  geom_line(aes(y = T3_snow), col = "blue", size = 0.8) +
  theme_dark() +
  ylab("Temperature") + xlab("Date")+
  ggtitle(id_to_plot)



# Determine the length of deep snow periods T2
dd %>% mutate(hydro_year = year(date + days(122))) %>% 
  group_by(site, hydro_year) %>% 
  mutate(grp = rleid(T2_snow)) %>% 
  group_by(site, grp, hydro_year) %>% 
  summarise(n = n(),
            snow = mean(T2_snow)) %>% 
  filter(snow == 1) %>% 
  group_by(site, hydro_year) %>% 
  summarise(deep_snow_days_T2 = sum(n),
            max_deep_snow_period_T2 = max(n),
            n_deep_snow_period_T2 = n()) -> d5

# Determine first and last dates with deep snow cover T2
dd %>% mutate(hydro_year = year(date + days(122))) %>% 
  group_by(site, hydro_year) %>% 
  filter(T2_snow == 1) %>% 
  summarise(first_deep_snow_date_T2 = min(date),
            last_deep_snow_date_T2 = max(date)) %>% 
  mutate(last_deep_snow_doy_T2 = yday(last_deep_snow_date_T2)) -> d6

# Determine the length of deep snow periods T3
dd %>% mutate(hydro_year = year(date + days(122))) %>% 
  group_by(site, hydro_year) %>% 
  mutate(grp = rleid(T3_snow)) %>% 
  group_by(site, grp, hydro_year) %>% 
  summarise(n = n(),
            snow = mean(T3_snow)) %>% 
  filter(snow == 1) %>% 
  group_by(site, hydro_year) %>% 
  summarise(deep_snow_days_T3 = sum(n),
            max_deep_snow_period_T3 = max(n),
            n_deep_snow_period_T3 = n()) -> d7

# Determine first and last dates with deep snow cover T3
dd %>% mutate(hydro_year = year(date + days(122))) %>% 
  group_by(site, hydro_year) %>% 
  filter(T3_snow == 1) %>% 
  summarise(first_deep_snow_date_T3 = min(date),
            last_deep_snow_date_T3 = max(date)) %>% 
  mutate(last_deep_snow_doy_T3 = yday(last_deep_snow_date_T3)) -> d8



# Combine the results
d1 <- full_join(d1,d2)
d1 <- full_join(d1,d3)
d1 <- full_join(d1,d5)
d1 <- full_join(d1,d6)
d1 <- full_join(d1,d7)
d1 <- full_join(d1,d8)

d1 %>% mutate(snow_days = ifelse(is.na(snow_days) & T2_ft > 0, 0, snow_days),
              max_snow_period = ifelse(is.na(max_snow_period) & T2_ft > 0, 0, max_snow_period),
              n_snow_period = ifelse(is.na(n_snow_period) & T2_ft > 0, 0, n_snow_period),
              last_snow_doy = ifelse(is.na(last_snow_doy) & T2_ft > 0, 0, last_snow_doy)) -> d1

d1 %>% mutate(deep_snow_days_T2 = ifelse(is.na(deep_snow_days_T2) & T2_ft > 0, 0, deep_snow_days_T2),
              max_deep_snow_period_T2 = ifelse(is.na(max_deep_snow_period_T2) & T2_ft > 0, 0, max_deep_snow_period_T2),
              n_deep_snow_period_T2 = ifelse(is.na(n_deep_snow_period_T2) & T2_ft > 0, 0, n_deep_snow_period_T2),
              last_deep_snow_doy_T2 = ifelse(is.na(last_deep_snow_doy_T2) & T2_ft > 0, 0, last_deep_snow_doy_T2)) -> d1

d1 %>% mutate(deep_snow_days_T3 = ifelse(is.na(deep_snow_days_T3) & T2_ft > 0, 0, deep_snow_days_T3),
              max_deep_snow_period_T3 = ifelse(is.na(max_deep_snow_period_T3) & T2_ft > 0, 0, max_deep_snow_period_T3),
              n_deep_snow_period_T3 = ifelse(is.na(n_deep_snow_period_T3) & T2_ft > 0, 0, n_deep_snow_period_T3),
              last_deep_snow_doy_T3 = ifelse(is.na(last_deep_snow_doy_T3) & T2_ft > 0, 0, last_deep_snow_doy_T3)) -> d1

# d1 %>% filter(hydro_year > 2019) -> d1

d1 %>% as.data.table() %>% sample_n(10)

# Write results to csv
write_csv(d1, "output/snow_variables.csv")


