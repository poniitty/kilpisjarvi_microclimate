library(tidyverse)
library(lubridate)

add_zeros <- function(x){
  if(nchar(x) == 1){
    return(as.character(paste0("00",x)))
  }
  if(nchar(x) == 2){
    return(as.character(paste0("0",x)))
  }
  if(nchar(x) > 2){
    return(as.character(x))
  }
}

d1 <- read_csv("data/haxo_data_corrected_AIL.csv") %>% 
  mutate(site = unlist(lapply(site, function(x) paste0("AIL", add_zeros(x))))) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))
d2 <- read_csv("data/haxo_data_corrected_MAL.csv") %>% 
  mutate(site = unlist(lapply(site, function(x) paste0("MAL", add_zeros(x))))) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))

d <- bind_rows(d1,d2) %>% 
  rename(probl = haxo_probl)

d3 <- read_csv("output/hobo_data_corrected.csv") %>% 
  rename(datetime = roundtime,
         probl = hobo_probl) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))

d <- bind_rows(d %>% mutate(logger = "a"),
               d3 %>% mutate(logger = "o"))

write_csv(d, "output/T4_combined.csv")
