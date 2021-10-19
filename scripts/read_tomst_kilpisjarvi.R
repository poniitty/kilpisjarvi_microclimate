
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

# Abrreviastion of the study area
abbr <- "KIL"

# Set date limits to remove implausible dates
mind <- "2018-06-01"
maxd <- "2021-09-01"


#################################################################################3
# Read year 2020 site visiting times
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
maxdt20 <- bind_rows(read_csv("data/reading_times_2020_AIL.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("AIL", add_zeros(x))))),
                   read_csv("data/reading_times_2020_MAL.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("MAL", add_zeros(x))))),
                   read_csv("data/reading_times_2020_SAA.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("SAA", add_zeros(x))))))
maxdt20 %>%  mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2")) -> maxdt20

# List logger data files to read
f <- list.files("data/tomst",pattern = "data_", full.names = T, recursive = T)

fi <- data.frame(file = f)

fi$file2 <- gsub("_..csv", "", fi$file)

fi$site <- toupper(unlist(lapply(fi$file, function(x) strsplit(x, "/")[[1]][3])))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",strsplit(x, "/")[[1]][4]), "_")[[1]][1])))

readdata <- function(i){
  nn <- sum(grepl(i, fi$file2))
  
  if(nn > 1){
    
    fi2 <- fi %>% filter(grepl(i, fi$file2))
    
    df2 <- data.frame()
    for(ii in fi2$file2){
      print(ii)
      d <- fread(ii)
      
      d %>% select(V2,V3,V4,V5,V6,V7) -> d
      
      d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
      
      df2 <- bind_rows(df2, d)
    }
    
    df2 %>% filter(!duplicated(.$V2, fromLast = T)) -> df2
    
    df2$site <- fi[which(fi$file2 == ii),"id"]
    
    df2 %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> df2
    
    df2 %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = "Etc/GMT-2")) -> df2
    
    
    return(df2)
    
  } else {
    
    print(i)
    d <- fread(fi$file[fi$file2 == i])
    
    d %>% select(V2,V3,V4,V5,V6,V7) -> d
    
    d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
    
    d %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> d
    
    d$site <- fi[which(fi$file2 == i),"site"]
    
    d %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = "Etc/GMT-2")) -> d
    
    return(d)
    
  }
  
}


mylist <- lapply(fi$file2, readdata)
df <- rbindlist( mylist )

# Rename columns
df %>% rename(datetime = V2,
              zone = V3,
              T1 = V4,
              T2 = V5,
              T3 = V6,
              moist = V7) -> df

df %>% arrange(site, datetime) -> df

# Remove implausible dates
df %>% filter(datetime > mind,
              datetime < maxd) -> df

sites <- unique(df$site)

df %>% group_by(site) %>% 
  summarise(maxdt = max(datetime)) -> maxdt
maxdt <- full_join(maxdt, fi %>% select(site, tomst_id) %>% filter(!duplicated(.)))
fwrite(maxdt, "data/reading_times_2021.csv")

# Replace the measurements of the time of the 2020 visiting with NA
# as reading the logger may influence the measurements
full_join(df, maxdt20 %>% select(-tomst_id) %>% mutate(visit = 1) %>% rename(datetime = maxdt)) %>% 
  mutate(across(T1:moist, ~ifelse(is.na(visit), ., NA))) %>%
  select(-visit) -> df


# Remove the measurements of the time of the 2021 visiting
# as reading the logger may influence the measurements
full_join(df, maxdt %>% select(-tomst_id) %>% mutate(visit = 1) %>% rename(datetime = maxdt)) %>% 
  filter(is.na(visit)) %>%
  select(-visit) -> df


############################################################################
# PLOTTINGS
############################################################################

# Plot temperatures
pdf("visuals/Temperature_graphs.pdf", 12, 5)
for(i in sites){
  #i <- sites[3]
  print(i)
  df %>% filter(site == i) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
    geom_line(aes_string(y = "T2"), col = "brown1") +
    geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-20, 35))+
    ggtitle(i) -> GG
  print(GG)
  
}
dev.off()

# Calculate different daily values for diagnostics
df %>% mutate(date = as_date(datetime)) %>% 
  group_by(site,date) %>% 
  mutate(roll_diff_T1 = T1 - lead(T1),
         roll_diff_T2 = T2 - lead(T2),
         roll_diff_T3 = T3 - lead(T3)) %>%
  summarise(soil_sd = sd(T1),
            air_sd = sd(T3),
            soil_mean = mean(T1),
            air_mean = mean(T3),
            soil_min = min(T1),
            soil_max = max(T1),
            surf_max = max(T2),
            air_max = max(T3),
            mean_diff = mean(abs(T3-T1)),
            moist = min(moist),
            corr = cor(T1,T3, use = "pairwise.complete.obs"),
            max_diff_T1 = max(roll_diff_T1, na.rm = T),
            max_diff_T2 = max(roll_diff_T2, na.rm = T),
            max_diff_T3 = max(roll_diff_T3, na.rm = T)) %>% 
  mutate(sa_diff = air_mean-soil_mean,
         sa_max_diff = air_max-soil_max,
         ss_max_diff = surf_max-soil_max) %>% 
  mutate(sd_ratio = soil_sd/air_sd) %>% 
  mutate(sd_ratio = ifelse(soil_sd < 1, 0, sd_ratio)) %>% 
  mutate(corr = ifelse(is.na(corr), 0, corr)) %>% 
  as.data.frame() -> df2

pdf("visuals/Temperature_diagnose_graphs.pdf", 18, 11)
for(i in sites){
  print(i)
  df %>% filter(site == i) %>%
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
    geom_line(aes_string(y = "T2"), col = "brown1") +
    geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-20, 35))+
    ggtitle(i) -> GG1
  
  df2 %>% filter(site == i) %>%
    ggplot(aes_string(x="date")) +
    geom_line(aes_string(y = "air_sd"), col = "cornflowerblue") +
    geom_line(aes_string(y = "soil_sd"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature SD") + xlab("Date") -> GG2
  
  df2 %>% filter(site == i) %>%
    ggplot(aes_string(x="date")) +
    geom_line(aes_string(y = "sd_ratio"), col = "black") +
    theme_minimal() +
    ylab("SD ratio") + xlab("Date") -> GG3
  
  df2 %>% filter(site == i) %>%
    ggplot(aes_string(x="date")) +
    geom_line(aes_string(y = "air_mean"), col = "cornflowerblue") +
    geom_line(aes_string(y = "soil_mean"), col = "darkgoldenrod") +
    geom_line(aes_string(y = "air_max"), col = "cornflowerblue") +
    geom_line(aes_string(y = "surf_max"), col = "brown1") +
    geom_line(aes_string(y = "soil_max"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature mean + max") + xlab("Date")+
    scale_y_continuous(limits = c(-25, 40))-> GG4
  
  df2 %>% filter(site == i) %>%
    ggplot(aes_string(x="date")) +
    geom_line(aes_string(y = "corr")) +
    theme_minimal() +
    ylab("Air - soil correlation") + xlab("Date")+
    scale_y_continuous(limits = c(-1, 1)) -> GG5
  
  df2 %>% filter(site == i) %>%
    ggplot(aes_string(x="date")) +
    geom_line(aes_string(y = "max_diff_T3"), col = "cornflowerblue") +
    geom_line(aes_string(y = "max_diff_T2"), col = "brown1") +
    geom_line(aes_string(y = "max_diff_T1"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("rolling diffs") + xlab("Date") -> GG6
  
  df2 %>% filter(site == i) %>%
    ggplot(aes_string(x="date")) +
    geom_line(aes_string(y = "sa_diff"), col = "cornflowerblue") +
    geom_line(aes_string(y = "sa_max_diff"), col = "brown1") +
    geom_line(aes_string(y = "ss_max_diff"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Differences between") + xlab("Date") -> GG7
  
  df2 %>% filter(site == i) %>%
    ggplot(aes_string(x="date")) +
    geom_line(aes_string(y = "moist"), col = "black") +
    theme_minimal() +
    ylab("moisture") + xlab("Date") -> GG8
  
  print(plot_grid(plotlist = list(GG1,GG2,GG3,GG4,GG5,GG6,GG7,GG8), nrow = 4))
}
dev.off()

###################################################################################################################

times <- seq(floor_date(as_date(min(df2$date)), "month"),
             ceiling_date(as_date(max(df2$date)), "month") + months(1) - days(1),
             by = "month")

df2 %>% mutate(probl = 0) -> df2

for(siteid in sites){
  #siteid <- "AIL105"
  print(siteid)
  pdf(paste0("visuals/monthly_", siteid, ".pdf"), 10, 6)
  for (tt in 1:(length(times) - 1)) {
    
    df %>% filter(site == siteid) %>%
      filter(datetime >= ymd(times[tt]),
             datetime < ymd(times[tt + 1])) -> dft
    
    if(nrow(dft %>% filter(complete.cases(.)) > 0)){
      dft %>%
        ggplot(aes_string(x = "datetime")) +
        geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T2"), col = "brown1") +
        geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
        theme_minimal() +
        ylab("Temperature") + xlab("Date") +
        ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
        scale_x_datetime(date_minor_breaks = "1 day") -> GG1
      
      dft %>%
        ggplot(aes_string(x = "datetime")) +
        geom_line(aes_string(y = "moist"), col = "blue") +
        theme_minimal() +
        ylab("Moisture") + xlab("Date") +
        ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
        scale_x_datetime(date_minor_breaks = "1 day") -> GG2
      
      print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
    }
  }
  dev.off()
}  

####################################
# AILAKKA

# SITE = AIL101
siteid <- "AIL101"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL102
siteid <- "AIL102"
office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL103
siteid <- "AIL103"
office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL104
siteid <- "AIL104"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL105
siteid <- "AIL105"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL106
siteid <- "AIL106"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-21")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL107
siteid <- "AIL107"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-21")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL108
siteid <- "AIL108"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL109
siteid <- "AIL109"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL110
siteid <- "AIL110"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-21")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL111
siteid <- "AIL111"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL112
siteid <- "AIL112"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL113
siteid <- "AIL113"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL114
siteid <- "AIL114"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL115
siteid <- "AIL115"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL116
siteid <- "AIL116"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-21")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL117
siteid <- "AIL117"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-21")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL118
siteid <- "AIL118"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-21")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL119
siteid <- "AIL119"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL120
siteid <- "AIL120"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL121
siteid <- "AIL121"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL122
siteid <- "AIL122"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL123
siteid <- "AIL123"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL124
siteid <- "AIL124"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL125
siteid <- "AIL125"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-15")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL126
siteid <- "AIL126"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-21")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL127
siteid <- "AIL127"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c(as_date(as_date("2020-07-05"):as_date("2020-08-24")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL128
siteid <- "AIL128"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c(as_date(as_date("2020-04-07"):as_date("2020-08-24")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL129
siteid <- "AIL129"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL130
siteid <- "AIL130"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL131
siteid <- "AIL131"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL132
siteid <- "AIL132"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL133
siteid <- "AIL133"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL134
siteid <- "AIL134"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c(as_date(as_date("2021-05-17"):as_date("2021-08-26")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL135
siteid <- "AIL135"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c(as_date(as_date("2020-10-15"):as_date("2021-08-26")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL136
siteid <- "AIL136"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 137
siteid <- 137

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL138
siteid <- "AIL138"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL139
siteid <- "AIL139"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL140
siteid <- "AIL140"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL141
siteid <- "AIL141"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL142
siteid <- "AIL142"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL143
siteid <- "AIL143"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL144
siteid <- "AIL144"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL145
siteid <- "AIL145"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL146
siteid <- "AIL146"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL147
siteid <- "AIL147"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c(as_date(as_date("2019-09-27"):as_date("2020-08-24")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL148
siteid <- "AIL148"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c(as_date(as_date("2019-09-27"):as_date("2020-08-24")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL149
siteid <- "c149"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c(as_date(as_date("2020-09-27"):as_date("2021-08-17")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL150
siteid <- "AIL150"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL151
siteid <- "AIL151"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL152
siteid <- "AIL152"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c(as_date(as_date("2020-10-14"):as_date("2021-08-26")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL153
siteid <- "AIL153"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL154
siteid <- "AIL154"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL155
siteid <- "AIL155"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL156
siteid <- "AIL156"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL157
siteid <- "AIL157"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c(as_date(as_date("2021-06-10"):as_date("2021-08-26")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL158
siteid <- "AIL158"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL159
siteid <- "AIL159"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL160
siteid <- "AIL160"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c(as_date(as_date("2020-06-07"):as_date("2020-08-25")),
            as_date(as_date("2021-08-10"):as_date("2021-08-17")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL161
siteid <- "AIL161"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL162
siteid <- "AIL162"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL163
siteid <- "AIL163"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c(as_date(as_date("2020-07-05"):as_date("2020-08-12")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL164
siteid <- "AIL164"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c(as_date(as_date("2021-06-03"):as_date("2021-08-26")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL165
siteid <- "AIL165"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c(as_date(as_date("2021-06-06"):as_date("2021-08-26")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL166
siteid <- "AIL166"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = AIL167
siteid <- "AIL167"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL168
siteid <- "AIL168"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL169
siteid <- "AIL169"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL170
siteid <- "AIL170"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL171
siteid <- "AIL171"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL172
siteid <- "AIL172"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-17")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL173
siteid <- "AIL173"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c(as_date(as_date("2020-10-07"):as_date("2021-08-26")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL174
siteid <- "AIL174"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL175
siteid <- "AIL175"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL176
siteid <- "AIL176"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL177
siteid <- "AIL177"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL178
siteid <- "AIL178"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL179
siteid <- "AIL179"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL180
siteid <- "AIL180"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL181
siteid <- "AIL181"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL182
siteid <- "AIL182"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL183
siteid <- "AIL183"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL184
siteid <- "AIL184"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL185
siteid <- "AIL185"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL186
siteid <- "AIL186"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL187
siteid <- "AIL187"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL188
siteid <- "AIL188"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL189
siteid <- "AIL189"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL190
siteid <- "AIL190"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL191
siteid <- "AIL191"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL192
siteid <- "AIL192"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL193
siteid <- "AIL193"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL194
siteid <- "AIL194"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL195
siteid <- "AIL195"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL196
siteid <- "AIL196"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-16")))
probls <- c()
hattu <- c(as_date(as_date("2020-06-06"):as_date("2020-08-29")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL197
siteid <- "AIL197"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL198
siteid <- "AIL198"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL199
siteid <- "AIL199"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = AIL200
siteid <- "AIL200"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

########################################################################
# MALLA

# SITE = MAL001
siteid <- "MAL001"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL002
siteid <- "MAL002"
office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL003
siteid <- "MAL003"
office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL004
siteid <- "MAL004"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL005
siteid <- "MAL005"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL006
siteid <- "MAL006"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c(as_date(as_date("2021-06-21"):as_date("2021-08-23")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL007
siteid <- "MAL007"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL008
siteid <- "MAL008"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL009
siteid <- "MAL009"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL010
siteid <- "MAL010"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL011
siteid <- "MAL011"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL012
siteid <- "MAL012"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL013
siteid <- "MAL013"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL014
siteid <- "MAL014"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL015
siteid <- "MAL015"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL016
siteid <- "MAL016"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL017
siteid <- "MAL017"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL018
siteid <- "MAL018"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL019
siteid <- "MAL019"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL020
siteid <- "MAL020"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c(as_date(as_date("2020-07-03"):as_date("2020-08-06")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL021
siteid <- "MAL021"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL022
siteid <- "MAL022"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL023
siteid <- "MAL023"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL024
siteid <- "MAL024"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL025
siteid <- "MAL025"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL026
siteid <- "MAL026"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL027
siteid <- "MAL027"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL028
siteid <- "MAL028"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL029
siteid <- "MAL029"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL030
siteid <- "MAL030"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL031
siteid <- "MAL031"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL032
siteid <- "MAL032"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-28")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL033
siteid <- "MAL033"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL034
siteid <- "MAL034"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL035
siteid <- "MAL035"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL036
siteid <- "MAL036"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c(as_date(as_date("2020-07-12"):as_date("2020-08-07")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL037
siteid <- "MAL037"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL038
siteid <- "MAL038"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL039
siteid <- "MAL039"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL040
siteid <- "MAL040"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL041
siteid <- "MAL041"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL042
siteid <- "MAL042"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL043
siteid <- "MAL043"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL044
siteid <- "MAL044"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL045
siteid <- "MAL045"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL046
siteid <- "MAL046"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL047
siteid <- "MAL047"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL048
siteid <- "MAL048"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL049
siteid <- "MAL049"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL050
siteid <- "MAL050"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL051
siteid <- "MAL051"
office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL052
siteid <- "MAL052"
office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL053
siteid <- "MAL053"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL054
siteid <- "MAL054"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL055
siteid <- "MAL055"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c(as_date(as_date("2021-08-25"):as_date("2021-08-25")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL056
siteid <- "MAL056"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c(as_date("2020-08-10"),as_date("2020-01-02"))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL057
siteid <- "MAL057"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL058
siteid <- "MAL058"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL059
siteid <- "MAL059"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL060
siteid <- "MAL060"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL061
siteid <- "MAL061"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL062
siteid <- "MAL062"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL063
siteid <- "MAL063"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL064
siteid <- "MAL064"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = MAL065
siteid <- "MAL065"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL066
siteid <- "MAL066"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL067
siteid <- "MAL067"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL068
siteid <- "MAL068"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL070
siteid <- "MAL070"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c(as_date(as_date("2020-08-26"):as_date("2021-07-15")))
hattu <- c(as_date(as_date("2021-07-16"):as_date("2021-08-20")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL071
siteid <- "MAL071"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL072
siteid <- "MAL072"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c(as_date(as_date("2020-07-27"):as_date("2020-08-08")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL073
siteid <- "MAL073"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-08")),
            as_date(as_date("2021-08-29"):as_date("2021-08-31")))
probls <- c(as_date(as_date("2019-08-22"):as_date("2020-08-10")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL074
siteid <- "MAL074"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL075
siteid <- "MAL075"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL076
siteid <- "MAL076"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL077
siteid <- "MAL077"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL078
siteid <- "MAL078"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c(as_date(as_date("2020-07-04"):as_date("2019-08-12")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL079
siteid <- "MAL079"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL080
siteid <- "MAL080"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL081
siteid <- "MAL081"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL082
siteid <- "MAL082"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL083
siteid <- "MAL083"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL084
siteid <- "MAL084"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL085
siteid <- "MAL085"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL086
siteid <- "MAL086"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL087
siteid <- "MAL087"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL088
siteid <- "MAL088"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL089
siteid <- "MAL089"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL090
siteid <- "MAL090"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL091
siteid <- "MAL091"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL092
siteid <- "MAL092"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL094
siteid <- "MAL094"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL095
siteid <- "MAL095"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL097
siteid <- "MAL097"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL098
siteid <- "MAL098"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL099
siteid <- "MAL099"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-13")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = MAL100
siteid <- "MAL100"

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-08-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

#######################################################################
# RARE ARCTIC SITES

# SITE = RA003
siteid <- "RA003"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-19")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA007
siteid <- "RA007"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-19")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA009
siteid <- "RA009"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA010
siteid <- "RA010"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA011
siteid <- "RA011"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-20")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA013
siteid <- "RA013"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")))
probls <- c(as_date(as_date("2021-08-05"):as_date("2021-08-10")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA014
siteid <- "RA014"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA015
siteid <- "RA015"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-25")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA016
siteid <- "RA016"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-27")))
probls <- c(as_date(as_date("2021-05-31"):as_date("2021-08-24")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA017
siteid <- "RA017"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA021
siteid <- "RA021"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA022
siteid <- "RA022"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA025
siteid <- "RA025"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-06")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA027
siteid <- "RA027"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-29")))
probls <- c(as_date(as_date("2020-09-17"):as_date("2021-07-27")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA028
siteid <- "RA028"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA029
siteid <- "RA029"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-05")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA030
siteid <- "RA030"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-05")))
probls <- c(as_date(as_date("2021-08-22"):as_date("2021-08-28")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA031
siteid <- "RA031"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA032
siteid <- "RA032"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA033
siteid <- "RA033"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA034
siteid <- "RA034"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA035
siteid <- "RA035"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA036
siteid <- "RA036"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA037
siteid <- "RA037"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA038
siteid <- "RA038"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA039
siteid <- "RA039"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA040
siteid <- "RA040"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA041
siteid <- "RA041"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA042
siteid <- "RA042"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA043
siteid <- "RA043"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-22")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA044
siteid <- "RA044"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA045
siteid <- "RA045"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA046
siteid <- "RA046"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA047
siteid <- "RA047"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA048
siteid <- "RA048"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA049
siteid <- "RA049"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA050
siteid <- "RA050"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA051
siteid <- "RA051"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA052
siteid <- "RA052"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA053
siteid <- "RA053"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA054
siteid <- "RA054"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-23")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA055
siteid <- "RA055"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA057
siteid <- "RA057"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA058
siteid <- "RA058"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA059
siteid <- "RA059"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA060
siteid <- "RA060"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA061
siteid <- "RA061"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")),
            as_date(as_date("2021-07-24"):as_date("2021-08-02")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA062
siteid <- "RA062"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-24")),
            as_date(as_date("2021-07-24"):as_date("2021-08-02")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA063
siteid <- "RA063"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-25")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA064
siteid <- "RA064"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-25")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA065
siteid <- "RA065"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-25")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA066
siteid <- "RA066"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-25")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA067
siteid <- "RA067"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-25")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA068
siteid <- "RA068"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-25")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA069
siteid <- "RA069"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA070
siteid <- "RA070"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA071
siteid <- "RA071"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA072
siteid <- "RA072"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-26")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA073
siteid <- "RA073"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA074
siteid <- "RA074"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA075
siteid <- "RA075"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA076
siteid <- "RA076"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-27")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA077
siteid <- "RA077"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-28")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA078
siteid <- "RA078"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-28")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA079
siteid <- "RA079"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-28")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA080
siteid <- "RA080"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-28")),
            as_date(as_date("2021-07-30"):as_date("2021-08-02")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA081
siteid <- "RA081"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA082
siteid <- "RA082"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-29")),
            as_date(as_date("2021-07-26"):as_date("2021-08-02")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA083
siteid <- "RA083"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA084
siteid <- "RA084"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA085
siteid <- "RA085"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA086
siteid <- "RA086"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-07-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA087
siteid <- "RA087"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA088
siteid <- "RA088"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA089
siteid <- "RA089"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA090
siteid <- "RA090"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA091
siteid <- "RA091"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA092
siteid <- "RA092"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA093
siteid <- "RA093"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA094
siteid <- "RA094"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA095
siteid <- "RA095"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA096
siteid <- "RA096"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-04")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = RA101
siteid <- "RA101"

office <- c(as_date(as_date(min(df$datetime)):as_date("2020-08-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
########################################################################
# MASK IMPOSSIBLE VALUES
df %>% mutate(T1 = ifelse(T1 < (-50) | T1 > 50, NA, T1),
              T2 = ifelse(T2 < (-50) | T2 > 50, NA, T2),
              T3 = ifelse(T3 < (-50) | T3 > 50, NA, T3),
              moist = ifelse(moist < 200, NA, moist)) -> df

# FILL MISSING TIMESTAMPS WITH NA
df3 <- data.frame()
for(i in unique(df$site)){
  #i <- 6
  
  df %>% filter(site == i) -> temp
  
  temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
  temp[1,"timediff"] <- 15
  holes <- table(temp$timediff)
  
  if(max(temp$timediff, na.rm = T) > 15){
    
    print(i)
    
    missingt <- c()
    for(ii in which(temp %>% pull(timediff) > 15)){
      
      temp %>% slice((ii-1):(ii+1)) %>% pull(timediff) -> diffs
      
      if(diffs[1] %% 15 == 0L){
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "15 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      } else {
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "10 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      }
    }
    
    missingdf <- data.frame(datetime = ymd_hms(missingt),
                            site = i)
    
    print(NROW(missingdf))
    
    temp %>% full_join(., missingdf) %>% 
      arrange(datetime) %>% 
      select(-timediff) -> temp
    
    df3 <- bind_rows(df3, temp)
    
  } else {
    
    temp %>% select(-timediff) -> temp
    
    df3 <- bind_rows(df3, temp)
  }
  
}

#################################################################################
# CORRECT BIASES BASED ON THE NOT-IN-FIELD DATA
#
dfc <- df3

diffs_all <- data.frame()
for(i in sites){
  
  office <- df2 %>% filter(site == i) %>% 
    filter(probl == 2) %>% pull(date)
  office <- office[-which(office == max(office))]
  
  df %>% filter(site == i) %>%
    mutate(date = as_date(datetime)) %>% 
    filter(date %in% office) %>% 
    mutate(change1a = abs(T3 - lag(T3,1)),
           change1b = abs(T3 - lag(T3,2)),
           change1c = abs(T3 - lag(T3,3)),
           change1d = abs(T3 - lag(T3,4)),
           change1e = abs(T3 - lag(T3,5)),
           change1f = abs(T3 - lag(T3,6)),
           change1g = abs(T3 - lead(T3,1)),
           change1h = abs(T3 - lead(T3,2)),
           change1i = abs(T3 - lead(T3,3))) %>% 
    rowwise() %>%
    mutate(change1 = max(change1a, change1b, change1c,
                         change1d, change1e, change1f,
                         change1g, change1g, change1i, na.rm = T)) %>%
    mutate(T3 = ifelse(change1 > 0.1250, NA, T3)) %>% 
    filter(!is.na(T3)) %>% 
    as.data.frame() -> temp
  
  means <- c(T1 = mean(temp$T1),
             T2 = mean(temp$T2),
             T3 = mean(temp$T3))
  
  diffs <- round(means - median(means),4)
  
  dfc %>% mutate(T1 = ifelse(site == i,T1 - diffs["T1"],T1),
                 T2 = ifelse(site == i,T2 - diffs["T2"],T2),
                 T3 = ifelse(site == i,T3 - diffs["T3"],T3)) -> dfc
  
  print(i)
  print(diffs)
  
  diffs_all <- bind_rows(diffs_all,
                         bind_cols(data.frame(site = i), 
                                   as.data.frame(t(as.data.frame(diffs)))))
  
}

fwrite(diffs_all, "output/Correction_temperatures.csv")

###################################################################################
# Delete erroneous data
#

dfc %>% mutate(date = as_date(datetime)) %>%
  left_join(., df2 %>% select(site, date, probl)) %>% 
  filter(probl != 2) -> dfc

dfc %>% mutate(h = hour(datetime)) %>% 
  group_by(site, date) %>% 
  summarise(nh = length(unique(h))) %>% 
  filter(nh != 24) -> missh

dfc %>% left_join(., missh) %>% 
  mutate(T1 = replace(T1, !is.na(nh), NA),
         T2 = replace(T2, !is.na(nh), NA),
         T3 = replace(T3, !is.na(nh), NA),
         moist = replace(moist, !is.na(nh), NA)) %>%
  select(-nh,-zone) -> dfc

##############################################################
# DETECT ANOMALIES CROSS-RELATING THE SITES

my_sd = function(x) {
  if(length(x) %% 2 == 0L) { return(sd(x, na.rm = T)) }
  if(length(x) %% 2 == 1L) { if(length(x) == 3){
    return(sd(x[-ceiling(0.5*length(x))])) 
  } else {
    mid <- ceiling(0.5*length(x))
    return(sd(x[-c(mid-1, mid, mid+1)], na.rm = T))
  }  }
}
my_mean = function(x) {
  if(length(x) %% 2 == 0L) { return(mean(x)) }
  if(length(x) %% 2 == 1L) {
    if(length(x) == 3){
      return(mean(x[-ceiling(0.5*length(x))])) 
    } else {
      if(length(x) == 5){
        mid <- ceiling(0.5*length(x))
        return(mean(x[-c(mid-1, mid, mid+1)]))
      } else {
        mid <- ceiling(0.5*length(x))
        return(mean(x[-c(mid-2, mid-1, mid, mid+1, mid+2)]))
      } 
    }
  }
}



dfc %>% mutate(my = paste0(year(date),"_",month(date))) -> dfc

dfall <- data.frame()
pdf("visuals/Temperature_graphs_spikes.pdf", 10, 12)
for(i in sites){
  #i <- 529
  
  print(i)
  dfc %>% filter(site == i) %>% 
    filter(probl != 1) %>% 
    mutate(timediff1 = as.numeric(datetime - lag(datetime)),
           timediff2 = as.numeric(lead(datetime) - datetime)) %>% 
    filter(timediff1 %in% c(10,15)|timediff2 %in% c(10,15)) %>% 
    mutate(timediff1 = as.numeric(datetime - lag(datetime)),
           timediff2 = as.numeric(lead(datetime) - datetime)) -> temp
  
  if(temp %>% pull(timediff1) %>% min(., na.rm = T) < 10){
    temp %>% filter(timediff1 + timediff2 >= 20 | is.na(timediff1 + timediff2)) -> temp
  }
  
  dftemp <- data.frame()
  for(ii in unique(temp$my)){
    #ii <- "2018_7"
    print(ii)
    
    # T1
    
    temp %>% filter(my == ii) %>% 
      select(datetime, T1, site) %>% 
      filter(complete.cases(.)) %>% 
      rename(T1f = T1,
             site2 = site) -> temp2
    
    rows <- NROW(temp2)
    
    temp2 %>%
      left_join(., dfc) %>% 
      filter(site != i) %>% 
      arrange(site, datetime) %>% 
      mutate(me = abs(T1f-T1)) %>% 
      group_by(site) %>% 
      summarise(me = mean(me),
                n = n()) %>% 
      filter(n > rows*0.95) %>% 
      arrange(me) %>% pull(site) -> mes
    
    if(length(mes) > 0){
      temp2 %>%
        left_join(., dfc %>% filter(site == mes[1])) %>% 
        mutate(me = T1f-T1) %>% 
        mutate(lag_T1 = me - lag(me)) %>% 
        mutate(lead_T1 = me - lead(me)) %>%
        mutate(lag_T1f = T1f - lag(T1f)) %>% 
        mutate(lead_T1f = T1f - lead(T1f)) %>% 
        mutate(error = ifelse(abs(lead_T1) > 3 & abs(lag_T1) > 3, 1, 0)) %>% 
        mutate(error = ifelse(abs(me) > 5 & abs(lag_T1)+abs(lead_T1) > 2, 1, error)) %>% 
        mutate(error = error + lag(error) + lead(error)) %>% 
        mutate(error = ifelse(error > 1, 1, error)) %>% 
        mutate(T1f = ifelse((!is.na(error) & !is.na(lag_T1f) & error == 1 & abs(lag_T1f) >= 0.375) | (!is.na(error) & !is.na(lead_T1f) & error == 1 & abs(lead_T1f) >= 0.375), NA, T1f),
               me = ifelse((!is.na(error) & !is.na(lag_T1f) & error == 1 & abs(lag_T1f) >= 0.375) | (!is.na(error) & !is.na(lead_T1f) & error == 1 & abs(lead_T1f) >= 0.375), NA, me)) %>% 
        mutate(error = ifelse(error == 1 & !is.na(T1f), 0, error)) %>% 
        mutate(fill = rollapply(me, width=3, FUN=my_mean, fill = NA) + T1,
               T1f = ifelse(is.na(T1f), fill, T1f),
               fill = rollapply(me, width=5, FUN=my_mean, fill = NA) + T1,
               T1f = ifelse(is.na(T1f), fill, T1f),
               fill = rollapply(me, width=7, FUN=my_mean, fill = NA) + T1,
               T1f = ifelse(is.na(T1f), fill, T1f)) -> temp2
      
      # temp2 %>% as.matrix() %>% edit()
      temp2 %>% ggplot(aes_string(x="datetime")) +
        geom_line(aes_string(y = "T1f"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T1"), col = "brown1") +
        geom_line(aes_string(y = "me"), col = "darkgoldenrod") +
        #geom_line(aes_string(y = "rollsd_T1"), col = "green") +
        geom_line(aes_string(y = "error"), col = "black") +
        theme_minimal() +
        ylab("T1") + xlab("Date")+
        ggtitle(paste(i, ii)) -> GG1
      
      temp2 %>% mutate(me = T1f-T1) %>% 
        mutate(rollsd_T1 = rollapply(me, width=33, FUN=my_sd, fill = NA)+0.001) %>% 
        mutate(lag_T1 = me - lag(me)) %>% 
        mutate(lead_T1 = me - lead(me)) %>% 
        mutate(lag_T1f = T1f - lag(T1f)) %>% 
        mutate(lead_T1f = T1f - lead(T1f)) %>% 
        mutate(fac = (abs(lag_T1)+abs(lead_T1))/rollsd_T1) %>% 
        mutate(error = ifelse(fac > 10, 1, 0)) %>% 
        mutate(error = ifelse(abs(me) > 1 & abs(lag_T1) > 1 & fac > 4, 1, error),
               error = ifelse(abs(me) > 1 & abs(lead_T1) > 1 & fac > 4, 1, error)) %>% 
        mutate(error = ifelse(error == 1 & lead_T1f*lag_T1f >= 0, 1, error)) %>% 
        mutate(error = error + lag(error) + lead(error)) %>% 
        mutate(error = ifelse(error > 1, 1, error)) %>% 
        mutate(T1f = ifelse((!is.na(error) & !is.na(lag_T1f) & error == 1 & abs(lag_T1f) >= 0.375) | (!is.na(error) & !is.na(lead_T1f) & error == 1 & abs(lead_T1f) >= 0.375), NA, T1f),
               me = ifelse((!is.na(error) & !is.na(lag_T1f) & error == 1 & abs(lag_T1f) >= 0.375) | (!is.na(error) & !is.na(lead_T1f) & error == 1 & abs(lead_T1f) >= 0.375), NA, me)) %>% 
        mutate(error = ifelse(error == 1 & !is.na(T1f), 0, error)) %>% 
        mutate(fill = rollapply(me, width=3, FUN=my_mean, fill = NA) + T1,
               T1f = ifelse(is.na(T1f), fill, T1f),
               fill = rollapply(me, width=5, FUN=my_mean, fill = NA) + T1,
               T1f = ifelse(is.na(T1f), fill, T1f),
               fill = rollapply(me, width=7, FUN=my_mean, fill = NA) + T1,
               T1f = ifelse(is.na(T1f), fill, T1f)) -> temp_T1
      
      temp_T1 %>% ggplot(aes_string(x="datetime")) +
        geom_line(aes_string(y = "T1f"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T1"), col = "brown1") +
        geom_line(aes_string(y = "me"), col = "darkgoldenrod") +
        geom_line(aes_string(y = "rollsd_T1"), col = "green") +
        geom_line(aes_string(y = "error"), col = "black") +
        theme_minimal() +
        ylab("T1") + xlab("Date")+
        ggtitle(paste(i, ii)) -> GG2
    } else {
      temp %>% filter(my == ii) %>% 
        select(datetime, T1) %>% 
        rename(T1f = T1) -> temp_T1
    }
    
    #################################### 
    # T2
    
    temp %>% filter(my == ii) %>% 
      select(datetime, T2) %>% 
      filter(complete.cases(.)) %>% 
      rename(T2f = T2) -> temp2
    
    rows <- NROW(temp2)
    
    temp2 %>%
      left_join(., dfc) %>% 
      filter(site != i) %>% 
      arrange(site, datetime) %>% 
      mutate(me = abs(T2f-T2)) %>% 
      group_by(site) %>% 
      summarise(me = mean(me),
                n = n()) %>% 
      filter(n > rows*0.95) %>% 
      arrange(me) %>% pull(site) -> mes
    
    if(length(mes) > 0){
      
      temp2 %>%
        left_join(., dfc %>% filter(site == mes[1])) %>% 
        mutate(me = T2f-T2) %>% 
        mutate(lag_T2 = me - lag(me)) %>% 
        mutate(lead_T2 = me - lead(me)) %>%
        mutate(lag_T2f = T2f - lag(T2f)) %>% 
        mutate(lead_T2f = T2f - lead(T2f)) %>% 
        mutate(error = ifelse(abs(lead_T2) > 10 & abs(lag_T2) > 10, 1, 0)) %>% 
        mutate(error = ifelse(abs(me) > 10 & abs(lag_T2)+abs(lead_T2) > 10, 1, error)) %>% 
        mutate(error = error + lag(error) + lead(error)) %>% 
        mutate(error = ifelse(error > 1, 1, error)) %>% 
        mutate(T2f = ifelse((!is.na(error) & !is.na(lag_T2f) & error == 1 & abs(lag_T2f) >= 0.375) | (!is.na(error) & !is.na(lead_T2f) & error == 1 & abs(lead_T2f) >= 0.375), NA, T2f),
               me = ifelse((!is.na(error) & !is.na(lag_T2f) & error == 1 & abs(lag_T2f) >= 0.375) | (!is.na(error) & !is.na(lead_T2f) & error == 1 & abs(lead_T2f) >= 0.375), NA, me)) %>% 
        mutate(error = ifelse(error == 1 & !is.na(T2f), 0, error)) %>% 
        mutate(fill = rollapply(me, width=3, FUN=my_mean, fill = NA) + T2,
               T2f = ifelse(is.na(T2f), fill, T2f),
               fill = rollapply(me, width=5, FUN=my_mean, fill = NA) + T2,
               T2f = ifelse(is.na(T2f), fill, T2f),
               fill = rollapply(me, width=7, FUN=my_mean, fill = NA) + T2,
               T2f = ifelse(is.na(T2f), fill, T2f)) -> temp2
      
      # temp2 %>% as.matrix() %>% edit()
      temp2 %>% ggplot(aes_string(x="datetime")) +
        geom_line(aes_string(y = "T2f"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T2"), col = "brown1") +
        geom_line(aes_string(y = "me"), col = "darkgoldenrod") +
        #geom_line(aes_string(y = "rollsd_T2"), col = "green") +
        geom_line(aes_string(y = "error"), col = "black") +
        theme_minimal() +
        ylab("T2") + xlab("Date") -> GG3
      
      temp2 %>% mutate(time = paste(hour(datetime), minute(datetime), sep = ":")) %>% 
        group_by(time) %>% summarise(mean_me = quantile(me, 0.97, na.rm = T),
                                     min_me = quantile(me, 0.03, na.rm = T)) %>% 
        right_join(temp2 %>% mutate(time = paste(hour(datetime), minute(datetime), sep = ":"))) %>%
        arrange(datetime) %>% 
        as.data.table() -> temp2
      
      temp2 %>% mutate(me = T2f-T2) %>% 
        mutate(rollsd_T2 = rollapply(me, width=33, FUN=my_sd, fill = NA)+0.001) %>% 
        mutate(lag_T2 = me - lag(me)) %>% 
        mutate(lead_T2 = me - lead(me)) %>% 
        mutate(lag_T2f = T2f - lag(T2f)) %>% 
        mutate(lead_T2f = T2f - lead(T2f)) %>% 
        mutate(fac = (abs(lag_T2)+abs(lead_T2))/rollsd_T2) %>% 
        mutate(error = ifelse(fac > 10, 1, 0)) %>% 
        mutate(error = ifelse(abs(me) > 3 & abs(lag_T2) > 3 & fac > 5, 1, error),
               error = ifelse(abs(me) > 3 & abs(lead_T2) > 3 & fac > 5, 1, error)) %>% 
        mutate(error = ifelse(me > 0 & mean_me > 0 & me < mean_me*2, 0, error)) %>% 
        mutate(error = ifelse(me < 0 & mean_me < 0 & me > min_me*2, 0, error)) %>% 
        mutate(error = ifelse(error == 1 & lead_T2f*lag_T2f >= 0, 1, error)) %>% 
        mutate(error = error + lag(error) + lead(error)) %>% 
        mutate(error = ifelse(error > 1, 1, error)) %>% #as.matrix() %>% edit()
        mutate(T2f = ifelse((!is.na(error) & !is.na(lag_T2f) & error == 1 & abs(lag_T2f) >= 0.375) | (!is.na(error) & !is.na(lead_T2f) & error == 1 & abs(lead_T2f) >= 0.375), NA, T2f),
               me = ifelse((!is.na(error) & !is.na(lag_T2f) & error == 1 & abs(lag_T2f) >= 0.375) | (!is.na(error) & !is.na(lead_T2f) & error == 1 & abs(lead_T2f) >= 0.375), NA, me)) %>% 
        mutate(error = ifelse(error == 1 & !is.na(T2f), 0, error)) %>% 
        mutate(fill = rollapply(me, width=3, FUN=my_mean, fill = NA) + T2,
               T2f = ifelse(is.na(T2f), fill, T2f),
               fill = rollapply(me, width=5, FUN=my_mean, fill = NA) + T2,
               T2f = ifelse(is.na(T2f), fill, T2f),
               fill = rollapply(me, width=7, FUN=my_mean, fill = NA) + T2,
               T2f = ifelse(is.na(T2f), fill, T2f)) -> temp_T2
      
      temp_T2 %>% ggplot(aes_string(x="datetime")) +
        geom_line(aes_string(y = "T2f"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T2"), col = "brown1") +
        geom_line(aes_string(y = "me"), col = "darkgoldenrod") +
        geom_line(aes_string(y = "rollsd_T2"), col = "green") +
        geom_line(aes_string(y = "error"), col = "black") +
        theme_minimal() +
        ylab("T2") + xlab("Date") -> GG4
      
    } else {
      temp %>% filter(my == ii) %>% 
        select(datetime, T2) %>% 
        rename(T2f = T2)  -> temp_T2
    }
    
    #######################################3
    # T3
    
    temp %>% filter(my == ii) %>% 
      select(datetime, T3) %>% 
      filter(complete.cases(.)) %>% 
      rename(T3f = T3) -> temp2
    
    rows <- NROW(temp2)
    
    temp2 %>%
      left_join(., dfc) %>% 
      filter(site != i) %>% 
      arrange(site, datetime) %>% 
      mutate(me = abs(T3f-T3)) %>% 
      group_by(site) %>% 
      summarise(me = mean(me),
                n = n()) %>% 
      filter(n > rows*0.95) %>% 
      arrange(me) %>% pull(site) -> mes
    
    if(length(mes) > 0){
      
      temp2 %>%
        left_join(., dfc %>% filter(site == mes[1])) %>% 
        mutate(me = T3f-T3) %>% 
        mutate(lag_T3 = me - lag(me)) %>% 
        mutate(lead_T3 = me - lead(me)) %>%
        mutate(lag_T3f = T3f - lag(T3f)) %>% 
        mutate(lead_T3f = T3f - lead(T3f)) %>% 
        mutate(error = ifelse(abs(lead_T3) > 10 & abs(lag_T3) > 10, 1, 0)) %>% 
        mutate(error = ifelse(abs(me) > 10 & abs(lag_T3)+abs(lead_T3) > 10, 1, error)) %>% 
        mutate(error = error + lag(error) + lead(error)) %>% 
        mutate(error = ifelse(error > 1, 1, error)) %>% 
        mutate(T3f = ifelse((!is.na(error) & !is.na(lag_T3f) & error == 1 & abs(lag_T3f) >= 0.375) | (!is.na(error) & !is.na(lead_T3f) & error == 1 & abs(lead_T3f) >= 0.375), NA, T3f),
               me = ifelse((!is.na(error) & !is.na(lag_T3f) & error == 1 & abs(lag_T3f) >= 0.375) | (!is.na(error) & !is.na(lead_T3f) & error == 1 & abs(lead_T3f) >= 0.375), NA, me)) %>% 
        mutate(error = ifelse(error == 1 & !is.na(T3f), 0, error)) %>% 
        mutate(fill = rollapply(me, width=3, FUN=my_mean, fill = NA) + T3,
               T3f = ifelse(is.na(T3f), fill, T3f),
               fill = rollapply(me, width=5, FUN=my_mean, fill = NA) + T3,
               T3f = ifelse(is.na(T3f), fill, T3f),
               fill = rollapply(me, width=7, FUN=my_mean, fill = NA) + T3,
               T3f = ifelse(is.na(T3f), fill, T3f)) -> temp2
      
      # temp2 %>% as.matrix() %>% edit()
      temp2 %>% ggplot(aes_string(x="datetime")) +
        geom_line(aes_string(y = "T3f"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T3"), col = "brown1") +
        geom_line(aes_string(y = "me"), col = "darkgoldenrod") +
        #geom_line(aes_string(y = "rollsd_T3"), col = "green") +
        geom_line(aes_string(y = "error"), col = "black") +
        theme_minimal() +
        ylab("T3") + xlab("Date") -> GG5
      
      temp2 %>% mutate(time = paste(hour(datetime), minute(datetime), sep = ":")) %>% 
        group_by(time) %>% summarise(mean_me = quantile(me, 0.97, na.rm = T),
                                     min_me = quantile(me, 0.03, na.rm = T)) %>% 
        right_join(temp2 %>% mutate(time = paste(hour(datetime), minute(datetime), sep = ":"))) %>%
        arrange(datetime) %>% 
        as.data.table() -> temp2
      
      temp2 %>% mutate(me = T3f-T3) %>% 
        #filter(datetime >= "2020-07-05 07:00:00") %>% 
        mutate(rollsd_T3 = rollapply(me, width=33, FUN=my_sd, fill = NA)+0.001) %>% 
        mutate(lag_T3 = me - lag(me)) %>% 
        mutate(lead_T3 = me - lead(me)) %>% 
        mutate(lag_T3f = T3f - lag(T3f)) %>% 
        mutate(lead_T3f = T3f - lead(T3f)) %>% 
        mutate(fac = (abs(lag_T3)+abs(lead_T3))/rollsd_T3) %>% 
        mutate(error = ifelse(fac > 10, 1, 0)) %>% 
        mutate(error = ifelse(abs(me) > 4 & abs(lag_T3) > 4 & fac > 7, 1, error),
               error = ifelse(abs(me) > 4 & abs(lead_T3) > 4 & fac > 7, 1, error)) %>% 
        mutate(error = ifelse(me > 0 & mean_me > 0 & me < mean_me*2, 0, error)) %>% 
        mutate(error = ifelse(me < 0 & mean_me < 0 & me > min_me*2, 0, error)) %>% 
        mutate(error = ifelse(error == 1 & lead_T3f*lag_T3f >= 0, 1, error)) %>% 
        mutate(error = error + lag(error) + lead(error)) %>% 
        mutate(error = ifelse(error > 1, 1, error)) %>% 
        mutate(T3f = ifelse((!is.na(error) & !is.na(lag_T3f) & error == 1 & abs(lag_T3f) >= 0.375) | (!is.na(error) & !is.na(lead_T3f) & error == 1 & abs(lead_T3f) >= 0.375), NA, T3f),
               me = ifelse((!is.na(error) & !is.na(lag_T3f) & error == 1 & abs(lag_T3f) >= 0.375) | (!is.na(error) & !is.na(lead_T3f) & error == 1 & abs(lead_T3f) >= 0.375), NA, me)) %>% 
        mutate(error = ifelse(error == 1 & !is.na(T3f), 0, error)) %>% 
        mutate(fill = rollapply(me, width=3, FUN=my_mean, fill = NA) + T3,
               T3f = ifelse(is.na(T3f), fill, T3f),
               fill = rollapply(me, width=5, FUN=my_mean, fill = NA) + T3,
               T3f = ifelse(is.na(T3f), fill, T3f)) -> temp_T3
      
      # temp_T3 %>% filter(datetime > "2020-01-27") %>% as.matrix() %>% edit()
      temp_T3 %>% ggplot(aes_string(x="datetime")) +
        geom_line(aes_string(y = "T3f"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T3"), col = "brown1") +
        geom_line(aes_string(y = "me"), col = "darkgoldenrod") +
        geom_line(aes_string(y = "rollsd_T3"), col = "green") +
        geom_line(aes_string(y = "error"), col = "black") +
        theme_minimal() +
        ylab("T3") + xlab("Date") -> GG6
      
      print(plot_grid(plotlist = list(GG1,GG2,GG3,GG4,GG5,GG6), nrow = 3))
      
    } else {
      temp %>% filter(my == ii) %>% 
        select(datetime, T3) %>% 
        rename(T3f = T3) -> temp_T3
    }
    
    temp2 <- full_join(temp %>% filter(my == ii) %>% select(site, datetime, moist, date, probl),
                       temp_T1 %>% select(datetime, T1f) %>% 
                         rename(T1 = T1f))
    temp2 <- full_join(temp2,
                       temp_T2 %>% select(datetime, T2f) %>% 
                         rename(T2 = T2f))
    temp2 <- full_join(temp2,
                       temp_T3 %>% select(datetime, T3f) %>% 
                         rename(T3 = T3f))
    
    dftemp <- bind_rows(dftemp,temp2)
    
  }
  dfall <- bind_rows(dfall, dftemp)
}
dev.off()

###############################################################################
# PLOT CORRECTED

pdf("visuals/Temperature_graphs_corrected.pdf", 10, 5)
for(i in sites){
  #i <- sites[3]
  print(i)
  dfall %>% filter(site == i) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
    geom_line(aes_string(y = "T2"), col = "brown1") +
    geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-20, 35))+
    ggtitle(i) -> GG
  print(GG)
  
}
dev.off()

dfall %>% 
  bind_rows(., dfc %>% select(-my) %>% filter(probl == 1)) %>% 
  arrange(site, datetime) -> dfall

for(i in sites){
  
  dfall %>% filter(site == i) %>% pull(date) %>% max() -> maxd
  
  dfall %>% filter(!c(site == i & date == maxd)) -> dfall
  
}

round2 <- function(x) round(x,2)

fwrite(dfall %>% select(-date) %>% 
         mutate(across(T1:T3, round2)), "output/tomst_data.csv")

####################################################################################
