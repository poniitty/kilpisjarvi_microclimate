
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
             ceiling_date(as_date(max(df2$date)), "month") - days(1),
             by = "month")

df2 %>% mutate(probl = 0) -> df2
for(siteid in sites){
  #siteid <- 120
  print(siteid)
  pdf(paste0("visuals/monthly_", siteid, ".pdf"), 10, 6)
  for (tt in 1:(length(times) - 1)) {
    if (!ymd(times[tt + 1]) < min(df %>% filter(site == siteid) %>% pull(datetime))) {
      df %>% filter(site == siteid) %>%
        filter(datetime >= ymd(times[tt]),
               datetime < ymd(times[tt + 1])) %>%
        ggplot(aes_string(x = "datetime")) +
        geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T2"), col = "brown1") +
        geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
        theme_minimal() +
        ylab("Temperature") + xlab("Date") +
        ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
        scale_x_datetime(date_minor_breaks = "1 day") -> GG1
      
      df %>% filter(site == siteid) %>%
        filter(datetime >= ymd(times[tt]),
               datetime < ymd(times[tt + 1])) %>%
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

# SITE = 1
siteid <- 1

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c(as_date(as_date("2020-05-28"):as_date("2020-08-01")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 3
siteid <- 3

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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
# SITE = 8
siteid <- 8

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c(as_date(as_date("2020-07-21"):as_date("2020-08-01")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 11
siteid <- 11

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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
# SITE = 15
siteid <- 15

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-14")))
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
# SITE = 17
siteid <- 17

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-14")))
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
# SITE = 22
siteid <- 22

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
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
# SITE = 25
siteid <- 25

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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
# SITE = 27
siteid <- 27

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
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

# SITE = 29
siteid <- 29

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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
# SITE = 31
siteid <- 31

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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
# SITE = 36
siteid <- 36

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-14")))
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
# SITE = 40
siteid <- 40

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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
# SITE = 42
siteid <- 42

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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
# SITE = 44
siteid <- 44

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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
# SITE = 46
siteid <- 46

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 50
siteid <- 50

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 52
siteid <- 52

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 56
siteid <- 56

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c(as_date(as_date("2019-09-12"):as_date("2020-08-01")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 60
siteid <- 60

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 63
siteid <- 63

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 64
siteid <- 64

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 65
siteid <- 65

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 66
siteid <- 66

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 68
siteid <- 68

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 70
siteid <- 70

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 71
siteid <- 71

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c(as_date(as_date("2019-09-13"):as_date("2020-08-02")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 73
siteid <- 73

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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

# SITE = 77
siteid <- 77

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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

# SITE = 79
siteid <- 79

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 83
siteid <- 83

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
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

# SITE = 88
siteid <- 88

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 92
siteid <- 92

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 95
siteid <- 95

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 97
siteid <- 97

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 98
siteid <- 98

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 99
siteid <- 99

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
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

# SITE = 102
siteid <- 102

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
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

# SITE = 104
siteid <- 104

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
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

# SITE = 107
siteid <- 107

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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

# SITE = 109
siteid <- 109

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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

# SITE = 110
siteid <- 110

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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

# SITE = 113
siteid <- 113

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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

# SITE = 116
siteid <- 116

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
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

# SITE = 118
siteid <- 118

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
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

# SITE = 121
siteid <- 121

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
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

# SITE = 123
siteid <- 123

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
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

# SITE = 125
siteid <- 125

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
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
