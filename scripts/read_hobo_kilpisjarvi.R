library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

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

parse_time_own <- function(x){
  if(nchar(x) == 4){
    return(paste(substr(x, 1, 2), substr(x, 3, 4), sep = ":"))
  }
  if(nchar(x) == 3){
    return(paste(substr(x, 1, 1), substr(x, 2, 3), sep = ":"))
  }
  if(nchar(x) != 3 | nchar(x) != 4){
    return(NA)
  }
}


maxdt20 <- bind_rows(read_csv("data/reading_times_2020_AIL.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("AIL", add_zeros(x))))),
                     read_csv("data/reading_times_2020_MAL.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("MAL", add_zeros(x))))),
                     read_csv("data/reading_times_2020_SAA.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("SAA", add_zeros(x))))))
maxdt20 %>%  mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2")) -> maxdt20

maxdt21 <- read_csv2("data/rarearctic_memo_2021.csv") %>% 
  mutate(site = toupper(site)) %>% 
  mutate(date = as_date(date),
         time = parse_time_own(time)) %>% 
  mutate(maxdt = ymd_hm(paste(date, time), tz = "Etc/GMT-3")) %>% 
  mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(maxdt))

# Is there duplicated sites?
maxdt21$site[duplicated(maxdt21$site)]

maxdt21 %>% filter(site == "RA068")

# manual fix
maxdt21 <- maxdt21 %>% filter(!(site == "RA068" & date == "2021-08-08"))

# Addition to 2020 visiting times
maxdt20 <- bind_rows(maxdt20,
                     read_csv2("data/hobo_install_2020_manual.csv") %>% 
                       mutate(site = toupper(site)) %>% 
                       mutate(maxdt = ymd_hms(paste(date, time), tz = "Etc/GMT-3")) %>% 
                       mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2")) %>% 
                       select(site, maxdt))

#################################################################
# List logger data files to rea
f <- list.files("data/hobo",pattern = ".csv$", full.names = T)

fi <- data.frame(file = f)

fi$site <- toupper(gsub(".csv", "", unlist(lapply(fi$file, function(x) strsplit(x, "/")[[1]][3]))))

fi <- fi %>% arrange(site)

# Check if all HOBO sites are in the reading time files
fi %>% filter(!site %in% maxdt20$site) # Good if none
fi %>% filter(!site %in% maxdt21$site) # Good if none

readHOBO <- function(i){
  
  print(i)
  
  d <- read_csv(i, skip = 1) %>% 
    select(2:4)
  
  names(d) <- c("datetime", "at", "arh")
  
  d %>% filter(!(is.na(at) & is.na(arh))) -> d
  
  d %>% mutate(datetime = gsub(" ap.", " am", datetime),
               datetime = gsub(" ip.", " pm", datetime)) -> d
  
  d$site <- fi[which(fi$file == i),"site"]
  
  d %>% mutate(datetime = mdy_hms(datetime, tz = "Etc/GMT-3")) %>% 
    mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) -> d
  
  return(d)
  
}


mylist <- lapply(fi$file, readHOBO)
df <- rbindlist( mylist )

df %>% arrange(site, datetime) -> df

sites <- unique(df$site)

# Remove measurements prior installing
full_join(df, maxdt20 %>% select(-tomst_id)) %>% 
  filter(datetime >= maxdt) %>%
  select(-maxdt) -> df

# Remove measurements after 2021 visit
full_join(df, maxdt21 %>% select(site, maxdt)) %>% 
  filter(datetime < maxdt) %>%
  select(-maxdt) -> df

df %>% mutate(roundtime = round_date(datetime, "30 minutes")) -> df

df %>% group_by(roundtime) %>% 
  summarise(med_at = median(at, na.rm = T),
            med_arh = median(arh, na.rm = T)) -> meds

df <- full_join(df, meds)

pdf("visuals/Hobo_graphs.pdf", 17, 10)
for(i in sites){
  #i <- sites[3]
  print(i)
  
  df %>% filter(site == i) %>%
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "med_at"), col = "black", linetype = "dashed") +
    geom_line(aes_string(y = "at"), col = "cornflowerblue") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-30, 35))+
    ggtitle(i) -> GG1
  
  df %>% filter(site == i) %>%
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "med_arh"), col = "black", linetype = "dashed") +
    geom_line(aes_string(y = "arh"), col = "cornflowerblue") +
    theme_minimal() +
    ylab("Air humidity") + xlab("Date")+
    scale_y_continuous(limits = c(0, 100)) -> GG2
  
  
  print(plot_grid(plotlist = list(GG1,GG2), nrow = 2))
  
}
dev.off()

####################################################################
# SPOT AND MARK ERRORS AND SUSPICIOUS ONES
# Site AIL178, AIL184 reported to have fallen to the ground

df2 <- data.frame()
for(i in sites){
  
  print(i)
  
  df %>% filter(site == i) %>% 
    mutate(cor85_at = rollapply(., 85 ,function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"med_at"])),
                                by.column=FALSE, fill = NA, partial = T)) %>% 
    mutate(cor85_arh = rollapply(., 85 ,function(x) cor(as.numeric(x[,"arh"]),
                                                        as.numeric(x[,"med_arh"])),
                                 by.column=FALSE, fill = NA, partial = T)) %>% 
    mutate(date = as_date(datetime)) %>% 
    mutate(rollsd_at = rollapply(at, width=61, FUN=sd, fill = NA, partial = T)) %>% 
    mutate(rollsd_arh = rollapply(arh, width=61, FUN=sd, fill = NA, partial = T)) %>% 
    mutate(abser_at = abs(at-med_at),
           abser_arh = abs(arh-med_arh)) %>% 
    mutate(rollabser_at = rollapply(abser_at, width=61, FUN=mean, fill = NA, partial = T)) %>% 
    mutate(rollabser_arh = rollapply(abser_arh, width=61, FUN=mean, fill = NA, partial = T)) -> temp
  
  
  
  df2 <- bind_rows(df2, temp)
  
}

df2 %>% group_by(site, date) %>% 
  summarise(mean_at = mean(at),
            min_at = min(at),
            max_at = max(at),
            sd_at = sd(at),
            sd_arh = sd(arh),
            meansd_at = mean(rollsd_at),
            meansd_arh = mean(rollsd_arh),
            mean_arh = mean(arh),
            min_arh = min(arh),
            cor85_at = min(cor85_at),
            cor85_arh = min(cor85_arh),
            rollabser_at = mean(rollabser_at),
            rollabser_arh = mean(rollabser_arh)) %>% 
  ungroup() %>% as.data.table() -> daily

daily %>% mutate(hobo_probl = 0) -> daily

###################################################
# Look into problematic sites one by one
# AIL178
site_id <- "AIL178"
daterange <- c("2020-12-01","2021-01-31")
# df %>% filter(site == site_id) %>% 
#   filter(datetime > daterange[1],
#          datetime < daterange[2]) %>% 
#   ggplot(aes_string(x="datetime")) +
#   geom_line(aes_string(y = "med_at"), col = "black") +
#   geom_line(aes_string(y = "at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("Temperature") + xlab("Date")
# df %>% filter(site == site_id) %>% 
#   filter(datetime > daterange[1],
#          datetime < daterange[2]) %>% 
#   ggplot(aes_string(x="datetime")) +
#   geom_line(aes_string(y = "med_arh"), col = "black") +
#   geom_line(aes_string(y = "arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("Temperature") + xlab("Date")

daily %>% 
  mutate(hobo_probl = ifelse(site == site_id & date > "2021-01-10", 1, hobo_probl)) -> daily

# AIL183
site_id <- "AIL183"
daterange <- c("2021-02-01","2021-04-01")
# df %>% filter(site == site_id) %>% 
#   filter(datetime > daterange[1],
#          datetime < daterange[2]) %>% 
#   ggplot(aes_string(x="datetime")) +
#   geom_line(aes_string(y = "med_at"), col = "black") +
#   geom_line(aes_string(y = "at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("Temperature") + xlab("Date")
# df %>% filter(site == site_id) %>% 
#   filter(datetime > daterange[1],
#          datetime < daterange[2]) %>% 
#   ggplot(aes_string(x="datetime")) +
#   geom_line(aes_string(y = "med_arh"), col = "black") +
#   geom_line(aes_string(y = "arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("Temperature") + xlab("Date")

daily %>% 
  mutate(hobo_probl = ifelse(site == site_id & date > "2021-02-18", 1, hobo_probl)) -> daily

# AIL184
site_id <- "AIL184"
daterange <- c("2021-01-01","2021-03-01")
# df %>% filter(site == site_id) %>% 
#   filter(datetime > daterange[1],
#          datetime < daterange[2]) %>% 
#   ggplot(aes_string(x="datetime")) +
#   geom_line(aes_string(y = "med_at"), col = "black") +
#   geom_line(aes_string(y = "at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("Temperature") + xlab("Date")
# df %>% filter(site == site_id) %>% 
#   filter(datetime > daterange[1],
#          datetime < daterange[2]) %>% 
#   ggplot(aes_string(x="datetime")) +
#   geom_line(aes_string(y = "med_arh"), col = "black") +
#   geom_line(aes_string(y = "arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("Temperature") + xlab("Date")

daily %>% 
  mutate(hobo_probl = ifelse(site == site_id & date > "2021-01-10", 1, hobo_probl)) -> daily


###############################################################
# WHICH ARE UNDER SNOW

daily %>% 
  mutate(cor85_at = ifelse(is.na(cor85_at), 0, cor85_at),
         cor85_arh = ifelse(is.na(cor85_arh), 0, cor85_arh)) %>% 
  mutate(cor85_at = ifelse(!is.finite(cor85_at), 0, cor85_at),
         cor85_arh = ifelse(!is.finite(cor85_at), 0, cor85_arh)) -> daily

# daily %>% filter(site == "MAL056") -> temp

# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "max_at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("max_at") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "min_at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("min_at") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "sd_at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("sd_at") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "sd_arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("sd_arh") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "meansd_at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("meansd_at") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "meansd_arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("meansd_arh") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "mean_arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("mean_arh") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "min_arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("min_arh") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "cor85_at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("cor85_at") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "cor85_arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("cor85_arh") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "rollabser_at"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("rollabser_at") + xlab("Date") + ggtitle(unique(temp$site))
# temp %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "rollabser_arh"), col = "cornflowerblue") +
#   theme_minimal() +
#   ylab("rollabser_arh") + xlab("Date") + ggtitle(unique(temp$site))

count_consecutives <- function(x){
  
  x <- ifelse(is.na(x), 666, x)
  
  x1 <- rle(x)
  
  x2 <- rep(x1$lengths, times = x1$lengths)
  
  return(x2)
}

daily %>% mutate(snow = 0) %>%
  mutate(snow = ifelse(min_arh > 95 & 
                         mean_arh > 95 &
                         meansd_arh < 1.2 &
                         meansd_at < 2 & 
                         sd_arh < 1.2 &
                         sd_at < 2 &
                         min_at > -15 &
                         max_at < 1 &
                         cor85_at < 0.7 &
                         cor85_arh < 0.75 &
                         rollabser_at > 1 &
                         rollabser_arh > 2, 2, snow)) %>% 
  mutate(hobo_probl = ifelse(hobo_probl == 1, hobo_probl, snow)) -> daily

daily %>% group_by(site) %>% 
  mutate(cons_count = count_consecutives(hobo_probl)) %>% 
  ungroup() %>% 
  mutate(hobo_probl = ifelse(hobo_probl == 0 & cons_count < 4, 2, hobo_probl)) %>% 
  group_by(site) %>% 
  mutate(cons_count = count_consecutives(hobo_probl)) %>% 
  ungroup() %>% 
  mutate(hobo_probl = ifelse(hobo_probl == 2 & cons_count < 7, 0, hobo_probl)) %>% 
  group_by(site) %>% 
  mutate(lag_probl = rollapply(hobo_probl, width=6, FUN=max, fill = NA, partial = T, align = "left")) %>% 
  mutate(lead_probl = rollapply(hobo_probl, width=3, FUN=max, fill = NA, partial = T, align = "right")) %>% 
  mutate(hobo_probl = ifelse(hobo_probl == 0 & lag_probl == 2, 2, hobo_probl)) %>% 
  mutate(hobo_probl = ifelse(hobo_probl == 0 & lead_probl == 2, 2, hobo_probl)) %>% 
  ungroup() -> daily2

# daily2 %>% filter(site == "RA080") %>% 
#   ggplot(aes_string(x="date")) +
#   geom_line(aes_string(y = "mean_at"), col = "cornflowerblue") +
#   geom_point(aes(x = date, y = mean_at),
#              data = daily2 %>% filter(site == "RA080") %>% filter(hobo_probl == 2),
#              col = "red") +
#   geom_point(aes(x = date, y = mean_at),
#              data = daily2 %>% filter(site == "RA080") %>% filter(hobo_probl == 1),
#              col = "black") +
#   theme_minimal() +
#   ylab("min_arh") + xlab("Date")

df %>% mutate(date = as_date(datetime)) %>% 
  left_join(., daily2 %>% select(site, date, hobo_probl)) -> df2

pdf("visuals/Hobo_graphs_marked.pdf", 12, 10)
for(i in sites){
  #i <- sites[4]
  print(i)
  
  df2 %>% filter(site == i) %>% 
    mutate(at1 = as.numeric(ifelse(hobo_probl == 1, at, NA))) %>% 
    mutate(at2 = as.numeric(ifelse(hobo_probl == 2, at, NA))) %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "at"), col = "black") +
    geom_line(aes_string(y = "at2"), col = "blue") +
    geom_line(aes_string(y = "at1"), col = "red") +
    theme_minimal() +
    ylab("min_arh") + xlab("Date") +
    scale_y_continuous(limits = c(-30, 35))+
    ggtitle(i) -> GG1
  
  df2 %>% filter(site == i) %>% 
    mutate(arh1 = as.numeric(ifelse(hobo_probl == 1, arh, NA))) %>% 
    mutate(arh2 = as.numeric(ifelse(hobo_probl == 2, arh, NA))) %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "arh"), col = "black") +
    geom_line(aes_string(y = "arh2"), col = "blue") +
    geom_line(aes_string(y = "arh1"), col = "red") +
    theme_minimal() +
    ylab("Air humidity") + xlab("Date")+
    scale_y_continuous(limits = c(0, 100)) -> GG2
  
  
  print(plot_grid(plotlist = list(GG1,GG2), nrow = 2))
  
}
dev.off()

# MANUAL CORRECTION

df2 %>% mutate(hobo_probl = ifelse(site == "AIL174", 0, hobo_probl)) -> df2
df2 %>% mutate(hobo_probl = ifelse(site == "AIL179" & date > "2020-12-15", 3, hobo_probl)) -> df2
df2 %>% mutate(hobo_probl = ifelse(site == "AIL192" & date < "2021-01-01", 0, hobo_probl)) -> df2
df2 %>% mutate(hobo_probl = ifelse(site == "MAL083" & date > "2021-04-01" & date < "2021-05-15", 2, hobo_probl)) -> df2
df2 %>% mutate(hobo_probl = ifelse(site == "MAL089" & date > "2021-04-01" & date < "2021-04-20", 2, hobo_probl)) -> df2

# fwrite(df2 %>% select(-med_at, -med_arh), "output/hobo_data.csv")

#################################################################################
# CORRECT FOR TIME SHIFTS AND TEMPORAL HOLES

d <- df2 %>% select(-med_at, -med_arh)

d %>% filter(hobo_probl == 0) %>% 
  group_by(roundtime) %>% 
  summarise(md = median(at, na.rm = T),
            md_arh = median(arh, na.rm = T)) %>% 
  ungroup() -> md

# plot(md$roundtime, md$md)
df <- data.frame()
for(i in unique(d$site)){
  # i <- "AIL101"
  
  d %>% filter(site == i) -> temp
  
  temp <- full_join(temp, md) %>% as.data.table()
  
  firstd <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max() - days(28)
  lastd_data <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max()
  lastd <- temp %>% pull(roundtime) %>% max() + days(2)
  
  temp %>% filter(roundtime >= firstd) -> move_data
  
  max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
  
  moves <- 0:max_move
  
  cors <- rep(NA, length(moves))
  names(cors) <- moves
  
  for(mi in moves){
    #mi <- 1
    move_data %>% filter(!is.na(at) & !is.na(arh)) %>% 
      select(roundtime, at, arh) %>% 
      mutate(roundtime = roundtime + hours(2*mi)) %>% 
      full_join(move_data %>% select(roundtime, md, md_arh)) %>% 
      arrange(roundtime) %>% 
      filter(complete.cases(.)) -> temp2
    
    # cor1 <- cor(temp2$at, temp2$md)
    # cor2 <- cor(temp2$arh, temp2$md_arh)
    # 
    cor1 <- sqrt(mean((temp2$at - temp2$md)^2))
    cor2 <- sqrt(mean((temp2$arh - temp2$md_arh)^2))
    
    cors[[paste0(mi)]] <- cor1+cor2
  }
  
  move_n <- as.numeric(names(which.min(cors)))
  
  if(move_n  != 0){
    
    print(i)
    
    d %>% filter(site == i) -> temp
    
    temp <- full_join(temp, md) %>% as.data.table()
    
    temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                        as.numeric(x[,"md"])),
                              by.column=FALSE, fill = NA)
    
    temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                            as.numeric(x[,"md_arh"])),
                                  by.column=FALSE, fill = NA)
    
    temp %>% 
      mutate(lead_probl = rollapply(hobo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
    
    temp %>% mutate(tdiff = abs(md-at)) %>% 
      mutate(corr24 = ifelse(corr24<0 | hobo_probl != 0 | lead_probl != 0,0,corr24),
             corr48 = ifelse(corr48<0 | hobo_probl != 0 | lead_probl != 0,0,corr48),
             corr168 = ifelse(corr168<0 | hobo_probl != 0 | lead_probl != 0,0,corr168),
             corr24_arh = ifelse(corr24_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr24_arh),
             corr48_arh = ifelse(corr48_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr48_arh),
             corr168_arh = ifelse(corr168_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr168_arh)) -> temp
    
    temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
      mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
      mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) %>% 
      mutate(comb = ifelse(hobo_probl != 0, 0, comb)) %>% 
      mutate(comb = ifelse(lead_probl != 0, 0, comb)) -> temp
    
    firstd <- temp$roundtime[min(which(temp$comb >= 10))]
    if(is.na(firstd)){
      d %>% filter(site == i) -> temp
      
      temp <- full_join(temp, md) %>% as.data.table()
      
      temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                          as.numeric(x[,"md"])),
                                by.column=FALSE, fill = NA)
      
      temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                              as.numeric(x[,"md_arh"])),
                                    by.column=FALSE, fill = NA)
      
      temp %>% 
        mutate(lead_probl = rollapply(hobo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
      
      temp %>% mutate(tdiff = abs(md-at)) %>% 
        mutate(corr24 = ifelse(corr24<0,0,corr24),
               corr48 = ifelse(corr48<0,0,corr48),
               corr168 = ifelse(corr168<0,0,corr168),
               corr24_arh = ifelse(corr24_arh<0,0,corr24_arh),
               corr48_arh = ifelse(corr48_arh<0,0,corr48_arh),
               corr168_arh = ifelse(corr168_arh<0,0,corr168_arh)) -> temp
      
      temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
        mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
        mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) -> temp
      
      firstd <- temp$roundtime[min(which(temp$comb >= 10))]
    }
    
    lastd_data <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max()
    lastd <- temp %>% pull(roundtime) %>% max() + days(2)
    
    temp %>% filter(roundtime >= firstd) -> move_data
    
    max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
    
    moves <- 0:max_move
    
    cors <- rep(NA, length(moves))
    names(cors) <- moves
    
    for(mi in moves){
      #mi <- 1
      move_data %>% filter(!is.na(at)) %>% 
        select(roundtime, at, arh) %>% 
        mutate(roundtime = roundtime + hours(2*mi)) %>% 
        full_join(move_data %>% select(roundtime, md, md_arh)) %>% 
        arrange(roundtime) %>% 
        filter(complete.cases(.)) -> temp2
      
      cor1 <- sqrt(mean((temp2$at - temp2$md)^2))
      cor2 <- sqrt(mean((temp2$arh - temp2$md_arh)^2))
      
      cors[[paste0(mi)]] <- cor1+cor2
    }
    
    move_n <- as.numeric(names(which.min(cors)))
    print(move_n)
    
    move_data %>% mutate(roundtime = roundtime + hours(2*move_n)) %>% 
      select(roundtime, at, arh, hobo_probl) %>% 
      bind_rows(temp %>% filter(roundtime < firstd) %>% select(roundtime, at, arh, hobo_probl)) %>% 
      full_join(md) %>% arrange(roundtime) %>% 
      filter(!is.na(md)) %>% 
      mutate(site = i) -> temp
    
  }
  
  df <- bind_rows(df, temp %>% select(site, roundtime, at, arh, hobo_probl))
  
}

df %>% filter(!is.na(site)) -> df

# ROUND 2

df2 <- data.frame()
for(i in unique(df$site)){
  # i <- 184
  
  df %>% filter(site == i) -> temp
  
  temp <- full_join(temp, md) %>% as.data.table()
  
  firstd <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max() - days(28)
  lastd_data <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max()
  lastd <- temp %>% pull(roundtime) %>% max(., na.rm = T) + days(2)
  
  temp %>% filter(roundtime >= firstd) -> move_data
  
  max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
  
  moves <- 0:max_move
  
  cors <- rep(NA, length(moves))
  names(cors) <- moves
  
  for(mi in moves){
    #mi <- 1
    move_data %>% filter(!is.na(at)) %>% 
      select(roundtime, at, arh) %>% 
      mutate(roundtime = roundtime + hours(2*mi)) %>% 
      full_join(move_data %>% select(roundtime, md, md_arh)) %>% 
      arrange(roundtime) %>% 
      filter(complete.cases(.)) -> temp2
    
    cor1 <- sqrt(mean((temp2$at - temp2$md)^2))
    cor2 <- sqrt(mean((temp2$arh - temp2$md_arh)^2))
    
    cors[[paste0(mi)]] <- cor1+cor2
  }
  
  move_n <- as.numeric(names(which.min(cors)))
  
  if(move_n  != 0){
    
    print(i)
    
    df %>% filter(site == i) -> temp
    
    temp <- full_join(temp, md) %>% as.data.table()
    
    temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                        as.numeric(x[,"md"])),
                              by.column=FALSE, fill = NA)
    
    temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                            as.numeric(x[,"md_arh"])),
                                  by.column=FALSE, fill = NA)
    
    temp %>% 
      mutate(lead_probl = rollapply(hobo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
    
    temp %>% mutate(tdiff = abs(md-at)) %>% 
      mutate(corr24 = ifelse(corr24<0 | hobo_probl != 0 | lead_probl != 0,0,corr24),
             corr48 = ifelse(corr48<0 | hobo_probl != 0 | lead_probl != 0,0,corr48),
             corr168 = ifelse(corr168<0 | hobo_probl != 0 | lead_probl != 0,0,corr168),
             corr24_arh = ifelse(corr24_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr24_arh),
             corr48_arh = ifelse(corr48_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr48_arh),
             corr168_arh = ifelse(corr168_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr168_arh)) -> temp
    
    temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
      mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
      mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) %>% 
      mutate(comb = ifelse(hobo_probl != 0, 0, comb)) %>% 
      mutate(comb = ifelse(lead_probl != 0, 0, comb)) -> temp
    
    firstd <- temp$roundtime[min(which(temp$comb >= 10))]
    if(is.na(firstd)){
      d %>% filter(site == i) -> temp
      
      temp <- full_join(temp, md) %>% as.data.table()
      
      temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                          as.numeric(x[,"md"])),
                                by.column=FALSE, fill = NA)
      
      temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                              as.numeric(x[,"md_arh"])),
                                    by.column=FALSE, fill = NA)
      
      temp %>% 
        mutate(lead_probl = rollapply(hobo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
      
      temp %>% mutate(tdiff = abs(md-at)) %>% 
        mutate(corr24 = ifelse(corr24<0,0,corr24),
               corr48 = ifelse(corr48<0,0,corr48),
               corr168 = ifelse(corr168<0,0,corr168),
               corr24_arh = ifelse(corr24_arh<0,0,corr24_arh),
               corr48_arh = ifelse(corr48_arh<0,0,corr48_arh),
               corr168_arh = ifelse(corr168_arh<0,0,corr168_arh)) -> temp
      
      temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
        mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
        mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) -> temp
      
      firstd <- temp$roundtime[min(which(temp$comb >= 10))]
    }
    
    lastd_data <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max()
    lastd <- temp %>% pull(roundtime) %>% max() + days(2)
    
    temp %>% filter(roundtime >= firstd) -> move_data
    
    max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
    
    moves <- 0:max_move
    
    cors <- rep(NA, length(moves))
    names(cors) <- moves
    
    for(mi in moves){
      #mi <- 1
      move_data %>% filter(!is.na(at)) %>% 
        select(roundtime, at, arh) %>% 
        mutate(roundtime = roundtime + hours(2*mi)) %>% 
        full_join(move_data %>% select(roundtime, md, md_arh)) %>% 
        arrange(roundtime) %>% 
        filter(complete.cases(.)) -> temp2
      
      cor1 <- sqrt(mean((temp2$at - temp2$md)^2))
      cor2 <- sqrt(mean((temp2$arh - temp2$md_arh)^2))
      
      cors[[paste0(mi)]] <- cor1+cor2
    }
    
    move_n <- as.numeric(names(which.min(cors)))
    print(move_n)
    
    move_data %>% mutate(roundtime = roundtime + hours(2*move_n)) %>% 
      select(roundtime, at, arh, hobo_probl) %>% 
      bind_rows(temp %>% filter(roundtime < firstd) %>% select(roundtime, at, arh, hobo_probl)) %>% 
      full_join(md) %>% arrange(roundtime) %>% 
      filter(!is.na(md)) %>% 
      mutate(site = i) -> temp
    
  }
  
  df2 <- bind_rows(df2, temp %>% select(site, roundtime, at, arh, hobo_probl))
  
}

df2 %>% filter(!is.na(site)) -> df2

# ROUND 3

df3 <- data.frame()
for(i in unique(df2$site)){
  # i <- 77
  
  df2 %>% filter(site == i) -> temp
  
  temp <- full_join(temp, md) %>% as.data.table()
  
  firstd <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max() - days(28)
  lastd_data <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max()
  lastd <- temp %>% pull(roundtime) %>% max() + days(2)
  
  temp %>% filter(roundtime >= firstd) -> move_data
  
  max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
  
  moves <- 0:max_move
  
  cors <- rep(NA, length(moves))
  names(cors) <- moves
  
  for(mi in moves){
    #mi <- 1
    move_data %>% filter(!is.na(at)) %>% 
      select(roundtime, at, arh) %>% 
      mutate(roundtime = roundtime + hours(2*mi)) %>% 
      full_join(move_data %>% select(roundtime, md, md_arh)) %>% 
      arrange(roundtime) %>% 
      filter(complete.cases(.)) -> temp2
    
    cor1 <- sqrt(mean((temp2$at - temp2$md)^2))
    cor2 <- sqrt(mean((temp2$arh - temp2$md_arh)^2))
    
    cors[[paste0(mi)]] <- cor1+cor2
  }
  
  move_n <- as.numeric(names(which.min(cors)))
  
  if(move_n  != 0){
    
    print(i)
    
    df2 %>% filter(site == i) -> temp
    
    temp <- full_join(temp, md) %>% as.data.table()
    
    temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                        as.numeric(x[,"md"])),
                              by.column=FALSE, fill = NA)
    
    temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                            as.numeric(x[,"md_arh"])),
                                  by.column=FALSE, fill = NA)
    
    temp %>% 
      mutate(lead_probl = rollapply(hobo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
    
    temp %>% mutate(tdiff = abs(md-at)) %>% 
      mutate(corr24 = ifelse(corr24<0 | hobo_probl != 0 | lead_probl != 0,0,corr24),
             corr48 = ifelse(corr48<0 | hobo_probl != 0 | lead_probl != 0,0,corr48),
             corr168 = ifelse(corr168<0 | hobo_probl != 0 | lead_probl != 0,0,corr168),
             corr24_arh = ifelse(corr24_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr24_arh),
             corr48_arh = ifelse(corr48_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr48_arh),
             corr168_arh = ifelse(corr168_arh<0 | hobo_probl != 0 | lead_probl != 0,0,corr168_arh)) -> temp
    
    temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
      mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
      mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) %>% 
      mutate(comb = ifelse(hobo_probl != 0, 0, comb)) %>% 
      mutate(comb = ifelse(lead_probl != 0, 0, comb)) -> temp
    
    firstd <- temp$roundtime[min(which(temp$comb >= 10))]
    if(is.na(firstd)){
      d %>% filter(site == i) -> temp
      
      temp <- full_join(temp, md) %>% as.data.table()
      
      temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                          as.numeric(x[,"md"])),
                                by.column=FALSE, fill = NA)
      
      temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                              as.numeric(x[,"md_arh"])),
                                    by.column=FALSE, fill = NA)
      
      temp %>% 
        mutate(lead_probl = rollapply(hobo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
      
      temp %>% mutate(tdiff = abs(md-at)) %>% 
        mutate(corr24 = ifelse(corr24<0,0,corr24),
               corr48 = ifelse(corr48<0,0,corr48),
               corr168 = ifelse(corr168<0,0,corr168),
               corr24_arh = ifelse(corr24_arh<0,0,corr24_arh),
               corr48_arh = ifelse(corr48_arh<0,0,corr48_arh),
               corr168_arh = ifelse(corr168_arh<0,0,corr168_arh)) -> temp
      
      temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
        mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
        mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) -> temp
      
      firstd <- temp$roundtime[min(which(temp$comb >= 10))]
    }
    
    lastd_data <- temp %>% filter(!is.na(at)) %>% pull(roundtime) %>% max()
    lastd <- temp %>% pull(roundtime) %>% max() + days(2)
    
    temp %>% filter(roundtime >= firstd) -> move_data
    
    max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
    
    moves <- 0:max_move
    
    cors <- rep(NA, length(moves))
    names(cors) <- moves
    
    for(mi in moves){
      #mi <- 1
      move_data %>% filter(!is.na(at)) %>% 
        select(roundtime, at, arh) %>% 
        mutate(roundtime = roundtime + hours(2*mi)) %>% 
        full_join(move_data %>% select(roundtime, md, md_arh)) %>% 
        arrange(roundtime) %>% 
        filter(complete.cases(.)) -> temp2
      
      cor1 <- sqrt(mean((temp2$at - temp2$md)^2))
      cor2 <- sqrt(mean((temp2$arh - temp2$md_arh)^2))
      
      cors[[paste0(mi)]] <- cor1+cor2
    }
    
    move_n <- as.numeric(names(which.min(cors)))
    print(move_n)
    
    move_data %>% mutate(roundtime = roundtime + hours(2*move_n)) %>% 
      select(roundtime, at, arh, hobo_probl) %>% 
      bind_rows(temp %>% filter(roundtime < firstd) %>% select(roundtime, at, arh, hobo_probl)) %>% 
      full_join(md) %>% arrange(roundtime) %>% 
      filter(!is.na(md)) %>% 
      mutate(site = i) -> temp
    
  }
  
  df3 <- bind_rows(df3, temp %>% select(site, roundtime, at, arh, hobo_probl))
  
}

df3 %>% filter(!is.na(site)) -> df3

# TRIM THE GAP MARGINS
# BACKWARD

df4 <- data.frame()
for(i in unique(df3$site)){
  # i <- "MAL012"
  
  print(i)
  print(Sys.time())
  
  df3 %>% filter(site == i) %>% 
    #select(-md, -md_arh) %>% 
    filter(complete.cases(.)) %>% 
    mutate(timediff = roundtime - lag(roundtime)) %>% 
    full_join(., md) %>% 
    arrange(roundtime) %>% as.data.table() -> temp
  
  # temp %>% filter(!is.na(at))
  # temp[1,"timediff"] <- 30
  
  splits <- which(temp$timediff > 30)
  
  if(length(splits) > 0){
    for(ii in splits){
      
      temp %>% slice(1:(ii-1)) %>% pull(at) %>% rev() -> ats
      
      tibble(grps = rleid(is.na(ats))) %>% group_by(grps) %>% count() %>% pull(n) -> part_lengths
      
      gap_length <- part_lengths[1]
      part_length <- part_lengths[2]
      
      # gap_length <- which.min(is.na(ats))-1
      
      if(gap_length >= 191 & part_length >= 671){
        
        test_df <- expand.grid(data_l = c(seq(671, ifelse(part_length < 2687, part_length, 2687), by = 4), 
                                          ifelse(part_length < 2687, part_length, 2687)),
                               move_l = seq(1, gap_length, by = 2))
        
        test_df <- bind_rows(data.frame(data_l = ifelse(part_length < 2687, part_length, 2687),
                                        move_l = 0),
                             test_df)
        
        temp %>% slice(ii:(ii+ifelse(part_length < 2687, part_length, 2687))) %>% pull(at) -> att
        
        maxl <- max(test_df$data_l)
        
        splitting_cors <- function(iii){
          if(test_df[iii,"data_l"] == maxl){
            temp %>% slice((ii-test_df[iii,"move_l"]):(ii-test_df[iii,"move_l"]+test_df[iii,"data_l"])) %>%
              pull(md) -> mdt
          } else {
            temp %>% slice(c((ii-test_df[iii,"move_l"]):(ii-test_df[iii,"move_l"]+test_df[iii,"data_l"]),
                             (ii+test_df[iii,"data_l"]+1):(ii+ifelse(part_length < 2687, part_length, 2687)))) %>%
              pull(md) -> mdt
          }
          
          return((1-cor(mdt, att, use = "pairwise.complete.obs"))*mean(abs(mdt-att), na.rm = T))
          
        }
        
        test_df$fac <- unlist(lapply(1:nrow(test_df), splitting_cors))
        
        test_df %>% pull(fac) %>% which.min() -> result
        
        temp %>% pull(at) -> ats
        move <- ats[ii:(ii+test_df[result,"data_l"])]
        ats[ii:(ii+test_df[result,"data_l"])] <- NA
        ats[(ii-test_df[result,"move_l"]):(ii-test_df[result,"move_l"]+test_df[result,"data_l"])] <- move
        temp$at <- ats
        
        temp %>% pull(arh) -> ats
        move <- ats[ii:(ii+test_df[result,"data_l"])]
        ats[ii:(ii+test_df[result,"data_l"])] <- NA
        ats[(ii-test_df[result,"move_l"]):(ii-test_df[result,"move_l"]+test_df[result,"data_l"])] <- move
        temp$arh <- ats
      }
    }
  }
  
  temp$site <- i
  
  df4 <- bind_rows(df4, temp %>% select(site, roundtime, at, arh, hobo_probl))
  
}

df4 %>% filter(!is.na(site)) -> df4

# FORWARD

df5 <- data.frame()
for(i in unique(df4$site)){
  # i <- "MAL012"
  
  print(i)
  print(Sys.time())
  
  df4 %>% filter(site == i) %>% 
    #select(-md, -md_arh) %>% 
    filter(complete.cases(.)) %>% 
    mutate(timediff = lead(roundtime) - roundtime) %>% 
    full_join(., md) %>% 
    arrange(roundtime) %>% as.data.table() -> temp
  
  splits <- which(temp$timediff > 30)
  
  if(length(splits) > 0){
    for(ii in splits){
      # ii <- 232
      temp %>% slice((ii+1):nrow(.)) %>% pull(at) -> ats_after
      temp %>% slice(1:ii) %>% pull(at) %>% rev() -> ats_before
      
      tibble(grps = rleid(is.na(ats_after))) %>% group_by(grps) %>% count() %>% pull(n) -> gap_lengths
      tibble(grps = rleid(is.na(ats_before))) %>% group_by(grps) %>% count() %>% pull(n) -> part_lengths
      
      gap_length <- gap_lengths[1]
      part_length <- part_lengths[1]
      
      if(gap_length >= 95 & part_length >= 671){
        
        maxdata_l <- ifelse(part_length < 2687, part_length, 2687)
        
        test_df <- expand.grid(data_l = c(seq(671, ifelse(part_length < 2687, part_length, 2687), by = 3), 
                                          ifelse(part_length < 2687, part_length, 2687)),
                               move_l = seq(1, gap_length, by = 2))
        
        test_df <- bind_rows(data.frame(data_l = ifelse(part_length < 2687, part_length, 2687),
                                        move_l = 0),
                             test_df)
        
        temp %>% slice((ii-maxdata_l):ii) %>% pull(at) -> att
        
        maxl <- max(test_df$data_l)
        
        splitting_cors <- function(iii){
          if(test_df[iii,"data_l"] == maxl){
            temp %>% slice((ii-maxl+test_df[iii,"move_l"]):(ii+test_df[iii,"move_l"])) %>%
              pull(md) -> mdt
          } else {
            temp %>% slice(c((ii-maxl):(ii-test_df[iii,"data_l"]),
                             (ii-test_df[iii,"data_l"]+1+test_df[iii,"move_l"]):(ii+test_df[iii,"move_l"]))) %>%
              pull(md) -> mdt
          }
          
          return((1-cor(mdt, att, use = "pairwise.complete.obs"))*mean(abs(mdt-att), na.rm = T))
          
        }
        
        test_df$fac <- unlist(lapply(1:nrow(test_df), splitting_cors))
        
        test_df %>% pull(fac) %>% which.min() -> result
        
        temp %>% pull(at) -> ats
        move <- ats[(ii-test_df[result,"data_l"]+1):ii]
        ats[(ii-test_df[result,"data_l"]+1):ii] <- NA
        ats[(ii-test_df[result,"data_l"]+1+test_df[result,"move_l"]):(ii+test_df[result,"move_l"])] <- move
        temp$at <- ats
        
        temp %>% pull(arh) -> ats
        move <- ats[(ii-test_df[result,"data_l"]+1):ii]
        ats[(ii-test_df[result,"data_l"]+1):ii] <- NA
        ats[(ii-test_df[result,"data_l"]+1+test_df[result,"move_l"]):(ii+test_df[result,"move_l"])] <- move
        temp$arh <- ats
        
      }
    }
  }
  
  temp$site <- i
  
  df5 <- bind_rows(df5, temp %>% select(site, roundtime, at, arh, hobo_probl))
  
}

df5 %>% filter(!is.na(site)) -> df5

# TRIM THE GAP MARGINS
# BACKWARD

df6 <- data.frame()
for(i in unique(df5$site)){
  # i <- "MAL012"
  
  print(i)
  print(Sys.time())
  
  df5 %>% filter(site == i) %>% 
    #select(-md, -md_arh) %>% 
    filter(complete.cases(.)) %>% 
    mutate(timediff = roundtime - lag(roundtime)) %>% 
    full_join(., md) %>% 
    arrange(roundtime) %>% as.data.table() -> temp
  
  # temp %>% filter(!is.na(at))
  # temp[1,"timediff"] <- 30
  
  splits <- which(temp$timediff > 30)
  
  if(length(splits) > 0){
    for(ii in splits){
      
      temp %>% slice(1:(ii-1)) %>% pull(at) %>% rev() -> ats
      
      tibble(grps = rleid(is.na(ats))) %>% group_by(grps) %>% count() %>% pull(n) -> part_lengths
      
      gap_length <- part_lengths[1]
      part_length <- part_lengths[2]
      
      # gap_length <- which.min(is.na(ats))-1
      
      if(gap_length >= 191 & part_length >= 191){
        
        test_df <- expand.grid(data_l = c(seq(191, ifelse(part_length < 1343, part_length, 1343), by = 3), 
                                          ifelse(part_length < 1343, part_length, 1343)),
                               move_l = seq(1, gap_length, by = 2))
        
        test_df <- bind_rows(data.frame(data_l = ifelse(part_length < 1343, part_length, 1343),
                                        move_l = 0),
                             test_df)
        
        temp %>% slice(ii:(ii+ifelse(part_length < 1343, part_length, 1343))) %>% pull(at) -> att
        
        maxl <- max(test_df$data_l)
        
        splitting_cors <- function(iii){
          if(test_df[iii,"data_l"] == maxl){
            temp %>% slice((ii-test_df[iii,"move_l"]):(ii-test_df[iii,"move_l"]+test_df[iii,"data_l"])) %>%
              pull(md) -> mdt
          } else {
            temp %>% slice(c((ii-test_df[iii,"move_l"]):(ii-test_df[iii,"move_l"]+test_df[iii,"data_l"]),
                             (ii+test_df[iii,"data_l"]+1):(ii+ifelse(part_length < 1343, part_length, 1343)))) %>%
              pull(md) -> mdt
          }
          
          return((1-cor(mdt, att, use = "pairwise.complete.obs"))*mean(abs(mdt-att), na.rm = T))
          
        }
        
        test_df$fac <- unlist(lapply(1:nrow(test_df), splitting_cors))
        
        test_df %>% pull(fac) %>% which.min() -> result
        
        temp %>% pull(at) -> ats
        move <- ats[ii:(ii+test_df[result,"data_l"])]
        ats[ii:(ii+test_df[result,"data_l"])] <- NA
        ats[(ii-test_df[result,"move_l"]):(ii-test_df[result,"move_l"]+test_df[result,"data_l"])] <- move
        temp$at <- ats
        
        temp %>% pull(arh) -> ats
        move <- ats[ii:(ii+test_df[result,"data_l"])]
        ats[ii:(ii+test_df[result,"data_l"])] <- NA
        ats[(ii-test_df[result,"move_l"]):(ii-test_df[result,"move_l"]+test_df[result,"data_l"])] <- move
        temp$arh <- ats
      }
    }
  }
  
  temp$site <- i
  
  df6 <- bind_rows(df6, temp %>% select(site, roundtime, at, arh, hobo_probl))
  
}

df6 %>% filter(!is.na(site)) -> df6

# FORWARD

df7 <- data.frame()
for(i in unique(df6$site)){
  # i <- "MAL012"
  
  print(i)
  print(Sys.time())
  
  df6 %>% filter(site == i) %>% 
    #select(-md, -md_arh) %>% 
    filter(complete.cases(.)) %>% 
    mutate(timediff = lead(roundtime) - roundtime) %>% 
    full_join(., md) %>% 
    arrange(roundtime) %>% as.data.table() -> temp
  
  splits <- which(temp$timediff > 30)
  
  if(length(splits) > 0){
    for(ii in splits){
      # ii <- 232
      temp %>% slice((ii+1):nrow(.)) %>% pull(at) -> ats_after
      temp %>% slice(1:ii) %>% pull(at) %>% rev() -> ats_before
      
      tibble(grps = rleid(is.na(ats_after))) %>% group_by(grps) %>% count() %>% pull(n) -> gap_lengths
      tibble(grps = rleid(is.na(ats_before))) %>% group_by(grps) %>% count() %>% pull(n) -> part_lengths
      
      gap_length <- gap_lengths[1]
      part_length <- part_lengths[1]
      
      if(gap_length >= 95 & part_length >= 191){
        
        maxdata_l <- ifelse(part_length < 1343, part_length, 1343)
        
        test_df <- expand.grid(data_l = c(seq(191, ifelse(part_length < 1343, part_length, 1343), by = 3), 
                                          ifelse(part_length < 1343, part_length, 1343)),
                               move_l = seq(1, gap_length, by = 2))
        
        test_df <- bind_rows(data.frame(data_l = ifelse(part_length < 1343, part_length, 1343),
                                        move_l = 0),
                             test_df)
        
        temp %>% slice((ii-maxdata_l):ii) %>% pull(at) -> att
        
        maxl <- max(test_df$data_l)
        
        splitting_cors <- function(iii){
          if(test_df[iii,"data_l"] == maxl){
            temp %>% slice((ii-maxl+test_df[iii,"move_l"]):(ii+test_df[iii,"move_l"])) %>%
              pull(md) -> mdt
          } else {
            temp %>% slice(c((ii-maxl):(ii-test_df[iii,"data_l"]),
                             (ii-test_df[iii,"data_l"]+1+test_df[iii,"move_l"]):(ii+test_df[iii,"move_l"]))) %>%
              pull(md) -> mdt
          }
          
          return((1-cor(mdt, att, use = "pairwise.complete.obs"))*mean(abs(mdt-att), na.rm = T))
          
        }
        
        test_df$fac <- unlist(lapply(1:nrow(test_df), splitting_cors))
        
        test_df %>% pull(fac) %>% which.min() -> result
        
        temp %>% pull(at) -> ats
        move <- ats[(ii-test_df[result,"data_l"]+1):ii]
        ats[(ii-test_df[result,"data_l"]+1):ii] <- NA
        ats[(ii-test_df[result,"data_l"]+1+test_df[result,"move_l"]):(ii+test_df[result,"move_l"])] <- move
        temp$at <- ats
        
        temp %>% pull(arh) -> ats
        move <- ats[(ii-test_df[result,"data_l"]+1):ii]
        ats[(ii-test_df[result,"data_l"]+1):ii] <- NA
        ats[(ii-test_df[result,"data_l"]+1+test_df[result,"move_l"]):(ii+test_df[result,"move_l"])] <- move
        temp$arh <- ats
        
      }
    }
  }
  
  temp$site <- i
  
  df7 <- bind_rows(df7, temp %>% select(site, roundtime, at, arh, hobo_probl))
  
}

df7 %>% filter(!is.na(site)) -> df7

# TRIM WITH THE LAST DAY ON FIELD

# Remove measurements prior installing
full_join(df7, maxdt20 %>% select(-tomst_id)) %>% 
  filter(roundtime >= maxdt) %>%
  select(-maxdt) -> df7

# Remove measurements after 2021 visit
full_join(df7, maxdt21 %>% select(site, maxdt)) %>% 
  filter(roundtime < maxdt) %>%
  select(-maxdt) -> df7

# PLOT

pdf("visuals/Haxo_corrected.pdf", 12, 8)
for(i in unique(df7$site)){
  # i <- 53
  print(i)
  
  df7 %>% filter(site == i) -> temp
  
  temp <- left_join(temp, md)
  
  temp %>% ggplot(aes_string(x="roundtime")) +
    geom_line(aes_string(y = "md"), col = "black", linetype = "dashed") +
    geom_line(aes_string(y = "at"), col = "cornflowerblue") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-20, 35))+
    ggtitle(i) -> GG1
  
  print(GG1)
  
}
dev.off()

fwrite(df7, "output/haxo_data_corrected.csv")


