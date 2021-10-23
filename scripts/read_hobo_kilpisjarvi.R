library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)

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

sites <- unique(df$site)

df %>% mutate(roundtime = round_date(datetime, "30 minutes")) -> df

df %>% group_by(roundtime) %>% 
  summarise(at_med = median(at, na.rm = T),
            arh_med = median(arh, na.rm = T)) -> meds


df <- full_join(df, meds)

pdf("visuals/Hobo_graphs.pdf", 17, 10)
for(i in sites){
  #i <- sites[3]
  print(i)
  
  df %>% filter(site == i) %>%
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "at_med"), col = "black", linetype = "dashed") +
    geom_line(aes_string(y = "at"), col = "cornflowerblue") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-30, 35))+
    ggtitle(i) -> GG1
  
  df %>% filter(site == i) %>%
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "arh_med"), col = "black", linetype = "dashed") +
    geom_line(aes_string(y = "arh"), col = "cornflowerblue") +
    theme_minimal() +
    ylab("Air humidity") + xlab("Date")+
    scale_y_continuous(limits = c(0, 100)) -> GG2
  
  
  print(plot_grid(plotlist = list(GG1,GG2), nrow = 2))
  
}
dev.off()


