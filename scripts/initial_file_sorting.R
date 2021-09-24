##########################################################################
# THIS CODE MAKES SOME QUALITY CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#

library(tidyverse)

# List binary and command files to be removed from repository if also data file exists

f <- c(list.files("data", pattern = "binary_", recursive = T, full.names = T),
       list.files("data", pattern = "command_", recursive = T, full.names = T))

for(i in f){ if(file.exists(gsub("binary_","data_",i)) | file.exists(gsub("command_","data_",i))){
  unlink(i)
} else {
  print(paste0("DATA FILE MISSING!!! ", i))
} 
}
# If no printed messages then no problems

###########################################################################
# Change names of the saana/mikkuna directories

dirs <- list.dirs("data/tomst", recursive = F)

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

for(i in dirs){
  id <- as.numeric(strsplit(i, "/")[[1]][3])
  if(!is.na(id)){
    file.rename(from = paste0(getwd(),"/",i),
                to = paste0(getwd(),"/",gsub(id, paste0("SAA", add_zeros(id)), i)))
  }
}

###########################################################################
# Check Tomst ID-numbers from last year data
maxdt <- bind_rows(read_csv("data/reading_times_2020_AIL.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("AIL", add_zeros(x))))),
                   read_csv("data/reading_times_2020_MAL.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("MAL", add_zeros(x))))),
                   read_csv("data/reading_times_2020_SAA.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("SAA", add_zeros(x))))))

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- toupper(unlist(lapply(fi$file, function(x) strsplit(x, "/")[[1]][3])))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",strsplit(x, "/")[[1]][4]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none
# Sites SAA1105 and SAA343 has two files with different tomst ids
maxdt %>% filter(tomst_id == 94181319)
maxdt %>% filter(tomst_id == 94181620)
# Based on 2020 data tomst id 94181620 is real site SAA1105
# And 94181319 belongs to site SAA1057
fi %>% filter(site == "SAA1057") # And there is no 2021 data for site SAA1057
# Thus I copy the 94181319 data to SAA1057 folder
file.copy(paste0(getwd(), "/data/tomst/SAA1105/data_94181319_0.csv"),
          paste0(getwd(), "/data/tomst/SAA1057/data_94181319_0.csv"))
unlink("data/tomst/SAA1105/data_94181319_0.csv")

maxdt %>% filter(tomst_id == 94181308)
maxdt %>% filter(tomst_id == 94181311)
# Based on 2020 data tomst id 94181308 is real site SAA343
# And 94181311 belongs to site SAA103
fi %>% filter(site == "SAA103") # And there is no 2021 data for site SAA103
# Thus I copy the 94181311 data to SAA103 folder
file.copy(paste0(getwd(), "/data/tomst/SAA343/data_94181311_0.csv"),
          paste0(getwd(), "/data/tomst/SAA103/data_94181311_0.csv"))
unlink("data/tomst/SAA343/data_94181311_0.csv")

###########################################################################
# Update the file list

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- toupper(unlist(lapply(fi$file, function(x) strsplit(x, "/")[[1]][3])))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",strsplit(x, "/")[[1]][4]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none
# Looks good now!

#######################################################################
# Check if missing sites in 2021 data
all <- full_join(fi, maxdt %>% rename(tomst_id_20 = tomst_id))

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # No, Good!

# Non-matching sites, Exclude RA sites which were started in 2020
all %>% filter(!complete.cases(.)) %>% filter(!grepl("RA",site))

# site AIL107 in 2021 but not in 2020
all %>% filter(tomst_id_20 == 94194047) # No such Tomst ids in 2020 so this is fine!
# site AIL117 in 2021 but not in 2020
all %>% filter(tomst_id_20 == 94194045) # No such Tomst ids in 2020 so this is fine!

#sites AIL105, AIL108, MAL042, MAL092 and SAA1195 in 2020 data but not in 2021
all %>% filter(tomst_id == 94194176) # No such Tomst ids in 2021 so this is fine!
all %>% filter(tomst_id == 94194028) # No such Tomst ids in 2021 so this is fine!
all %>% filter(tomst_id == 94194142) # No such Tomst ids in 2021 so this is fine!
all %>% filter(tomst_id == 94194157) # No such Tomst ids in 2021 so this is fine!
all %>% filter(tomst_id == 94181612) # No such Tomst ids in 2021 so this is fine!

# For sites AIL105, AIL108, MAL042, MAL092 and SAA1195 find 2020 data and copy to repository
f2 <- list.files("C:/Users/OMISTAJA/OneDrive - University of Helsinki/R_Projects/microclim_suomi/raw_field_data",
                 pattern = "data_", recursive = T, full.names = T)

# Copy site AIL105 data from last year data
f2[grepl("/ailakka/tomst/105", f2)]
dir.create(paste0(getwd(), "/data/tomst/ail105"))
file.copy(f2[grepl("/ailakka/tomst/105", f2)],
          paste0(getwd(), "/data/tomst/ail105/data_94194176_0.csv"))

# Copy site AIL108 data from last year data
f2[grepl("/ailakka/tomst/108", f2)]
dir.create(paste0(getwd(), "/data/tomst/ail108"))
file.copy(f2[grepl("/ailakka/tomst/108", f2)],
          paste0(getwd(), "/data/tomst/ail108/data_94194028_0.csv"))

# Copy site MAL042 data from last year data
f2[grepl("/malla/tomst/042", f2)]
dir.create(paste0(getwd(), "/data/tomst/mal042"))
file.copy(f2[grepl("/malla/tomst/042", f2)],
          paste0(getwd(), "/data/tomst/mal042/data_94194142_0.csv"))

# Copy site MAL092 data from last year data
f2[grepl("/malla/tomst/092", f2)]
file.copy(f2[grepl("/malla/tomst/092", f2)],
          paste0(getwd(), "/data/tomst/mal092/data_94194157_0.csv"))

# Copy site SAA1195 data from last year data
f2[grepl("94181612", f2)]
file.copy(f2[grepl("94181612", f2)],
          paste0(getwd(), "/data/tomst/SAA1195/data_94181612_1.csv"))


########################################################################################
# Update file list

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- toupper(unlist(lapply(fi$file, function(x) strsplit(x, "/")[[1]][3])))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",strsplit(x, "/")[[1]][4]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none
# Looks good still!!!

#######################################################################
# Check if Tomst ids match between years
all <- full_join(fi, maxdt %>% rename(tomst_id_20 = tomst_id))

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # No, Good!

all %>% filter(tomst_id == tomst_id_20)
all %>% filter(tomst_id != tomst_id_20)
# All seems to match nicely!!!!!!!!!!


# Good to go and read the data


