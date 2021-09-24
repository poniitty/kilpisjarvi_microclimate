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
maxdt <- read_csv("data/reading_times_2020.csv")

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- unlist(lapply(fi$file, function(x) as.numeric(gsub("RL","",strsplit(x, "/")[[1]][3]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",strsplit(x, "/")[[1]][4]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# 94194288 occurs in two folders
maxdt %>% filter(tomst_id == 94194288)
# Based on year 2020 data the site 104 is correct
# Thus I erase the file in 77 folder
unlink("data/tomst/RL77/data_94194288_0.csv")
fi <- fi %>% filter(file != "data/tomst/RL77/data_94194288_0.csv")
fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none
# No more duplicates

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none
# Site 15 has two files with different tomst ids
maxdt %>% filter(tomst_id == 94184843)
maxdt %>% filter(tomst_id == 94194307)
# Based on 2020 data tomst id 94194307 is real site 15
# And 94184843 belongs to site 71
fi %>% filter(site == 71) # And there is no 2021 data for site 71
# Thus I copy the 94184843 data to 71 folder
file.copy(paste0(getwd(), "/data/tomst/RL15/data_94184843_0.csv"),
          paste0(getwd(), "/data/tomst/RL71/data_94184843_0.csv"))
unlink("data/tomst/RL15/data_94184843_0.csv")

###########################################################################
# Update the file list

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- unlist(lapply(fi$file, function(x) as.numeric(gsub("RL","",strsplit(x, "/")[[1]][3]))))

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

# Non-matching sites
all %>% filter(!complete.cases(.))

# site 12 in 2021 but not in 2020
all %>% filter(tomst_id_20 == 94184856) # No such Tomst ids in 2020 so this is fine!

#sites 68 and 110 in 2020 data but not in 2021
all %>% filter(tomst_id == 94194306) # No such Tomst ids in 2021 so this is fine!
all %>% filter(tomst_id == 94184848) # No such Tomst ids in 2021 so this is fine!

# For sites 68 and 110 find 2020 data and copy to repository
f2 <- list.files("C:/Users/OMISTAJA/OneDrive - University of Helsinki/R_Projects/microclim_suomi/raw_field_data/rastigaisa",
                 pattern = "data_", recursive = T, full.names = T)

# Copy site 110 data from last year data
f2[grepl("/RL110_20/", f2)]
file.copy(f2[grepl("/RL110_20/", f2)],
          paste0(getwd(), "/data/tomst/RL110/data_94184848_0.csv"))

# Copy site 68 data from last year data
f2[grepl("/RL68_20/", f2)]
dir.create(paste0(getwd(), "/data/tomst/RL68"))
file.copy(f2[grepl("/RL68_20/", f2)],
          paste0(getwd(), "/data/tomst/RL68/data_94194306_0.csv"))


########################################################################################
# Update file list

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- unlist(lapply(fi$file, function(x) as.numeric(gsub("RL","",strsplit(x, "/")[[1]][3]))))

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


