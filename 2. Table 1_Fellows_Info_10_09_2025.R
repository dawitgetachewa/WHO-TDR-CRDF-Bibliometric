################################################################################
# Title :	Summarising biblio database
################################################################################
#install.packages("sjr_journals")
rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse,ggpubr,purrr,readr,stringr,patchwork,table1,doBy,
               gtools,janitor,ggbreak, RColorBrewer,wesanderson,viridis,forcats,tibble,tidyr,
               dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,classInt, 
               car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools,textclean,waffle,ggwaffle,cowplot
)
library(worldbank)
library(sjrdata)

#=============================
# Working directory
#=============================
setwd("C:/IDDO_TDR/Mahanaz/bilblio/Jul data from Dawit")

# Read the fellows data (n=128)
all_fellows <-read_excel("Desktop/Mahnaz project/final/Clinical Research and Development Program Publications_May 2025_v2_DGA_11_07_2025.xlsx", 
                         sheet = "data_mahnaz_pd") %>%
  clean_names()

# for analysis only (for subset of fellows with publications available)
dat_fellows <-read_excel("Desktop/Mahnaz project/final/Clinical Research and Development Program Publications_May 2025_v2_DGA_11_07_2025.xlsx", 
                         sheet = "data_mahnaz_pd") %>%
  clean_names()
# Trim whitespace from the 'Name' column
dat_fellows <- dat_fellows %>%
  mutate(
    home_country = str_trim(home_country),
    country_name = str_trim(country_name)
  )

dat_fellows <- dat_fellows %>%
  mutate(
    WHO_Region = case_when(
      home_country  %in% c("Benin","Botswana","Burkina Faso","Cameroon","Congo","Eswatini",
                           "Ethiopia","Gabon","Ghana","Guinea","Kenya","Liberia","Madagascar","Malawi","Mali",
                           "Mozambique","Nigeria","RDC","Rwanda","Senegal","Sierra Leone","South Africa","Sudan",
                           "Tanzania","The Gambia","Uganda","Zambia","Zimbabwe")                               ~ "African Region",
      
      home_country =="Tunisia"                                                            ~ "Eastern Mediterranean Region",
      home_country %in% c("Argentina","Brazil","Colombia","Peru")                         ~  "Region of the Americas",
      home_country %in% c("Bangladesh","India","Nepal")                                   ~  "South-East Asian Region",
      home_country %in% c("China","Indonesia","Vietnam")                                  ~  "Western Pacific Region"
    )
  )

dat_fellows <- dat_fellows %>%
  mutate(
    WHO_Region_Code = case_when(
      home_country  %in% c("Benin","Botswana","Burkina Faso","Cameroon","Congo","Eswatini",
                           "Ethiopia","Gabon","Ghana","Guinea","Kenya","Liberia","Madagascar","Malawi","Mali",
                           "Mozambique","Nigeria","RDC","Rwanda","Senegal","Sierra Leone","South Africa","Sudan",
                           "Tanzania","The Gambia","Uganda","Zambia","Zimbabwe")                               ~ "AFR",
      
      home_country =="Tunisia"                                                            ~ "EMR",
      home_country %in% c("Argentina","Brazil","Colombia","Peru")                         ~  "AMR",
      home_country %in% c("Bangladesh","India","Nepal")                                   ~  "SEAR",
      home_country %in% c("China","Indonesia","Vietnam")                                  ~  "WPR"
    )
  )

# Income status from R package
wb_dat <- wb_country() %>% 
  select(country_name, income_level_id) %>% 
  filter(country_name %in% dat_fellows$country_name)

# Correct the INX=not indexed code to LMC for Ethiopia
wb_dat$income_level_id[wb_dat$country_name=="Ethiopia"]<-"LMC"

dat_fellows <- dat_fellows %>% 
  left_join(wb_dat, by="country_name") %>% 
  dplyr:: rename(who_fellow = name) 

dat_fellows <- dat_fellows %>% 
  select(-c(n_first:n_total)) # these are old dataset

# proportion by income level
library(janitor)
library(dplyr)

dat_fellows <- dat_fellows %>% clean_names()

# Now the column is probably called income_level_id

fellows_by_income <- dat_fellows %>% 
    dplyr::group_by(income_level_id) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%   # drop grouping after summarise
    dplyr::mutate(
        total = sum(count),
        prop = (count / total) * 100
    )
fellows_by_income 

#-----------------------------------
# Read the publications database
#-----------------------------------
analysis_dat <- read_excel("Desktop/Mahnaz project/final/tdr_biblio_analysis_dataset_10_09_2025.xlsx")%>%
    clean_names
# number of fellows with a publication
length(table(analysis_dat$who_fellow))

# pub by fellows
dat_pub <- analysis_dat %>%
  dplyr::group_by(who_fellow,who_region, author_position) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  pivot_wider(
    names_from = author_position,
    values_from = count,
    names_prefix = "n_",
    values_fill = 0  # Fill missing values with 0
  ) 

dat_pub <- dat_pub %>%  
 mutate(
    n_total = n_first + n_mid + n_last
  )

#---------------------------------------------------
# Merge the n_publications with fellows information
#---------------------------------------------------
# Keep only the fellows who were in the publication database
dat_fellows <- dat_fellows[which(dat_fellows$who_fellow %in% analysis_dat$who_fellow),]


# number of fellow grouped by year of placement

all_fellows <-all_fellows %>%
    mutate(year_group = cut(
        placement_year,
        breaks = c(2008, 2013, 2018, 2021),  # define intervals
        labels = c("2009-2013", "2014-2018", "2019-2021"),
        right = TRUE
    ))

# Check the counts per year group
table(all_fellows$year_group)


dat_fellows <- dat_fellows %>%
    mutate(who_region = WHO_Region) %>%
    select(-WHO_Region)

dat_fellows <- dat_fellows %>% 
  left_join(dat_pub, by=c("who_fellow", "who_region"))

# mean age at fellowship
summary(dat_fellows$age_at_fellowship)

# sex distribution
table(dat_fellows$gender, useNA="ifany")

n_fellows_by_region <- dat_fellows %>% 
  dplyr::group_by(who_region) %>%
  dplyr::summarise(count = n(), .groups = "drop_last") %>%
  dplyr::mutate(
    total = sum(count),
    prop = (count / total) * 100
  )

n_fellows_by_region

n_fellows_by_gender <- dat_fellows %>% 
  dplyr::group_by(gender) %>%
  dplyr::summarise(count = n(), .groups = "drop_last") %>%
  dplyr::mutate(
    total = sum(count),
    prop = (count / total) * 100
  )

n_fellows_by_gender

# Summary of the participants (n=76 fellows)

dat_fellows_region <-dat_fellows %>%
    group_by(who_region)
       
table(dat_fellows_region$country_name)

table(dat_fellows_region$gender, dat_fellows_region$who_region)
table(dat_fellows_region$tpo_region, dat_fellows_region$who_region)

#-----------------------------------
# n_pub before and after
#-----------------------------------
analysis_dat %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    count = n(),  .groups = "drop"
  ) 

before_after_region <- analysis_dat %>%
   dplyr::group_by(who_region) %>%
   dplyr::summarise(
     count = n(),  .groups = "drop"
   ) 

before_after_region_time <- analysis_dat %>%
  dplyr::group_by(who_region,time) %>%
  dplyr::summarise(
    count = n(),  .groups = "drop"
  ) 

# Authorship positions
auth_positon <- analysis_dat %>%
   dplyr::group_by(author_position) %>%
   dplyr::summarise(
     count = n(),  .groups = "drop"
   ) 

# Authorship positions by WHO region
auth_positon_region<- analysis_dat %>%
   dplyr::group_by(who_region,author_position) %>%
   dplyr::group_by(author_position) %>%
   dplyr::summarise(
     count = n(),  .groups = "drop"
   ) 

#----------------------------
# Presence of ORCID ID
#----------------------------
dat_fellows <- dat_fellows %>%
  mutate(orcid_presence= case_when(
    !(is.na(orcid_id))    ~ "Yes",
    is.na(orcid_id)      ~ "No"
    )
  )
#---------------------------------
# gender
#---------------------------------
analysis_dat %>%
   dplyr::group_by(gender_of_fellow) %>%
   dplyr::summarise(
     count = n(),  .groups = "drop"
   ) 
#===================================
# Number of publication before-after 
#===================================
# before-after fellowship
before_after <- analysis_dat %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

#=====================================
# by REGION and before-after fellowship
#=====================================
analysis_dat %>%
   dplyr::group_by(who_region_code) %>%
   dplyr::summarise(count = n(), .groups = "drop_last") %>%
   dplyr::mutate(
     total = sum(count),
     prop = (count / total) * 100
   )

analysis_dat %>%
   dplyr::group_by(who_region_code, time) %>%
   dplyr::summarise(count = n(), .groups = "drop_last") %>%
   dplyr::mutate(
     total = sum(count),
     prop = (count / total) * 100
   )

# by time-period
summary(analysis_dat$fellowship_year)
analysis_dat %>%
  dplyr::group_by(cut(fellowship_year, c(1999,2013,2019, 2022))) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

# by gender
analysis_dat %>%
  dplyr::group_by(gender_of_fellow) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

# by gender
per_fellow<- analysis_dat %>%
  dplyr::group_by(who_fellow) %>%
  dplyr::summarise(
    count = n(),
      ) 
summary(per_fellow$count)

#===========================
# Fellow characteristics
#===========================
length(table(dat_fellows$country_name))
table(dat_fellows$country_name,dat_fellows$WHO_Region)
table(dat_fellows$WHO_Region)

# by WHO region
dat_fellows %>%
  dplyr::group_by(WHO_Region_Code) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

# by region
dat_fellows %>%
  dplyr::group_by(WHO_Region_Code) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

# by region and year
dat_fellows %>%
  dplyr::group_by(WHO_Region_Code) %>%
  dplyr::summarise(
    min_year = min(placement_year),
    max_year = max(placement_year)
  ) 

# by region and gender
dat_fellows %>%
  dplyr::group_by(gender) %>%
  #dplyr::group_by(WHO_Region_Code, gender) %>%
  dplyr::summarise(count = n(), .groups = "drop_last") %>%
  dplyr::mutate(
    total = sum(count),
    prop = (count / total) * 100
  )

# by WHO region and ORCID
dat_fellows %>%
  dplyr::group_by( orcid_presence) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

# by WHO region and any publication status
dat_fellows %>%
  #dplyr::group_by(WHO_Region_Code, any_pub) %>%
  dplyr::group_by(any_pub) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

# by income level
dat_fellows %>%
  dplyr::group_by(income_level_id) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

# by income level and WHO region
dat_fellows %>%
  dplyr::group_by(home_country, who_region_code) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( 
    total = sum(count),
    prop = (count/sum(count))*100) 

# by TPO destination
dat_fellows %>%
  dplyr::group_by(tpo_region) %>%
  #dplyr::group_by(WHO_Region_Code, tpo_region) %>%
  dplyr::summarise(count = n(), .groups = "drop_last") %>%
  dplyr::mutate(
    total = sum(count),
    prop = (count / total) * 100
  )

dat_fellows %>%
  dplyr::group_by(who_region_code, tpo_region) %>%
  dplyr::summarise(count = n(), .groups = "drop_last") %>%
  dplyr::mutate(
    total = sum(count),
    prop = (count / total) * 100
  )

#======================================================
# Authorship positions
#======================================================

#---------------------------------------------
# by region
#---------------------------------------------
analysis_dat %>%
  dplyr::group_by(who_region_code, author_position) %>%
  dplyr::summarise(count = n(), .groups = "drop_last") %>%
  dplyr::mutate(
    total = sum(count),
    prop = (count / total) * 100
  )

#---------------------------------------------
# median number of publications per fellows
#---------------------------------------------
med_pub <- analysis_dat %>%
  dplyr::group_by(who_fellow,who_region_code) %>%
    dplyr::summarise(
    count = n()
  ) %>% 
  dplyr::group_by(who_region_code) %>%
  dplyr::summarise(
    median_pub = median(count),
    q25_pub = quantile(count,0.25),
    q75_pub = quantile(count,0.75),
    min_pub = min(count),
    max_pub = max(count)
  ) 

med_pub
#------------------------------------
# Export fellows information as Table
#------------------------------------
pdf("Desktop/Mahnaz project/final/fellows_details.pdf", 
    height=8, 
    width=8
)

grid.table(before_after)

plot.new()
grid.table(before_after_region)

plot.new()
grid.table(before_after_region_time)

plot.new()
grid.table(n_fellows_by_gender)

plot.new()
grid.table(auth_positon)

plot.new()
grid.table(auth_positon_region)

plot.new()
grid.table(med_pub)

dev.off()

# END