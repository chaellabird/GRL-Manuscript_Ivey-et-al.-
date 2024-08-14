#Michaella Ivey
#This script does the same thing as 2, but at the L1 level
  #it removes all duplicates
#Its output is an L1 Presence/Absence dataframe.

#Load packages
  library(pacman)
  pacman::p_load(tidyverse,readxl,writexl,sf,terra)


#Load data
  random<-read_csv("Output/Data/L1_all_buffered_points_woody_cover.csv") %>%
    filter(Year > 1989)


#import the file with all the fires in the L1
fire.cover<-read_excel("Output/Data/L1_All_Fires_Woody_Cover.xlsx") %>% #We need to remove duplicates though. 
  filter(previousyear == Rap_year) %>% #choose only years where RAP year matches the previous year designation. 
  mutate(PA = 1) %>% 
  rename(fractional_cover = "percent_cover") 

#pull years previous to wildfires, assign randomly to the random points
year_key<-fire.cover %>%
  select(Rap_year,L3CODE,NA_L3NAME) 

# Assuming 'random' is your dataframe
random_list <- random %>%
  select(TARGET_FID, L3CODE) %>%
  distinct() %>% 
  split(.$L3CODE)

random_list<- lapply(random_list, function(x){
  region = x$L3CODE
  length_out = length(unique(x$TARGET_FID))
  x$RAP_year = rep(year_key$Rap_year[year_key$L3CODE == region],
                    length.out = length_out) 
  x$NA_L3NAME = year_key$NA_L3NAME[year_key$L3CODE == region] %>% 
    unique()
  return(x)
})
random_with_years<-do.call(rbind,random_list) #there are 29999 unique TARGET_FIDs. 

#Sample the Data
sample_targets <- function(data) {
  sample_size <- length(unique(fire.cover$Event_ID)) #this removes the duplicates. 
  sample_n(data, sample_size)
}
sampled_points<-random_with_years %>% 
  sample_targets()


nullfires<- random[random$TARGET_FID %in% sampled_points$TARGET_FID ,] %>% 
  left_join(sampled_points) %>% 
  select(-c(Evaluated_Hectares,SHR_Hectares,Tree_Hectares)) %>% 
  filter(RAP_year == Year) %>% 
  select(-c(RAP_year,Year,L3CODE,NA_L3NAME,TARGET_FID)) %>% 
  mutate(PA = 0) %>% #,
  #TARGET_FID = as.character(TARGET_FID)) %>% 
  rename(#Event_ID = "TARGET_FID",
    fractional_cover = "percent_cover") 

colnames(nullfires)
colnames(fire.cover)


#now create new dataframes where all the columns and names match up. 
PA<-fire.cover %>%
  select(fractional_cover,PA) %>%
  distinct() %>% 
  bind_rows(nullfires) %>% 
  mutate(NA_L3NAME = "L1 Eastern Temperate Forests",
         L3CODE = NA)

write_xlsx(PA, "Output/Data/L1_Presence_Absence_data.xlsx")

