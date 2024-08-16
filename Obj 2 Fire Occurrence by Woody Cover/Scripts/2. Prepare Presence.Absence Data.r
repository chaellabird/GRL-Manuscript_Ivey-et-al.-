#Michaella Ivey
#The purpose of this script is to turn the L3 fire data and the random points data
 #into a single dataframe with a column for presence and absence of fire.
#It's output is just that. 

#Load packages
  library(pacman)
  pacman::p_load(tidyverse,readxl,writexl)


#import the selected random points. 
  random<-read_excel("Output/Data/selectedrdmpts_woody_cover.xlsx") %>%
    subset(Year > 1989)


#import the file with only the fires from regions with more than 10
  fire.cover<-read_excel("Output/Data/study_regions_fire_woody_cover.xlsx") %>%
    filter(previousyear == Rap_year) %>% #choose only years where RAP year matches the previous year designation. 
    mutate(PA = 1) %>% 
    rename(fractional_cover = "percent_cover") %>% 
    select(-c(Evaluated_Hectares,Ig_Date,area_ha))



#pull years previous to wildfires, assign randomly to the random points
  year_key<-fire.cover %>%
    select(NA_L3NAME,Rap_year,L3CODE) %>%
    arrange(L3CODE)
  
  
  nullfires <- random %>%
    arrange(L3CODE) %>%
    select(TARGET_FID,L3CODE) %>%
    unique()%>%
    mutate(select.year = year_key$Rap_year) %>% 
    left_join(random) %>% 
    filter(select.year == Year) %>% 
    select(-c(select.year,Year)) %>% 
    mutate(PA = 0,
           TARGET_FID = as.character(TARGET_FID)) %>% 
    rename(Event_ID = "TARGET_FID",
           fractional_cover = "percent_cover") %>% 
    select(-Event_ID)

  colnames(nullfires)
  colnames(fire.cover)


#now create new dataframes where all the columns and names match up. 
  PA<-fire.cover %>% 
    select(L3CODE,fractional_cover,PA) %>% 
    bind_rows(nullfires)

#This will be used in script 3
write_csv(PA, "Output/Data/Presence_Absence_data.csv")


####Save a version of the fires split by decade####


PA_by_decade<-fire.cover %>% 
  select(L3CODE,fractional_cover,PA,fire_year) %>% 
 mutate(decade = case_when(fire_year <= 2001 ~ "1991-2001",
                           fire_year < 2012 & fire_year >2001 ~ "2002-2011",
                           fire_year >2011 ~ "2012-2021"))


write_csv(PA_by_decade, "Output/Data/Presence_Absence_data_by_decade.csv")


