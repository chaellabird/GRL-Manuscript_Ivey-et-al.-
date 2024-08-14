#Michaella Ivey
#The purpose of this script is to sample random points and join them to their
 #woody cover data
#Its outputs are a csv file with the woody cover for all random points in the L1,
 #a .xlsx file with all the fires at the L1 level (duplicates NOT removed),
 #a .xlsx file with all the fires for selected L3 regions,
 #a .xlsx file with all the random points in the selected L3 regions
 #a .xlsx file with sampled random points in the selected L3 regions



library(pacman)
pacman::p_load(tidyverse,lubridate,purrr,writexl,readxl)

####Process buffered points' vegetation cover data . (From the RAP)####

##bring in buffered random points with their vegetation cover data
  shrub_buffer<-read_csv("Data/New_Rcalc_unsplit_buffer_SHR.csv")
  tree_buffer<-read.csv("Data//New_Rcalc_unsplit_buffer_Evaluated_Hectares_Trees.csv")
  both_buffers<-merge(shrub_buffer,tree_buffer,by=c("TARGET_FID","Year"))
  buffer_area<-read_csv('Data/New_Rcalc_unsplit_buffer_evaluated_area_hectares.csv') 

  woody_buffer_points<-left_join(both_buffers,buffer_area) %>%
    mutate(percent_cover = (Tree_Hectares+SHR_Hectares)/Evaluated_Hectares) %>% 
    select(-c(L3CODE.x,L3CODE.y)) %>% #with new buffers. This currently has the entire Eastern United States. 
    filter(Evaluated_Hectares >0) #remove points that landed in masked out regions

#clean up the environment before moving on to the next step.
  rm(tree_buffer, shrub_buffer,both_buffers,buffer_area)

#save for calculating L1 Level statistics in script 2B
  write_csv(woody_buffer_points, file = "Output/Data/L1_all_buffered_points_woody_cover.csv")

####Process Fire data and its cover####
#This section gets all the woody cover data (Trees and shrubs) in a single dataframe, and selects only regions with 
  #fires > 200 ha and fire numbers > 10. It will also remove duplicate entries.
  
#import the fire and the fire accessory data in and merge
 #this has the burned area size of individual fires 
    accessory.fire<-read_excel("C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Data/Output/fires_Eastern_temperate_forest.xlsx") %>% 
    mutate(L3CODE = as.numeric(US_L3CODE)) %>% 
    select(-c(NA_L3CODE,US_L3CODE))
  length(unique(accessory.fire$Event_ID)) #there are 2184 unique Event_IDs.
  
 #These two files have the shrub and tree cover for fires, from the RAP, calculated in Google Earth Engine.
  fire.shrub.cover<-read.csv("Data/UNSPLIT_Fire_Evaluated_Hectares_Shrubs.csv")
  fire.tree.cover<-read.csv("Data/UNSPLIT_Fire_Evaluated_Hectares_Trees.csv")
  fire_area<-read.csv('Data/UNSPLIT_Fire_evaluated_total_hectares.csv')
#there are 999936 because it is the whole United States. 
 
   fire_woody_cover<-left_join(fire.shrub.cover,fire.tree.cover) %>% 
    left_join(fire_area) %>%
     select(-c(L3CODE,burned_area_split,area_ha_presplit)) %>% 
    right_join(accessory.fire, by = c("Event_ID")) %>% 
    mutate(percent_cover = (Tree_Hectares+SHR_Hectares)/Evaluated_Hectares,
           fire_year = year(as.POSIXct(Ig_Date, format="%m/%d/%Y  %H:%M:%S")),
           previousyear = fire_year-1) %>%  #Previous year's vegetation is what we are interested in. 
    rename(Rap_year = "Year") %>% 
    filter(fire_year >1990) %>%
    group_by(NA_L3NAME) %>% 
    mutate(count = length(unique(Event_ID)))
   
#Save a dataframe with the L1 fires(duplicates not removed) for use in script 2B 
   write_xlsx(fire_woody_cover,"Output/Data/L1_All_Fires_Woody_Cover.xlsx")
   
#filter out the counts to exclude regions with too few fires
   fire_woody_cover <-fire_woody_cover %>% 
     filter(count >=10)
  
#save fire data for later use in script 2  
   write_xlsx(fire_woody_cover, "Output/Data/study_regions_fire_woody_cover.xlsx")

  
#get the random points data
  randdata <- woody_buffer_points %>%
    filter(L3CODE %in% fire_woody_cover$L3CODE) #exclude the regions with <10 fires
  
#save processed random points for use in bootstrapping in script 4. 
  write_xlsx(randdata, "Output/Data/all_random_points_woody_cover.xlsx")

  
#### Prepare data for random sampling ####

  
#create dataframe with just unique combinations of region and point id. 
#the purpose of this is to reduce computer run time while random sampling. 
  df1<-fire_woody_cover %>% 
    select(L3CODE,count) %>% 
    unique() 
  sum(df1$count)  
  unique_temp<-randdata %>%
    select(c(L3CODE,TARGET_FID)) %>%
    unique() %>% 
    left_join(df1) #add the fire count data from the wildfire dataframe
 

  sample_targets <- function(data) {
    sample_size <- unique(data$count) 
    sample_n(data, sample_size)
  }
  
# Group by NA_L3NAME and apply the sampling function
  sampled_IDs <- unique_temp %>%
    group_by(L3CODE) %>%
    group_modify(~ sample_targets(.x)) %>%
    ungroup()
  selectedpoints<- randdata[randdata$TARGET_FID %in% sampled_IDs$TARGET_FID ,] %>% 
    select(-c(Evaluated_Hectares,SHR_Hectares,Tree_Hectares))
  

#Write a file with selected random points for use in script 2
  write_xlsx(selectedpoints, "Output/Data/selectedrdmpts_woody_cover.xlsx")


