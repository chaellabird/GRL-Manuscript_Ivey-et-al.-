library(pacman)
pacman::p_load(tidyverse, readxl,writexl)

new_row <- data.frame(
  NA_L3NAME = "L1 Eastern Temperate Forests",
  L3CODE = 0)

key <-read_excel('Output/Data/ecoregion_key.xlsx') %>% 
  rbind(new_row)



####bootstrapped histogram####

  
#Import random points
L3.rand.cover<-read_xlsx("Output/Data/all_random_points_woody_cover.xlsx") %>% 
  select(-c(Evaluated_Hectares,SHR_Hectares,Tree_Hectares)) %>% 
  mutate(TARGET_FID = as.character(TARGET_FID))

L1.rand.cover<-read_csv('Output/Data/L1_all_buffered_points_woody_cover.csv') %>% 
  select(-c(SHR_Hectares,Tree_Hectares,Evaluated_Hectares,L3CODE)) %>%
  mutate(L3CODE = 0,
         TARGET_FID = paste0("L1",TARGET_FID))
rand.cover<-bind_rows(L1.rand.cover,L3.rand.cover) %>% 
  left_join(key)
 


#create a dataframe with just years fires happened in each region.
yearsandregions.L3<-read_excel("Output/Data/study_regions_fire_woody_cover.xlsx") %>% 
  subset(select = c(L3CODE,NA_L3NAME,previousyear,count)) %>% 
  unique() %>% 
  mutate(sample_count = 1000)

#we need to eliminate duplicates. 
yearsandregions.L1<-read_excel('Output/Data/L1_All_Fires_Woody_Cover.xlsx') %>% 
  select(previousyear,Event_ID) %>%
  unique() %>%
  mutate(sample_count = length(unique(L1.rand.cover$TARGET_FID)),
         count = length(Event_ID),
         NA_L3NAME = "L1 Eastern Temperate Forests",
         L3CODE = 0) %>% 
  select(-Event_ID)
yearsandregions<-bind_rows(yearsandregions.L1,yearsandregions.L3) #2555 long. 


#For each ecoregion, select 1000 years, resampling if necessary. 
sampled_years <- yearsandregions %>%
  group_by(L3CODE) %>%
  group_modify(~ slice_sample(.x, n = .x$sample_count[1], replace = TRUE)) %>% 
  arrange(L3CODE) 


#merge the burnable years with the Target_FIDs of the null point data.
random_ids<-rand.cover %>% 
  select(TARGET_FID,L3CODE) %>% 
  unique()%>% #this has 48999 obs, because L1 has 29,999.
  arrange(L3CODE) %>% 
  bind_cols(sampled_years) %>% #here is the issue. Wrong number of rows
  select(previousyear,TARGET_FID) %>% 
  inner_join(rand.cover, by = "TARGET_FID") %>% 
  filter(Year == previousyear) %>% 
  left_join(key)


#this rand data is now good to go through the bootstrapping loop, either for 
   #the L1 ecoregion, or by L3 ecoregion. 

#set the breakpoints that will be used. 
breakpoints <- c(0, .10, .20, .30,  .40 , .50  ,.60 , .70 , .80,  .90 ,1.01)
bin<-c(.10,  .20,  .30,  .40 , .50  ,.60 , .70 , .80,  .90 ,1.01) #bin should start at .1



# I create a grouping variable dataframe that just has ecoregions and counts
sample_info<-yearsandregions %>% 
  select(-c(sample_count,previousyear)) %>% 
  unique()  

wood_x<- vector("list", length = 1000)#what does this 1000 refer to?

   # Sample the data for the current group
all_sampled_data_list <- list() #big list to store everybody

for(current_group in sample_info$NA_L3NAME) {
  list_element_name <-paste0("wood_", current_group)
#create an empty list to store data
sampled_counts_list <- list()
  for (i in 1:1000) { #by the way 1000 is just EVERYTHING  
  drawidwood <- random_ids %>%
    filter(NA_L3NAME == current_group) %>%
    sample_n(sample_info$count[sample_info$NA_L3NAME == current_group], replace = TRUE)
    hist<-hist(drawidwood$percent_cover, breaks= breakpoints, plot = FALSE)
    sampled_counts_list[[i]]<- hist$counts }

assign(list_element_name, 
       sampled_counts_list, envir = .GlobalEnv)

all_sampled_data_list[[list_element_name]] <-sampled_counts_list
#rm(list_element_name) This didn't work to remove the thing from the environment
}


#some current issues with the code are that it still spits out the individual lists. 
#They clutter up the environment. I need a way to write them to the ALL
#without going to the environment.

breaks<-c("0 - 10","10 - 20","20 - 30","30 - 40","40 - 50","50 - 60","60 - 70","70 - 80","80 - 90","90 - 100")

#A loop to get the col means of all groups
wood.cover.means<- lapply(all_sampled_data_list, function(x) {
 woodr_output <- matrix(unlist(x), ncol = 10, byrow = TRUE)
wood_mean<-colMeans(woodr_output, na.rm = FALSE, dims = 1) %>%
  as.numeric() %>% 
  bind_cols(breaks)
  }) %>% 
  bind_rows(.id = "df_name") %>% 
  separate(df_name, into = c('NA',"NA_L3NAME"), sep = "_") %>% 
  rename("counts" = "...1",
         "Bins" = "...2") %>% 
  mutate(Type = "Random Points") %>% 
  select(NA_L3NAME,Bins,counts,Type)



L3<-read_csv("Output/Data/Presence_Absence_data.csv") %>% 
  mutate(PA = factor(PA)) %>% 
  left_join(key) 
L1<-read_excel('Output/Data/L1_Presence_Absence_data.xlsx')
pa.counts<-rbind(L1,L3) %>% 
  mutate(PA = as.factor(PA)) %>% 
  filter(PA == 1) %>% 
  mutate(Bins = cut(fractional_cover, breaks = breakpoints, labels = breaks)) %>%
  left_join(key) %>% 
  group_by(NA_L3NAME) %>% 
  count(Bins)%>% #unsure why this step is here. Unless to count how many are in the bin
  mutate(Type = "Fires") %>% 
  rename("counts" = "n")
colnames(pa_counts)
colnames(wood.cover.means)


all.manipulated.data<-rbind(pa.counts,wood.cover.means)

  all.manipulated.data <-all.manipulated.data %>% 
    mutate(NA_L3NAME = factor(NA_L3NAME, 
                              levels = c("L1 Eastern Temperate Forests",
                                         "Arkansas Valley",
                                         "Atlantic Coastal Pine Barrens",
                                         "Blue Ridge",
                                         "Boston Mountains",
                                         "Central Appalachians",
                                         "East Central Texas Plains",
                                         "Interior Plateau",
                                         "Middle Atlantic Coastal Plain",
                                         "Mississippi Valley Loess Plains",
                                         "North Central Hardwood Forests",
                                         "Ouachita Mountains",
                                         "Ozark Highlands",
                                         "Piedmont",
                                         "Ridge and Valley",
                                         "South Central Plains",
                                         "Southeastern Plains",
                                         "Southern Coastal Plain",
                                         "Southwestern Appalachians",
                                         "Western Allegheny Plateau")))


#Save for visualization. 
write_xlsx(all.manipulated.data,"Output/Data/Bootstrapped_Counts.xlsx")







