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
 
length(unique(L1.rand.cover$TARGET_FID))

#create a dataframe with just years fires happened in each region.
yearsandregions.L3<-read_excel("Output/Data/study_regions_fire_woody_cover.xlsx") %>% 
  select(L3CODE,NA_L3NAME,previousyear,count) %>% 
  unique() %>% 
  mutate(sample_count = 1000) #This indicates we are starting with 1000 random points per ecoregion.





#we need to eliminate duplicates resulting from fires that cross ecoregion boundaries 
yearsandregions.L1<-read_excel('Output/Data/L1_All_Fires_Woody_Cover.xlsx') %>% 
  select(previousyear,Event_ID) %>% #disregard what ecoregion it came from originally.
  unique() %>%
  mutate(sample_count = length(unique(L1.rand.cover$TARGET_FID)), #equals 29999, the number of random points we start with
         count = length(Event_ID), #this equals the number of actual wildfires.
         NA_L3NAME = "L1 Eastern Temperate Forests",
         L3CODE = 0) %>% 
  select(-Event_ID)
yearsandregions<-bind_rows(yearsandregions.L1,yearsandregions.L3) #2555 long. 


#For each ecoregion, select 1000 (or in the case of L1 29999) years, resampling if necessary.
 #every random point will need a year assigned, regardlss of whether it makes it into the final distribution
sampled_years <- yearsandregions %>%
  group_by(L3CODE) %>%
  group_modify(~ slice_sample(.x, n = .x$sample_count[1], replace = TRUE)) %>% 
  arrange(L3CODE) 


####just for funsies pull from ALL years. skip if you are running the normal code####
key <-read_excel('Output/Data/ecoregion_key.xlsx') %>% 
  filter(NA_L3NAME %in% yearsandregions.L3$NA_L3NAME)
yearsandregions_allyears <- data.frame(
  previousyear = seq(from = 1990, to = 2020, by = 1)
) %>%  
 mutate(NA_L3NAME = "L1 Eastern Temperate Forests",
        L3CODE = 0,
        sample_count = length(unique(L1.rand.cover$TARGET_FID))) %>% 
  bind_rows(data.frame(
    previousyear = rep(seq(from = 1990, to = 2020, by = 1), times = 19)
  ) %>%  
    mutate(NA_L3NAME = rep(key$NA_L3NAME, each = 31),
           L3CODE = rep(key$L3CODE, each = 31),
           sample_count = 1000)) %>% 
  left_join(yearsandregions %>% 
              select(NA_L3NAME,count) %>% 
              unique())%>% 
    group_by(L3CODE,NA_L3NAME) 

sampled_years<-yearsandregions_allyears %>% 
  group_modify(~ slice_sample(.x, n = .x$sample_count[1], replace = TRUE)) %>% 
  arrange(L3CODE)

key <-read_excel('Output/Data/ecoregion_key.xlsx') %>% 
  rbind(new_row)



####Back to normal ####
#merge the burnable years with the Target_FIDs of the null point data.
random_ids<-rand.cover %>% 
  select(TARGET_FID,L3CODE) %>% 
  unique()%>% #this has 48999 obs, because L1 has 29,999, the rest are L3 obs.
  arrange(L3CODE) %>% 
  bind_cols(sampled_years) %>% 
  select(previousyear,TARGET_FID) %>% 
  inner_join(rand.cover, by = "TARGET_FID") %>% 
  filter(Year == previousyear) %>% 
  left_join(key)


#this rand data is now good to go through the bootstrapping loop, either for 
   #the L1 ecoregion, or by L3 ecoregion. 

#set the breakpoints that will be used. 
breakpoints <- c(0, .10, .20, .30,  .40 , .50  ,.60 , .70 , .80,  .90 ,1.01)
bin<-c(.10,  .20,  .30,  .40 , .50  ,.60 , .70 , .80,  .90 ,1.01) #bin should start at .1



# I create a grouping variable dataframe that just has ecoregions and fire counts
sample_info<-yearsandregions %>% 
  select(-c(sample_count,previousyear)) %>% 
  unique()  
#initiate empty list
wood_x<- vector("list", length = 1000)

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

all_sampled_data_list[[current_group]] <- sampled_counts_list
#rm(list_element_name) This didn't work to remove the thing from the environment
}

#what you have in all sampled_data_list is a list of each region, each of which contains a list of 1000.
#the 1000 is the number of times we sampled. Each of those 1000 is a vector of 10 numbers, referring
#to counts in each cover bin. Think of them as counts of how many artificial fires occurred in each cover category.
#Those 10 will become 10 columns in the next few steps
#then we will get the column means, stdev, and standard error. 


####Moving on####
breaks<-c("0 - 10","10 - 20","20 - 30","30 - 40","40 - 50","50 - 60","60 - 70","70 - 80","80 - 90","90 - 100")

#A loop to get the col means of all groups

wood.cover.means<- lapply(all_sampled_data_list, function(x) {
 woodr_output <- matrix(unlist(x), ncol = 10, byrow = TRUE) # this is a matrix, 1000 rows X 10
 #write a loop for each column in the matrix
 wood_standard_error <- apply(woodr_output, 2, function(cols) {
   sd(cols, na.rm = TRUE) / sqrt(sum(!is.na(cols))) 
 }) #this produces a vector (length = 10) with the standard error of each column
 wood_standard_dev<-apply(woodr_output, 2, function(cols) {
   sd(cols, na.rm = TRUE)
 })
 wood_mean<- colMeans(woodr_output, na.rm = FALSE, dims = 1) %>%  
  as.numeric() %>%
  bind_cols(breaks) %>%  #add the breaks for identification
  mutate(standard_error = wood_standard_error,
         stdev = wood_standard_dev)
  }) %>% 
  bind_rows(.id = "NA_L3NAME") %>% 
  #separate(df_name, into = c('NA',"NA_L3NAME"), sep = "_") %>% 
  rename("counts" = "...1",
         "Bins" = "...2") %>% 
  mutate(Type = "Random Points") %>% 
  select(NA_L3NAME,Bins,counts,Type,standard_error,stdev)



L3<-read_csv("Output/Data/Presence_Absence_data.csv") %>% 
  mutate(PA = factor(PA)) %>% 
  left_join(key) 
L1<-read_excel('Output/Data/L1_Presence_Absence_data.xlsx')
pa.counts<-rbind(L1,L3) %>% 
  mutate(PA = as.factor(PA)) %>% 
  filter(PA == 1) %>% #just the actual fires mind you 
  mutate(Bins = cut(fractional_cover, breaks = breakpoints, labels = breaks)) %>%
  left_join(key) %>% 
  group_by(NA_L3NAME) %>% 
  count(Bins)%>% #unsure why this step is here. Unless to count how many are in the bin
  mutate(Type = "Fires",
         standard_error = NA,
         stdev = NA) %>% 
  rename("counts" = "n")
colnames(pa.counts)
colnames(wood.cover.means)


all.manipulated.data<-bind_rows(pa.counts,wood.cover.means)

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


#if you did the all years version
write_xlsx(all.manipulated.data,"Output/Data/Bootstrapped_Counts_ALL_YEARS.xlsx")




####Create a version split by decade####
L3<-read_csv("Output/Data/Presence_Absence_data_by_decade.csv") %>% 
  mutate(PA = factor(PA)) %>% 
  left_join(key) 
L1<-read_csv('Output/Data/L1_Presence_Absence_data_by_decade.csv') %>% 
  mutate(NA_L3NAME = "L1 Eastern Temperate Forests",
          L3CODE = 0)


pa.counts_by_decade<-rbind(L1,L3) %>% 
  mutate(PA = as.factor(PA)) %>% 
  mutate(Bins = cut(fractional_cover, breaks = breakpoints, labels = breaks)) %>%
  left_join(key) %>% 
  group_by(NA_L3NAME,decade) %>% 
  count(Bins) %>% #unsure why this step is here. Unless to count how many are in the bin
  mutate(Type = "Fires",
         standard_error = NA) %>% 
  rename("counts" = "n")


write_xlsx(pa.counts_by_decade, "Output/Data/binned_counts_by_decade.xlsx")




