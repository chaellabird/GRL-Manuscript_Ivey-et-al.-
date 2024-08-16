
#Load packages
library(pacman)
pacman::p_load(tidyverse, readxl, lemon)


#Load data
all.manipulated.data<-read_excel('Output/Data/Bootstrapped_Counts.xlsx') %>% 
mutate(Type = factor(Type, levels = c("Random Points","Fires")),
       NA_L3NAME = factor(NA_L3NAME, 
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
 
random_points_data <- all.manipulated.data %>% 
  filter(Type == "Random Points")
fires_data <- all.manipulated.data %>% filter(Type == "Fires")

# Create the plot
bootstrapped <- ggplot() +
  # Plot Random Points first
  geom_col(data = random_points_data, aes(x = Bins, y = counts, fill = Type), alpha = 0.4, color = "black", position = position_identity()) +
  # Add error bars for Random Points
  geom_errorbar(data = random_points_data, 
                aes(x = Bins, ymin = counts - stdev, ymax = counts + stdev), 
                width = 0.2, color = "black", position = position_identity()) +
  # Plot Fires on top
  geom_col(data = fires_data, aes(x = Bins, y = counts, fill = Type), alpha = 0.4, color = "black", position = position_identity()) +
  facet_rep_wrap(~NA_L3NAME, 
             scales = "free_y",
             nrow = 5,
             repeat.tick.labels = FALSE,
             labeller = label_wrap_gen(width = 22))+
  xlab("Percent woody cover") +
  ylab("Wildfire Frequency")+
  theme_bw(base_size = 6)+
  scale_fill_manual(values = c("blue","yellow"),
                    name= "Sample type")+
  theme(axis.text.x = element_text(angle = 60, hjust=1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 8),
        legend.position = "right",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"))#,



ggsave(bootstrapped,file = "Output/Graphs and Figures/Figure 3.jpg",
       dpi = 500,
       width = 6,
       height = 5)


ggsave(bootstrapped,file = "Output/Graphs and Figures/Figure 3_standard_error.jpg",
       dpi = 500,
       width = 6,
       height = 5)

ggsave(bootstrapped, file ="Output/Graphs and Figures/Figure 3_standard_deviation.jpg",
       dpi = 500,
       width = 6,
       height = 5) 

####Do the ALL YEARS VERSION ####

#Load data
ALLYEARS<-read_excel('Output/Data/Bootstrapped_Counts_ALL_YEARS.xlsx') %>% 
  mutate(Type = factor(Type, levels = c("Random Points","Fires")),
         NA_L3NAME = factor(NA_L3NAME, 
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
                                       "Western Allegheny Plateau"))) %>% 
  filter(Type == "Random Points") %>% 
  mutate(range = "All years")
restricted_years <-random_points_data %>% 
  mutate(range = "Fire years only")
colnames(restricted_years)
colnames(ALLYEARS)
# Create the plot
bootstrapped_restricted_vs_all <- ggplot() +
  # Plot all years first
  geom_col(data = ALLYEARS, aes(x = Bins, y = counts, fill = range), alpha = 0.4, color = "black", position = position_identity()) +
  # Add error bars for Random Points
  geom_errorbar(data = ALLYEARS, 
                aes(x = Bins, ymin = counts - standard_error, ymax = counts + standard_error), 
                width = 0.2, color = "black", position = position_identity()) +
  # Plot restricted years on top
  geom_col(data = restricted_years, aes(x = Bins, y = counts, fill = range), alpha = 0.4, color = "black", position = position_identity()) +
  geom_errorbar(data = restricted_years, 
                aes(x = Bins, ymin = counts - standard_error, ymax = counts + standard_error), 
                width = 0.2, color = "black", position = position_identity()) +
  facet_wrap(~NA_L3NAME, 
             scales = "free",
             nrow = 5,
             labeller = label_wrap_gen(width = 22))+
  xlab("Percent woody cover") +
  ylab("Wildfire Frequency")+
  theme_bw(base_size = 6)+
  scale_fill_manual(values = c("blue","yellow"),
                    name= "year sampling range")+
  theme(axis.text.x = element_text(angle = 60, hjust=1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 8),
        legend.position = "right",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"))#,



ggsave(bootstrapped_restricted_vs_all,file = "Output/Graphs and Figures/allyears_vs_fireyearsonly_standard_error_.jpg",
       dpi = 500,
       width = 6,
       height = 5)

ggsave(bootstrapped_restricted_vs_all, file ="Output/Graphs and Figures/allyears_vs_fireyearsonly_stdev.jpg",
       dpi = 500,
       width = 6,
       height = 5) 







####Create a distribution split by decade (1991-2001,2002-2011,2012-2021)####

split<-read_excel('Output/Data/binned_counts_by_decade.xlsx')
dec90s<-split %>% 
  filter(decade == "1991-2001")
dec00s<-split %>% 
  filter(decade == "2002-2011")
dec10s<-split %>% 
  filter(decade == "2012-2021")

split_plot<-ggplot() +
  # Plot Fires on top
  #geom_col(data = dec90s, aes(x = Bins, y = counts, fill = decade), alpha = 0.4, color = "black", position = position_identity()) +
  #
  #geom_col(data = dec00s, aes(x = Bins, y = counts, fill = decade),
  #         alpha = 0.4, color = "black", position = position_identity()) +
  geom_col(data = dec10s, aes(x = Bins, y = counts, fill = decade), 
           alpha = 0.4, color = "black", position = position_identity()) +
  facet_wrap(~NA_L3NAME, 
             scales = "free_y",
             nrow = 5,
             labeller = label_wrap_gen(width = 22))+
  xlab("Percent woody cover") +
  ylab("Wildfire Frequency")+
  theme_bw(base_size = 6)+
  scale_fill_manual(values = "red",#c("blue","yellow","red"),
                    name = "Decade")+
  theme(axis.text.x = element_text(angle = 60, hjust=1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 8),
        legend.position = "right",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"))#,


ggsave(split_plot, file ="Output/Graphs and Figures/dist_split.jpg",
       dpi = 500,
       width = 6,
       height = 5) 


ggsave(split_plot, file ="Output/Graphs and Figures/dist_1990s.jpg",
       dpi = 500,
       width = 6,
       height = 5) 

ggsave(split_plot, file ="Output/Graphs and Figures/dist_2000s.jpg",
       dpi = 500,
       width = 6,
       height = 5) 
ggsave(split_plot, file ="Output/Graphs and Figures/dist_2010s.jpg",
       dpi = 500,
       width = 6,
       height = 5) 
