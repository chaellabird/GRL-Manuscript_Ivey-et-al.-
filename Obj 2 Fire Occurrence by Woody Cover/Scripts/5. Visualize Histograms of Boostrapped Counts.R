
#Load packages
library(pacman)
pacman::p_load(tidyverse, readxl)


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
 
random_points_data <- all.manipulated.data %>% filter(Type == "Random Points")
fires_data <- all.manipulated.data %>% filter(Type == "Fires")

# Create the plot
bootstrapped <- ggplot() +
  # Plot Random Points first
  geom_col(data = random_points_data, aes(x = Bins, y = counts, fill = Type), alpha = 0.4, color = "black", position = position_identity()) +
  # Plot Fires on top
  geom_col(data = fires_data, aes(x = Bins, y = counts, fill = Type), alpha = 0.4, color = "black", position = position_identity()) +
  facet_wrap(~NA_L3NAME, 
             scales = "free",
             nrow = 5,
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




