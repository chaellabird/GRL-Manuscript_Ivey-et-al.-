
#Load packages
library(pacman)
pacman::p_load(readxl,tidyverse,sf,ggrepel,
               terra,tidyterra,cowplot,RColorBrewer,
               classInt,ggspatial)


####Study Area Map#####

#Bring in Level 1 ecoregions
us_l1<-read_sf("Data/us_eco_l3/us_eco_l3.shp") %>% 
  subset(NA_L1CODE == 8) %>% 
  st_make_valid() %>% 
  st_union() 
#plot(us_l1)
#import a boundary of the United States


colnames(states)

#### ####
inset.map<-ggplot()+
  geom_sf(data = us.boundary, color = "black", fill = "transparent")+
  geom_sf(data = lev1.merged, fill = "light grey")+
  geom_sf(data = states, fill = "transparent")+
  coord_sf()+
  theme_void()+
  theme(panel.border = element_rect(colour = "black",
                                    fill="NA", 
                                    linewidth=2.5),
        panel.background = element_rect(fill = "white"))


####Map Ecoregions with Labels####
#ecoregion.key<-read_excel("C:/Users/micha/Thesis Data/Keys and Acessory Files/ecoregion_key.xlsx")
states_and_regions<-read_sf('Data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp') %>% 
  filter(NA_L1CODE == 8) %>% 
  left_join(read_sf('Data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp') %>% 
              filter(NA_L1CODE == 8) %>%
              group_by(NA_L3NAME) %>% 
              mutate(min_latitude = min(st_coordinates(geometry)[, "Y"])) %>%
              select(min_latitude,NA_L3NAME) %>%
              st_drop_geometry() %>% 
              unique() %>%
              ungroup() %>% 
              arrange(min_latitude) %>% 
              mutate(latnum = row_number())
  ) %>% 
  unite(col = "latnum_name", 
        c("latnum", "NA_L3NAME"), sep = "  ",
        remove = FALSE) %>% 
  mutate(latnum_name = factor(latnum_name, 
                              levels = c("1  Southern Coastal Plain",                            
                                         "2  East Central Texas Plains",                        
                                         "3  Mississippi Alluvial Plain",                      
                                         "4  South Central Plains",                            
                                         "5  Mississippi Valley Loess Plains",                  
                                         "6  Southeastern Plains",                             
                                         "7  Piedmont",                                         
                                         "8  Middle Atlantic Coastal Plain",                   
                                         "9  Ridge and Valley",                                
                                         "10  Southwestern Appalachians",                       
                                         "11  Ouachita Mountains",                             
                                         "12  Arkansas Valley",                                
                                         "13  Blue Ridge",                                     
                                         "14  Interior Plateau",                               
                                         "15  Boston Mountains",                               
                                         "16  Ozark Highlands",                                
                                         "17  Central Appalachians",                           
                                         "18  Interior River Valleys and Hills",                
                                         "19  Western Allegheny Plateau",                       
                                         "20  Eastern Corn Belt Plains",                       
                                         "21  Northern Piedmont",                              
                                         "22  Central Corn Belt Plains",                        
                                         "23  Erie Drift Plain",                                
                                         "24  Atlantic Coastal Pine Barrens",                  
                                         "25  Southern Michigan/Northern Indiana Drift Plains", 
                                         "26  Huron/Erie Lake Plains",                         
                                         "27  Eastern Great Lakes Lowlands",                   
                                         "28  Driftless Area",                                  
                                         "29  Southeastern Wisconsin Till Plains",             
                                         "30  Northeastern Coastal Zone",                       
                                         "31  Northern Allegheny Plateau",                     
                                         "32  North Central Hardwood Forests",                 
                                         "33  Acadian Plains and Hills"))) %>%  
 mutate(latnum_name = fct_relabel(latnum_name, ~str_wrap(.x, width = 25)))

earth_tone_palette <- c(
  "#8D6E63", # Brown
  "#A1887F", # Light Brown
  "#BCAAA4", # Beige
  "#f2eee4", # Cream
  "#9E9D24", # Olive Green
  "#7B8D40", # Moss Green
  "#4CAF50", # Green
  "#C0CA33", # Lime Green
  "#8E8C84", # Khaki
  "#D7CCC8"  # Light Beige
)

test_states<-states_and_regions %>% 
  group_by(latnum) %>% 
  summarize(geometry = st_union(geometry))


ecoregion.plot<- ggplot() + 
  geom_sf(data = (states_and_regions %>% 
            group_by(latnum_name) %>% 
            summarize(geometry = st_union(geometry))),
          aes(fill = latnum_name),
          size = 1.5, color = "#333333") +
  geom_sf(data = (states_and_regions %>% 
            group_by(STATE_NAME) %>% 
            summarize(geometry = st_union(geometry))),
          aes(fill = STATE_NAME),
          size = 1.75, 
          color = "black",
          fill = "transparent")+
  theme_bw()+
  coord_sf()+
  scale_fill_manual(values = rep(greens_palette, length.out = 33),
                    name = "Ecoregion") +
 # scale_fill_manual(values = rep("#f2eee4", length(unique(states_and_regions$NA_L3NAME))), 
        #            name = "Ecoregion")+
  theme(axis.title       = element_blank(),
        panel.background = element_rect(color = "white"), #element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5,
                                    size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.margin = margin(0, 0, 0, 0), 
        legend.text = element_text(size = 7, margin = margin(-50, 0, -50, -19)), 
        legend.background = element_blank())+
  labs(main = "L3 Ecoregions of the Eastern United States",) + 
  guides(fill = guide_legend(override.aes = list(color = NA, fill = NA),
                             ncol = 4,
                             title.position = "top"))+
  
  ggrepel::geom_text_repel(data = test_states,
                           aes(label = latnum,
                               geometry = geometry),
                          # nudge_x = ifelse(test_states$latnum == 9, -15, 0),
                           #nudge_y = ifelse(states_and_regions$latnum == 9, 0.2, 0),
                           stat = "sf_coordinates",
                          box.padding = 0,
                          point.padding = 0.1,
                           fun.geometry = st_point_on_surface,
                           point.size = NA,
                           min.segment.length = Inf,
                           size = 2.5, 
                           color = "black",
                           bg.color = "white",
                           bg.r = 0.3,
                           max.overlaps = Inf)+
  annotation_scale(location = ('bl'), 
                   width_hint = 0.4,
                   pad_x = unit(0.3, "in"),
                   pad_y = unit(0.1, "in"),
                   text_cex = 1)+
  annotation_north_arrow(location = "br", width = unit(1.5, "cm"),
                         which_north = "true",
                         pad_x = unit(0.4, "in"),
                         pad_y = unit( 0.27, "in"),
                         style = north_arrow_fancy_orienteering())


ggsave(file = "Output/Graphs and Figures/ecoregion_map.png",
       dpi = 500,
       width = 5,
       height = 7.09)
num_colors <- 9

# Generate the palette
greens_palette <- c("white",(brewer.pal(num_colors, "Greens")[1:8]))

# Print the palette
print(greens_palette)

####Figure 3 ####

East_temp<-st_read("C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Data/Output/us_l3_ecoregions_ETF.shp")

sen_slope <- read_excel("C:/Users/micha/OneDrive - University of Florida/Chapter 1 Data Analysis/Obj 1 Trends in Woody Cover Over Time/Output/Data/mannkendall_sens.xlsx") %>% 
  select(NA_L3NAME, sens.slope) %>% 
  mutate(sens.slope = sens.slope) %>% 
  filter(NA_L3NAME != "L1 Eastern Temperate Forests") 

sen.slope.sf<-East_temp %>%
  arrange(NA_L3NAME)%>%
  left_join(sen_slope, by = "NA_L3NAME") 




sens.plot<-ggplot() + 
  geom_sf(data = sen.slope.sf,
          aes(fill = sens.slope),
          size = 1.5, color = "black") + 
  scale_fill_gradient(name = "Sen's Slope \n (% cover/year)",
                      low ="#edfcf1", 
                      high = "#03360A")+
  theme_bw()+
  coord_sf()+
  theme(axis.text = element_text(size = 6),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right", 
        legend.justification = "left",
        plot.margin = unit(c(0.4, 0.15, 0.2, 0.15), "cm"),
        #legend.spacing.x = unit(0.01,'cm'),
        legend.key.width = unit(0.25, "cm"), 
        legend.key.height = unit(0.25, "cm"),
        legend.text= element_text(size = 6),
        legend.title = element_text(hjust = 0.5,
                                    size = 7),
        legend.background = element_blank())+
  labs(tag = "A") + 
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = c(0.04,1.04))


ggsave(file = "Output/Graphs and Figures/sensslope_map.png",
       dpi = 500,
       width = 3.15,
       height = 3.15)

####Make a map of Odds Ratio ####

#bring in log odds data. 
log.odds<- read_xlsx("C:/Users/micha/OneDrive - University of Florida/Chapter 1 Data Analysis/Obj 2 Fire Occurrence by Woody Cover/Output/Data/glm_results.xlsx") %>% 
  arrange(NA_L3NAME)%>%
  filter(NA_L3NAME != "L1 Eastern Temperate Forest") %>% 
  select(NA_L3NAME,OR,p) 

#Find the quantiles
justsig<- log.odds %>% 
  filter(p < 0.05) 
notsig<-log.odds %>% 
  filter(p >= 0.05) %>% 
  mutate(Bins = "Insignificant")

quantile.breaks.odds<- quantile(justsig$OR)
justsig$OR %>% 
  sort()

#assign values that make more sense
breakpoints<-c(1.01,1.03,1.05,1.07,1.09,1.2)
#Make the breakpoint labels

breaks <- c("1.01 - 1.03",
            "1.03 - 1.05",
            "1.05 - 1.07",
            "1.07 - 1.09",
            "1.09 - 1.2") 
#Cut the data
justsig$Bins <- cut(justsig$OR, include.lowest=TRUE, breaks = breakpoints, labels = breaks)

log.odds<-rbind(justsig,notsig) %>% 
  select(NA_L3NAME,OR,Bins) %>% 
  mutate(Bins = as.character(Bins))

#Add NAs for the excluded regions. 
excluded_regions <- East_temp$NA_L3NAME[!(East_temp$NA_L3NAME %in% log.odds$NA_L3NAME)]
excluded_regions <-as.data.frame(excluded_regions) %>%
  mutate(OR = NA,
         Bins = "Excluded")%>%
  rename(NA_L3NAME = excluded_regions) %>% 
  select(NA_L3NAME,OR,Bins)

log.odds_joined <-rbind(log.odds,excluded_regions) %>% 
  mutate(Bins = factor(Bins,
         levels = c("1.01 - 1.03",
                    "1.03 - 1.05",
                    "1.05 - 1.07",
                    "1.07 - 1.09",
                    "1.09 - 1.2",
         "Insignificant",
         "Excluded")))

log.oddsmap<-East_temp %>% 
  left_join(log.odds_joined) 

  

####Plot Log Odds####
palette.red2 <- brewer.pal(5,"Reds")


#now plot it all
odds.plot<- 
  ggplot() + 
  geom_sf(data = log.oddsmap,
          aes(fill = Bins),
          size = 1.5, color = "black") + 
  scale_fill_manual(name = "Odds Ratio",
                    values = c(palette.red2,'darkgray',"white"),
                    labels = c("1.01 - 1.03",
                               "1.03 - 1.05",
                               "1.05 - 1.07",
                               "1.07 - 1.09",
                               "1.09 - 1.2",
                               "Insignificant",
                               "Excluded")) +
  theme_bw()+
  coord_sf()+
  
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 6),   
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.position = "right",
        legend.title = element_text(hjust = 0.5, size = 7),
        legend.text = element_text(size = 6),
        legend.key.width = unit(0.3, "cm"), 
        legend.key.height = unit(0.3, "cm"),
        legend.background = element_blank(),
        plot.margin = unit(c(0.4, 0.2, 0.2, 0.2), "cm"))+  # top, right, bottom, left)
  guides(fill = guide_legend(ncol = 1)) +   
  labs(tag = "B") + 
  theme(plot.tag = element_text(face = "bold"),
        plot.tag.position = c(0.04,1.04))        


ggsave(file = "Output/Graphs and Figures/fireodds_map2.png",
       dpi = 500,
       width = 3.15,
       height = 3.15)


#Plot a single region for a powerpoint
ggplot() + 
  geom_sf(data = (log.oddsmap %>% 
                    filter(NA_L3NAME == "Ridge and Valley")),
                    fill = "#FB6A4A",
          size = 1.5, color = "black") + 
  theme_void()+
  coord_sf()



ggsave(file = "Output/Graphs and Figures/single_region/ouachita_mountains.png",
       dpi = 500,
       width = 3.15,
       height = 3.15)

ggsave(file = "Output/Graphs and Figures/single_region/southeastern_plains.png",
       dpi = 500,
       width = 3.15,
       height = 3.15)
ggsave(file = "Output/Graphs and Figures/single_region/ridge_and_valley.png",
       dpi = 500,
       width = 3.15,
       height = 3.15)



####Plot Both Together ####

two.plots<- plot_grid(sens.plot, 
                      odds.plot, 
                      nrow = 2,
                      align = 'v', axis = 'l')
ggsave(two.plots, file = "Output/Graphs and Figures/Figure2.png",
       dpi = 500,
       width = 3.15, 
       height = 4)



