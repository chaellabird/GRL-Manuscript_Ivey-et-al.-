#load packages
library(pacman)
pacman::p_load(tidyverse, sf,terra,readxl)

#load data

fire_info<-read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Data/Output/fires_Eastern_temperate_forest.xlsx') %>% 
  group_by(US_L3CODE) %>% 
  summarize(fire_radius = sqrt((mean(area_ha)*10000)/pi)) #get mean area, convert to square m, /divide by pi.
#i'm missing 3 regions that must have had NO fires. I'll start off by excluding them from the L1 calculation. 

ecoregions<-st_read('Data/us_eco_l3/us_eco_l3.shp') %>% 
  filter(NA_L1CODE == 8) %>% 
  group_by(US_L3CODE) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  filter(US_L3CODE %in% fire_info$US_L3CODE) %>% 
  left_join(fire_info)



# Function to create buffered points
create_buffered_points <- function(polygon, buffer_size, n_points = 1000) {
  # Create random points within the polygon
  points <- st_sample(polygon, size = n_points, type = "random")
  # Buffer the points by the average wildfire size
  buffered_points <- st_buffer(points, dist = buffer_size)
  buffered_points <- st_as_sf(buffered_points, crs = st_crs(polygon)) 
  buffered_points$US_L3CODE = polygon$US_L3CODE
   return(buffered_points)
}

# Apply the function to each ecoregion and store results in a list
buffered_points_list <- lapply(1:nrow(ecoregions), function(i) {
  create_buffered_points(ecoregions[i, ], ecoregions$fire_size[i])
})

# Combine all buffered points into a single sf object
buffered_points <- do.call(rbind, buffered_points_list) %>% 
  mutate(TARGET_FID = seq(from = 1, to = 30000, by = 1))



st_write(buffered_points,'Outputs/unsplit_buffered_points.shp',
         delete_dsn = TRUE) 


plot(st_geometry(ecoregions), col = 'lightblue', border = 'black')
plot(st_geometry(buffered_points), col = 'red', add = TRUE)
 
####L1 Eastern Temperate Forests Scale####

fire_info<-read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Data/Output/fires_Eastern_temperate_forest.xlsx') %>% 
  summarize(fire_radius = sqrt((mean(area_ha)*10000)/pi)) #get mean area, convert to square m, /divide by pi.
 

L1_ETF<-st_read('Data/us_eco_l3/us_eco_l3.shp') %>% 
  filter(NA_L1CODE == 8) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  cbind(fire_info)



# Function to create buffered points
create_buffered_points <- function(polygon, fire_radius, n_points = 10000) {
  # Create random points within the polygon
  points <- st_sample(polygon, size = n_points, type = "random")
  # Buffer the points by the average wildfire size
  buffered_points <- st_buffer(points, dist = fire_radius)
  buffered_points <- st_as_sf(buffered_points, crs = st_crs(polygon)) 
  return(buffered_points)
}

L1_buffered_points<- create_buffered_points(L1_ETF,L1_ETF$fire_radius)%>% 
  mutate(Event_ID = row_number())

st_write(L1_buffered_points,'Outputs/L1sampledpoints_1000.shp',
         delete_dsn = TRUE) 


####L2 Buffer Points####
fire_info_L2<-read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Data/Output/fires_Eastern_temperate_forest.xlsx') %>% 
  group_by(NA_L2NAME) %>% 
  summarize(fire_radius = sqrt((mean(area_ha)*10000)/pi)) #get mean area, convert to square m, /divide by pi.
  


L2_ETF<-st_read('Data/us_eco_l3/us_eco_l3.shp') %>% 
  filter(NA_L1CODE == 8) %>%
  group_by(NA_L2NAME) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  left_join(fire_info_L2)

# Function to create buffered points
create_buffered_points <- function(polygon, buffer_size, n_points = 2000) {
  # Create random points within the polygon
  points <- st_sample(polygon, size = n_points, type = "random")
  # Buffer the points by the average wildfire size
  buffered_points <- st_buffer(points, dist = buffer_size)
  buffered_points <- st_as_sf(buffered_points, crs = st_crs(polygon)) 
  buffered_points$NA_L2NAME = polygon$NA_L2NAME
  return(buffered_points)
}

# Apply the function to each ecoregion and store results in a list
L2_buffered_points <- lapply(1:nrow(L2_ETF), function(i) {
  create_buffered_points(L2_ETF[i, ], L2_ETF$fire_radius[i])
})

# Combine all buffered points into a single sf object
L2_buffered_points <- do.call(rbind, L2_buffered_points) %>% 
  mutate(Event_ID = row_number())


  



st_write(L2_buffered_points,'Outputs/L2sampledpoints.shp',
         delete_dsn = TRUE) 






# Apply the function to each ecoregion and store results in a list
buffered_points_list <- lapply(1:nrow(ecoregions), function(i) {
  create_buffered_points(ecoregions[i, ], ecoregions$fire_size[i])
})

# Combine all buffered points into a single sf object
buffered_points <- do.call(rbind, buffered_points_list) %>% 
  mutate(TARGET_FID = seq(from = 1, to = 30000, by = 1))




