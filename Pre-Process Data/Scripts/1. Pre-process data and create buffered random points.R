####Load packages####
pacman::p_load(tidyverse, sf, terra,
               tidyterra, ggspatial,
               lwgeom,writexl)
sf_use_s2(FALSE)

#I need to create a file that has info on which regions touch which Event_IDs. 
#Then, I need to filter the MTBS base shpfile by it. 


####Load Data####
mtbs <-
  read_sf("Data/mtbs_perimeter_data/mtbs_perims_DD.shp") %>% 
  mutate(year = year(Ig_Date), 
         area_ha = as.numeric(st_area(.) / 10000)) %>%
  group_by(Event_ID) %>%
  mutate(area_ha = sum(area_ha)) %>% ungroup %>%
  filter(Incid_Type == "Wildfire",
         area_ha >=200, #filter out all fires smaller than 200 ha
         year >= 1991 & year <= 2021) %>% 
  select(Event_ID,Ig_Date,geometry,area_ha)%>% vect()


etf <-
  st_read("Data/us_eco_l3/us_eco_l3.shp") %>%
  filter(NA_L1CODE == 8) %>% vect()



####Intersect MTBS with ETF####
if (!identical(crs(etf), crs(mtbs))) {
  mtbs <- project(mtbs, crs(etf))
}

# Intersect mtbs with etf
intersection_result <- intersect(mtbs, etf)

####Save these as new polygons####
mtbs<-st_as_sf(intersection_result) %>% 
  #mutate(burned_area = as.numeric(st_area(.) / 10000)) %>% 
  #filter(burned_area >=100) %>% 
  select(-c("L2_KEY","L1_KEY",
            "Shape_Leng","Shape_Area",
            "NA_L1CODE","NA_L1NAME","L3_KEY","US_L3NAME"))


east_temp_fire<-mtbs %>% 
  st_drop_geometry() 
write_xlsx(east_temp_fire,"Output/fires_Eastern_temperate_forest.xlsx")

#Bring in the MTBS file
MTBS<-st_read("Data/mtbs_perimeter_data/mtbs_perims_DD.shp") %>% 
  filter(Event_ID %in% east_temp_fire$Event_ID)


st_write(MTBS,"Output/Unaltered_MTBS_ETF_Limited.shp")


####Package the ETF away for future mapping ####
just_etf<-st_as_sf(etf) %>%
  select(-c("L2_KEY","L1_KEY",
            "Shape_Leng","Shape_Area",
            #"NA_L2CODE",  "NA_L2NAME",
            "NA_L1CODE","NA_L1NAME")) %>% 
  mutate(poly_id = row_number()) %>%
  group_by(NA_L2CODE,NA_L2NAME, NA_L3NAME) %>% 
  #group_by(NA_L3NAME,US_L3CODE,NA_L3CODE,L3_KEY) %>% 
  summarize(geometry = st_union(geometry))


st_write(just_etf,'Output/us_l3_ecoregions_ETF.shp')  




####Create Random Buffers###



fire_info<-east_temp_fire %>% 
  group_by(US_L3CODE) %>% 
  summarize(fire_radius = sqrt((mean(area_ha)*10000)/pi)) #get mean area, convert to square m, /divide by pi.

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

fire_info<-east_temp_fire %>% 
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

