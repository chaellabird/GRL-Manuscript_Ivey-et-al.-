#Michaella Ivey
#The purpose of this script is to examine changes in woody cover across L1 and 
#L3 ecoregions of the Eastern Temperate Forests.
#There are two statistical tests, Modified Mann Kendall and Sen's Slope. 
#The outputs are a series of jpgs depicting acf plots for each region,
  #a table presenting the stats from the mmk and the sen's slope,
  #and a graph of percent woody cover over time.
#It also outputs a .xlsx file with the results of the mmk and sen's slope. 
  #It is used in the script that makes a map of sen's slope across the region.

####Load Packages####
library(pacman)
pacman::p_load(ggplot2,dplyr, mgcv,modifiedmk,lubridate,
               openxlsx, readxl,tibble, gt,trend, Kendall,scales,flextable,writexl,sf)

####Load Data####
#Create an extra row for the key that represents the L1 region.
  extra_row<-data.frame(L3CODE = 0,
                          NA_L3NAME = "L1 Eastern Temperate Forests")
#Create a key that has both L3 code and NA_L3NAME
  key<-st_read("C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Data/Data/us_eco_l3/us_eco_l3.shp") %>% 
    st_drop_geometry() %>%
    filter(NA_L1CODE == 8) %>% 
    mutate(L3CODE = as.numeric(US_L3CODE)) %>% 
    select(-c(NA_L3CODE,US_L3NAME,NA_L2CODE,NA_L2NAME,
              NA_L1NAME,NA_L1CODE,L2_KEY,L1_KEY,US_L3CODE,
              Shape_Leng,Shape_Area,L3_KEY)) %>%
    bind_rows(extra_row) %>% 
    unique()
#Produce the factor order of all the ecoregions
  factor.order<- c("L1 Eastern Temperate Forests",
                   "Acadian Plains and Hills",
                   "Arkansas Valley",
                   "Atlantic Coastal Pine Barrens",
                   "Blue Ridge",
                   "Boston Mountains",
                   "Central Appalachians",
                   "Central Corn Belt Plains",
                   "Driftless Area",
                   "East Central Texas Plains",
                   "Eastern Corn Belt Plains",
                   "Eastern Great Lakes Lowlands",
                   "Erie Drift Plain",
                   "Huron/Erie Lake Plains",
                   "Interior Plateau",
                   "Interior River Valleys and Hills",
                   "Middle Atlantic Coastal Plain",
                   "Mississippi Alluvial Plain",
                   "Mississippi Valley Loess Plains",
                   "North Central Hardwood Forests",
                   "Northeastern Coastal Zone",
                   "Northern Allegheny Plateau",                     
                   "Northern Piedmont",
                   "Ouachita Mountains",                             
                   "Ozark Highlands",
                   "Piedmont",
                   "Ridge and Valley",
                   "South Central Plains",
                   "Southeastern Plains",
                   "Southeastern Wisconsin Till Plains",
                   "Southern Coastal Plain",
                   "Southern Michigan/Northern Indiana Drift Plains",
                   "Southwestern Appalachians",
                   "Western Allegheny Plateau")


#load the original rap csv files
  shrub<- read.csv("Data/L3_Evaluated_Hectares_Shrubs.csv")
  shrubandtree<-read.csv("Data/L3_Evaluated_Hectares_Trees.csv") %>% 
    left_join(shrub) %>% 
    mutate(woody_cover_ha = SHR_Hectares + Tree_Hectares)

#bring in L3 areas in hectares
 l3_woody_cover<- read.csv("Data/L3_evaluated_total_hectares.csv") %>% 
   left_join(shrubandtree) %>% 
   mutate(prct_woody = woody_cover_ha/Evaluated_Hectares)

#in this case the L1 values are truly the sum of the L3, because we are not excluding regions. 
 L1<-l3_woody_cover %>%
   group_by(Year) %>% 
   summarize(prct_woody = sum(woody_cover_ha)/sum(Evaluated_Hectares),
             Evaluated_Hectares = sum(Evaluated_Hectares)) %>% 
   mutate(L3CODE = 0)
  colnames(l3_woody_cover)
  colnames(L1)
  woody_cover<-l3_woody_cover %>% 
    select(Year,prct_woody,L3CODE,Evaluated_Hectares) %>% 
    bind_rows(L1) %>% 
    left_join(key) %>% 
    arrange(Year) %>% 
    mutate(NA_L3NAME = factor(NA_L3NAME, 
                              levels = factor.order))

#Clean up the environment a tad   
  rm(shrubandtree,shrub,L1,extra_row,l3_woody_cover) 


  
  
  
####Woody cover over time statistical tests####


 ####Check for autocorrelation####

#Fix the names. the / is screwing up the directory creation. 
  woody_cover_name_corrected <- woody_cover %>% 
      mutate(NA_L3NAME = case_when(NA_L3NAME == "Huron/Erie Lake Plains" ~ "Huron-Erie Lake Plains",
             NA_L3NAME == "Southern Michigan/Northern Indiana Drift Plains" ~ "Southern Michigan-Northern Indiana Drift Plains",
             TRUE ~ NA_L3NAME))
  
  output_folder<-"Output/Graphs and Figures/Diagnostic Plots/"
  
  list<-woody_cover_name_corrected %>% 
    group_by(NA_L3NAME) %>% 
    group_split()
    
  lapply(list,function(x){
    region <-x$NA_L3NAME[1]
    output_file <-paste0(output_folder,
                         "acf_plot_",
                         gsub(" ", "_", region),
                         ".jpg") 
# Open a JPEG device
  jpeg(output_file, width = 5, 
       height = 5, units = "in", res = 300)
  acf(x$prct_woody,
      plot = TRUE,
      main = region)

  # Close the JPEG device
  dev.off()

})  
  

####Not sure what the purpose of this is, but don't want to delete it.####
#acf_df <- lapply(names(wood_acf), function(group_name) {
 # data.frame(
 #   lag = wood_acf[[group_name]]$lag,
  #  acf = wood_acf[[group_name]]$acf,
   # group = group_name
  #)
#}) %>%
#  bind_rows()

# Calculate number of observations (assuming all groups have same number of observations)
#df<-shrub3 %>%
#  subset(select = c(NA_L3NAME,Year)) %>% 
#  group_by(NA_L3NAME) %>% 
#  mutate(n = length(Year)) %>%
#  mutate(group = NA_L3NAME) %>% 
 # subset(select = c(group,n)) %>% 
 # unique()
#acf_df<-acf_df %>% 
#  left_join(df, by = "group") %>% 
 # mutate(n = as.numeric(n))


####modified Mann Kendall and Sen's Slope ####
  list<-woody_cover %>% 
    arrange(Year) %>%
    mutate(prct_woody = prct_woody*100) %>%  #make fractional cover percent cover
    split(.$NA_L3NAME) 
#Perform the Modified Mann Kendall    
  mmkh_list<-lapply(list,function(x){
    mmkh(x$prct_woody)
  })
  names(mmkh_list) <- names(list)
  Mann_kendall_east_temp<-as.data.frame(mmkh_list, check.names = FALSE) %>% 
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    select(c("rowname","Tau","new P-value")) %>% 
    rename(NA_L3NAME = "rowname",
           Variance.corrected.P.value = "new P-value")
#Perform the Sen's Slope Calculation 
  sens.table <- lapply(list, function(x) {
    sens.slope(x[, "prct_woody"])}) 
  
  sens.table <- lapply(names(sens.table), function(group_name) {
    data.frame(
      sens.slope = sens.table[[group_name]]$estimates,
      sens.p.value = sens.table[[group_name]]$p.value,
      NA_L3NAME = group_name
    )
  }) %>%
    bind_rows() %>% 
    `rownames<-`(NULL)


  Mann_Kendall_results<-left_join(Mann_kendall_east_temp,sens.table)

#Save dataframe to excel file. This will preserve the NA_L3NAME.
  write_xlsx(Mann_Kendall_results, "Output/Data/mannkendall_sens.xlsx")

#### Make a lovely table for the manuscript ####
#Add some additional columns representing initial cover, ending cover, relative
#change, and the all around 30 year mean
  added_columns<-woody_cover %>% 
    group_by(NA_L3NAME) %>% 
    summarize(Area = unique(Evaluated_Hectares), 
              Cover.1990 = round(prct_woody[Year == 1990],3)*100,
              Cover.2020 = round(prct_woody[Year == 2020],3)*100,
              Relative.increase = round((prct_woody[Year == 2020] - prct_woody[Year == 1990])/ prct_woody[Year == 1990],3)*100,
              year_30_mean = round(mean(prct_woody),3)*100) 
  colnames(Mann_Kendall_results)
  Mann_Kendall_results<-left_join(Mann_Kendall_results,added_columns) %>% 
    mutate(Variance.corrected.P.value = formatC(Variance.corrected.P.value,
                                                format = "e",
                                                digits = 1),
           Tau = round(Tau, 3),
           sens.slope = round(sens.slope,3),
           sens.p.value = formatC(sens.p.value,
                                        format = "e",
                                        digits = 1),
           Area = round(Area,1)) %>% 
    mutate(NA_L3NAME = factor(NA_L3NAME, 
                              levels = factor.order))



#Create a table using flextable
  colnames(Mann_Kendall_results)
    large_cover_table <- Mann_Kendall_results %>%
      select(c(NA_L3NAME,
               Area,
               Cover.1990,
               Cover.2020,
               year_30_mean,
               Relative.increase,
               Tau,
               Variance.corrected.P.value,
               sens.slope,
               sens.p.value)) %>% 
      flextable() %>%
      set_header_labels(.,
                        NA_L3NAME           = "Ecoregion",
                        Area                = "Area (ha)",
                        Cover.1990          = "1990",
                        Cover.2020          = "2020",
                        year_30_mean        = "30 Year Mean",
                        Relative.increase   = "Relative increase",
                        Tau                 = "Tau",
                        Variance.corrected.P.value       = "Tau P value",
                        sens.slope          = "Sen's Slope",
                        sens.p.value  = "Sen's Slope P Value"
                        ) %>%
      set_formatter(Variance.corrected.P.value = function(x){
        formatC(x,format = "e", digits = 1)}) %>%
      set_table_properties(layout = "autofit") %>%
      width(., j = c(2:7), width = 1.5) %>%
      align(., align = "center", part = "all")  
    
    
    save_as_docx(file = large_cover_table,
                 path = "Output/Graphs and Figures/Table1.docx")

  
  
  
####Visualize!####

  l3graph<-woody_cover %>% 
    mutate(prct_woody = prct_woody*100) %>% 
    ggplot(mapping = aes(Year,prct_woody))+
    geom_smooth(color='darkgreen')+
    ylab("Percent Woody Cover")+
    facet_wrap(~NA_L3NAME,
               scales='free',
               nrow = 5,
               labeller = label_wrap_gen(width = 22))+
    theme(strip.text = element_text(size = 6,
                                    color = "black")) +
    theme_bw(base_size = 6)+
    scale_y_continuous(labels = scales::label_number(accuracy =1))
    theme(axis.title.x = element_text(size = 8))+
    theme(axis.title.y = element_text(size = 8))+
    theme(axis.text.y = element_text(size = 6))+ 
    theme(axis.text.x = element_text(size = 6))
  
  ggsave(l3graph,file = "Output/Graphs and Figures/Figure1.jpg",
         dpi = 500,
         width = 6.2, 
         height = 5) 
  
  
  
  
  
  
  
  
  
  