#Michaella Ivey
#The purpose of this script is to run a generalized linear model for presence/
 #absence of fires ~woody cover. 
#Its outputs are a .xlsx with the statistical output,
 #a pretty table in a .doc for use in the manuscript
 #and a graph of fire probability over woody cover for the supp file.  



#Load packages
  library(pacman)
  pacman::p_load(tidyverse, readxl, knitr, 
                 grid, stringdist, writexl, ggdist, 
                 gt,flextable,car,questionr)


####Make an ecoregion key####

key<-read_excel("C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Data/Output/fires_Eastern_temperate_forest.xlsx") %>% 
  select(US_L3CODE,NA_L3NAME,NA_L3CODE)%>%
  mutate(L3CODE = as.numeric(US_L3CODE)) %>% 
  select(-c(US_L3CODE,NA_L3CODE)) %>% 
  unique() 

write_xlsx(key, "Output/Data/ecoregion_key.xlsx")
 

####Generalized linear model ####

#Bring in Presence/Absence Data
L3<-read_csv("Output/Data/Presence_Absence_data.csv") %>% 
  left_join(key)



#add an extra subset for the L1 data.
  pa<-read_excel('Output/Data/L1_Presence_Absence_data.xlsx') %>%
    rbind(L3) %>%
    mutate(PA = as.factor(PA))

  
  
#derive an Odds ratio that makes intuitive sense. This makes a nice dataframe
  glm.firepa.stats<- pa %>%
  mutate(prct_woody = 100*round(fractional_cover, digits = 4)) %>% 
  group_by(NA_L3NAME) %>%
  nest() %>% 
  mutate(glm = map(data, ~ glm(PA ~ prct_woody,
                               family = binomial(link = "logit"), data = .x)),#this part is fine. It's the odds ratio that is broken.
         OR = map(glm, odds.ratio)) %>% 
  unnest(OR) %>% 
  ungroup %>% 
  select(NA_L3NAME, OR, "2.5 %", "97.5 %", p) %>%
  slice(., seq(2, nrow(.), 2))


write_xlsx(glm.firepa.stats, "Output/Data/glm_results.xlsx")

####Make a lovely Table of stats for a manuscript####  

colnames(glm.firepa.stats)
large_cover_table <- glm.firepa.stats %>%
  arrange(desc(OR)) %>% 
  mutate(OR = round(OR,3),
         `2.5 %`= round(`2.5 %`,3),
         `97.5 %`=round(`97.5 %`,3))%>% 
  flextable() %>%
  set_header_labels(.,
                    NA_L3NAME           = "Ecoregion",
                    `2.5 %`               = "2.5%%",
                    `97.5 %`              = "97.5 %",
                    p                    = "P value") %>%
  set_formatter(p = function(x){
    formatC(x,format = "e", digits = 1)}) %>%
  set_table_properties(layout = "autofit") %>%
  width(., j = c(1:5), width = 1.5) %>%
  align(., align = "center", part = "all")  


save_as_docx(file = large_cover_table,
             path = "Output/Graphs and figures/glmstats.docx")


####Convert Log Odds to Probability and graph####

#this one will give you a list of models you can do stuff with.
result <- pa  
result<- split(result, result$NA_L3NAME)
glm.firepa.models<-lapply(result, function(y){
  glm(PA ~ fractional_cover, data = y, family = binomial(link = "logit"))
})



#Create factor order 
  factor.order<- c("L1 Eastern Temperate Forests",
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
                   "Western Allegheny Plateau")
  
  

#Create some model data with which to predict.
  Mydata<- data.frame(fractional_cover = seq(from = 0.01, to = 1, by = 0.01))
  Mydata.replicated <- Mydata %>% 
    slice(rep(1:n(), times = 20)) %>%
    mutate(NA_L3NAME = rep(factor.order, each = 100))
  
  
# Predict using each model in the list for the new data
  predictions <- lapply(glm.firepa.models, function(model) {
  predict(model, newdata = Mydata, type = "response")
  })

# Combine vectors into a single column
  pred <- data.frame(
    Value = unlist(predictions),
    Group_Name = rep(names(predictions), sapply(predictions, length)),
    row.names = NULL
  )

#Create line data for each one 
  linedata <-as.data.frame(cbind(Mydata,pred))
  colnames(linedata)<-c("Mydata","pred","NA_L3NAME") 
  linedata <-cbind(Mydata.replicated,pred) %>%
    select(-Group_Name) %>% 
    rename(Prediction = "Value") %>% 
    mutate(NA_L3NAME=factor(NA_L3NAME, 
                            levels = factor.order))
  
#Make PA numeric.
  pa<- pa %>%
    mutate(PA = as.numeric(as.character(PA)))

  p.values<-glm.firepa.stats %>%
     select(NA_L3NAME,p)%>%
     mutate(p.value = signif(p, digits = 3))%>%
     mutate(labels = paste("p", p.value, sep = " = ")) %>% 
    select(-c(p,p.value))
 
  pa<-merge(pa,p.values, by = "NA_L3NAME") %>% 
    mutate(NA_L3NAME = factor(NA_L3NAME, 
                       levels = factor.order))
  str(pa)
#Plot all the probabilities
  probabilities<-ggplot(pa, aes(x = fractional_cover, y = PA))+
   scale_shape_identity()+
  facet_wrap(~NA_L3NAME, nrow = 5)+
    geom_point(shape = 108, size = 4)+ #108
    xlab("Percent Woody Cover")+
    ylab("Probability")+
    geom_line(data = linedata, 
              aes(x = fractional_cover, y = Prediction, 
                  group = "NA_L3NAME"), 
              color = "red",
              linewidth = 1.5)+
    theme_bw(base_size = 20)+
  geom_text(data = pa,aes(x=.15, y=.75, label=labels), fontface='bold', size=4)
    

  
  ggsave(probabilities, file = "Output/Graphs and Figures/Figure S2 Log_odds_converted_to_probabilty.png",
  dpi = 500,
  width = 14.5,
  height = 10)



 


  
  
