#Michaella Ivey
#You don't have to worry about this one. Move on to script 4. 


#Load Packages
library(pacman)

pacman::p_load(ggdist,tidyverse,DHARMa,hnp,readxl,
               gridExtra,vcdExtra,performance,broom)


#Data
key<-read_excel("Output/Data/ecoregion_key.xlsx")# %>% 
 # mutate(L3CODE = as.numeric(US_L3CODE))
pa<-read_csv("Output/Data/UnsplitPresence_Absence_data.csv") %>% 
  mutate(PA = as.factor(PA),
         prct_woody = fractional_cover*100) %>% 
  left_join(key)

output_folder<-"Output/Graphs and Figures/Diagnostic_plots/"

list<-pa %>%
  group_by(NA_L3NAME)%>% 
  group_split

lapply(list,function(x){
  region <- x$NA_L3NAME
  model<-glm(PA ~ prct_woody, data = x, family = binomial)
  output_file <- paste0(output_folder,
                        "diagnostic_plot_",
                        gsub(" ", "_", region),
                        ".jpg") 
  # Open a PNG device
  png(filename = output_file, width = 6, height = 6, units = "in", res = 500)
  par(mfrow = c(3, 2))
  
  plot(x = model,
       which = 1)#residuals by fitted
  mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
       which = 2)#QQplot
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
       which = 3)
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
    which = 4)
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
  which = 5)
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
   which = 6)#Cook's leverage
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 1.2)
  
  dev.off()
  })


####Up to here good####
#I'm not sure of the usefulness of everything below this point. 




####calculate McFadden's R-squared for model####

#for all models

McFadden.rsquared <- lapply(glm.firepa, function(z){
  with(summary(z), 1 - deviance/null.deviance)
}) %>% 
  as.data.frame(check.names = FALSE) %>% 
  t() 
NA_L3NAME <- rownames(McFadden.rsquared)
rownames(McFadden.rsquared) <- NULL
McFadden.rsquared <- as_tibble(cbind(NA_L3NAME,McFadden.rsquared)) %>% 
  rename("McF.R2" = "V2")

#another way to calculate, using the log likelihood function. 
  #this eliminates the out of range values, but doesn't improve the small ones.

logLikmod<- lapply(glm.firepa, function(z){
  logLik(z)})%>% 
  as.data.frame(check.names = FALSE) %>% 
  t() 
NA_L3NAME <- rownames(logLikmod)
rownames(logLikmod) <- NULL
logLikmod <- as_tibble(cbind(NA_L3NAME,logLikmod)) %>% 
rename("logLik" = "V2")%>% 
  mutate(logLik = as.numeric(logLik))

logLikNull<-lapply(null.Mod, function(z){
  logLik(z) })  %>% 
  as.data.frame(check.names = FALSE) %>% 
  t() 
NA_L3NAME <- rownames(logLikNull)
rownames(logLikNull) <- NULL
logLikNull <- as_tibble(cbind(NA_L3NAME,logLikNull)) %>% 
  rename("logLik" = "V2") %>% 
  mutate(logLik = as.numeric(logLik))
McFadden.rsquared.way2<- as_tibble(1-(logLikmod$logLik/logLikNull$logLik))  %>% 
  mutate(NA_L3NAME = as.character(logLikmod$NA_L3NAME))




####Tjur Psuedo R squared.####
r2_tjur(Arkansas_Valley)
Tjur <- lapply(glm.firepa, function(z){
  r2_tjur(z)}) %>% 
  as.data.frame(check.names = FALSE) %>% 
  t() 
NA_L3NAME <- rownames(Tjur)
rownames(Tjur) <- NULL
Tjur <- as_tibble(cbind(NA_L3NAME,Tjur)) 

#NOTE: This method got the same issue of the Mississippi Valley Loess Plains
   #and Ouachita mountains having out of range values
#Tjur is no better.



####Diagnostic Plots####
#1 Residual vs fitted plot
#2. QQ plot
#3. Scale location plot
#4. Cook's Distance
#5. Leverage plot
#6. Cook's distance vs leverage  

####Arkansas Valley ####
summary(Arkansas_Valley)
vcdExtra::LRstats(object = Arkansas_Valley)
#the pr(>chisq is 0.002306), which is honestly terrible.Indicates a lack of fit.
plot(x = Arkansas_Valley,
     which = c(4,6), #this will give us cook's distance and leverage
     col =pa$PA)
   
#residual visualization and QQ plot.
#this should identify extreme residuals
  plot(x = Arkansas_Valley,
       which = c(1,2),
       col = pa$PA)
#some bending at ends of residual vs fitted plot.
#BUT acceptable for logistic
#qqplot will bend off at ends and that's okay.

#the scale location plot also identifies extreme residuals
#which may need to be omitted or modified. 
plot(x = Arkansas_Valley,
     which = 3,
     col = pa$PA)
  
#now cook's distance plot. This tells us if certain points are
   #disproportionately affecting the model
plot(x= Arkansas_Valley,
     which = 4,
     col = pa$PA)
#so for this one there'es like 5-10 points
#highly influencing the model. 

#Leverage plot.
plot(x = Arkansas_Valley,
     which = 5,
     col = pa$PA)
#the leverage plots bend like the St Louis Arch.
#curvature can mean the model needs a 
#higher degree or a transfomration

  #degree 
#Cook's distance vs leverage plot
plot(x = Arkansas_Valley,
     which = 6,
     col = pa$PA)
#partial residual by component can be used 
   #for when has multiple predictors. 

####Atlantic Coastal Pine Barrens####
plot(x = glm.firepa$`Atlantic Coastal Pine Barrens`,
     which = c(1:6),
     col = pa$PA)
vcdExtra::LRstats(object = glm.firepa$`Atlantic Coastal Pine Barrens`)

  #1. residuals bend, but that's typical for logistic
  #2 qq plots have a few outliers in the upper quantile ranges.
  #3. the scale location plot notes the same 5 outliers
  #4. The cook's distance implicates those five outliers again
  #5. curvature of leverge could indicate need for transformation. 
  #6. Cook's distance vs leverage? I don't yet understand this diagnostic. 
#pr(>chisq = 0.11), which is better

####Blue Ridge ####
vcdExtra::LRstats(object = glm.firepa$`Blue Ridge`)
#veeery bad pr(>chisq) 5.98 3-05
plot(x = glm.firepa$`Blue Ridge`,
     which = c(1:6),
     col =pa$PA)
  #1 residuals are mostly straight, but have big gaps.That could be a problem
  #2 QQs take a shift and go up above the straight line.That means a lot of outliers in upper ranges
  #3. Scale location plot has those outliers all clumped together.
  #4. Cook's distance is a mess There's a lot of good points, but also a lot of points pulling more than their weight
  #5. The leverage plots bend back early on.
  #6. 

####Boston Mountains ####
vcdExtra::LRstats(object = glm.firepa$`Boston Mountains`)
#fairly small prchisq. 0.009
plot(x = glm.firepa$`Boston Mountains`,
     which = c(1:6),
     col =pa$PA)
#1. Residuals bend  downward, but that's okay. Quite a few outliers. 
#2 qqs great except for five outliers above 2 theoretical quantiles
#3.Scale location shows those same five outliers
#4.Cook's distance shows the same five outliers affecting the model disproportionately.
#5. Leverage plots bend back. BUT the outliers are a bigger deal.
#6.
####Central Appalachians ####
vcdExtra::LRstats(object = glm.firepa$`Central Appalachians`)
#3.315e-08 prchisq
plot(x = glm.firepa$`Central Appalachians`,
     which = c(1:6),
     col =pa$PA)
  #1. residuals bend. That's fine. Four outliers.
  #2. Qqs have those four outliers, plus a few minorly outlying.
  #3. Scale location points out the outliers again.
  #4. As does the Cook's distance plot
  #5. leverage plots bend backward. Plus those four outliers hanging out in the corner
  #6. Box tidwell package exists find it. 

####East Central Texas Plains. ####
vcdExtra::LRstats(object = glm.firepa$`East Central Texas Plains`)
#prchisq  0.1825 
plot(x = glm.firepa$`East Central Texas Plains`,
     which = c(1:6),
     col =pa$PA)
  #1.The residuals are sparse. Maybe 2 outliers. 
  #2. 4 outliers in QQ plot. 
  #3. Scale location shows 4 outliers, but they aren't clustered together.
  #4. Cook's distance shows 4. This is a smaller dataset. 
  #5. the residual vs leverage plots are good and random. Those four outliers again.
  #6.
easttexas<-augment(glm.firepa$`East Central Texas Plains`)
easttexas.excluded<-easttexas %>% 
  subset(.cooksd < 0.1)
#excludes four observations
east.texas.excluded<-glm(PA ~ prct_woody, data = easttexas.excluded, family = binomial)
vcdExtra::LRstats(object = east.texas.excluded)


####Interior Plateau####
vcdExtra::LRstats(object = glm.firepa$`Interior Plateau`)
#prchisq  0.2186. That's actually not bad.
plot(x = glm.firepa$`Interior Plateau`,
     which = c(1:6),
     col =pa$PA)
  #1. residuals seem decent. 2 offensive outliers.
  #2.2 outliers on qqs, maybe 3 if you squint.
  #3. Scale location fine. two or three outliers.I say 2.
  #4. Cook's distance two major affectors
  #5.leverage plots look good.
  #6. 

ip.cooks<-as.data.frame(cooks.distance(glm.firepa$`Interior Plateau`)) %>% 
  rename(cooks = "cooks.distance(glm.firepa$`Interior Plateau`)") %>% 
  rownames_to_column(var = "object_id") %>% 
  subset(cooks > 0.2) #4/N being the cutoff. But that doesn't remove any outliers.

pa.ip<-pa %>% 
  filter(NA_L3NAME == "Interior Plateau") %>% 
  filter(object_id %in% ip.cooks$object_id) 

#now run the model with the subset data
pa.ip.cooks.glm<-glm(PA ~ prct_woody, data = pa.ip, family = binomial)
summary(pa.ip.cooks.glm)
summary(glm.firepa$`Interior Plateau`)
vcdExtra::LRstats(object = pa.ip.cooks.glm)
#for this region, removing outliers above 0.01 improves the VCD.
#with outliers above 0.007 removed, the prchisq gets even better! 0.68. That's a legit one!
plot(x = pa.macp.cooks.glm,
     which = c(1:6),
     col =pa$PA)





####Middle Atlantic Coastal Plain ####
vcdExtra::LRstats(object = glm.firepa$`Middle Atlantic Coastal Plain`)
#prchisq  0.000376. Not fantastic.
plot(x = glm.firepa$`Middle Atlantic Coastal Plain`,
     which = c(1:6),
     col =pa$PA)
  #1.residuals are fine
  #2. QQs go off crazy in the upper quantiles. This indicats a LOT of outliers
  #3. Scale location plot looks normal
  #4. The cook's plot looks almost wo tiered, with close to 30 in the upper.
  #5. leverage plots bend backward.


macp.cooks<-as.data.frame(cooks.distance(glm.firepa$`Middle Atlantic Coastal Plain`)) %>% 
  rename(cooks = "cooks.distance(glm.firepa$`Middle Atlantic Coastal Plain`)") %>% 
  rownames_to_column(var = "object_id") %>% 
  subset(cooks > 0.007) #4/N being the cutoff. But that doesn't remove any outliers.

pa.macp<-pa %>% 
  filter(NA_L3NAME == "Middle Atlantic Coastal Plain") %>% 
  filter(object_id %in% macp.cooks$object_id) 

#now run the model with the subset data
pa.macp.cooks.glm<-glm(PA ~ prct_woody, data = pa.macp, family = binomial)
summary(pa.macp.cooks.glm)
summary(glm.firepa$`Middle Atlantic Coastal Plain`)
vcdExtra::LRstats(object = pa.macp.cooks.glm)
#for this region, removing outliers above 0.01 improves the VCD.
#with outliers above 0.007 removed, the prchisq gets even better! 0.68. That's a legit one!
plot(x = pa.macp.cooks.glm,
     which = c(1:6),
     col =pa$PA)

#removing 4/N outliers leaves SO many outliers left in the model. 






####Mississippi Valley Loess Plains ####
vcdExtra::LRstats(object = glm.firepa$`Mississippi Valley Loess Plains`)
#prchisq  0.05436. 
plot(x = glm.firepa$`Mississippi Valley Loess Plains`,
     which = c(1:6),
     col =pa$PA)
  #1. good residuals 
  #2. qqs follow  a good line.  But very few datapoints
  #3. Scale location is one light, ith outliers on either end.
  #4. Cook's distance only has one outlier.
  #5. The leverage plot looks great
  #6. 
####North Central Hardwood Forests ####
vcdExtra::LRstats(object = glm.firepa$`North Central Hardwood Forests`)
#prchisq  0.05958 comparatively good
plot(x = glm.firepa$`North Central Hardwood Forests`,
     which = c(1:6),
     col =pa$PA)
  #1. residuals are straight, but have large gaps between datapoints.a notable outlier too.
  #2. qqs fine except for one outlier. Low datapoint number.
  #3. scale location shows hte one outlier.
  #4. same shows up on Cook's Distance
  #5. leverage plots are fine.
  #6.
colnames(hardwood.cooks)
hardwood.cooks<-as.data.frame(cooks.distance(glm.firepa$`North Central Hardwood Forests`)) %>% 
  rename(cooks = "cooks.distance(glm.firepa$`North Central Hardwood Forests`)") %>% 
  rownames_to_column(var = "object_id") %>% 
  subset(cooks < 0.1)

pa.harwood<-pa %>% 
  filter(NA_L3NAME == "North Central Hardwood Forests") %>% 
  filter(object_id %in% hardwood.cooks$object_id) 

#now run the model with the subset data
pa.hard.cooks.glm<-glm(PA ~ prct_woody, data = pa.harwood, family = binomial)
summary(pa.hard.cooks.glm)
summary(glm.firepa$`North Central Hardwood Forests`)
vcdExtra::LRstats(object = pa.hard.cooks.glm)
plot(x = pa.hard.cooks.glm,
     which = c(1:6),
     col =pa$PA)





####Ouachita Mountains ####
vcdExtra::LRstats(object = glm.firepa$`Ouachita Mountains`)
#prchisq  0.0003449
plot(x = glm.firepa$`Ouachita Mountains`,
     which = c(1:6),
     col =pa$PA)
  #straight residuals.
  #good qqs
  #Leverage plots look good. 


plot(x = glm.firepa$`Ozark Highlands`,
     which = c(1:6),
     col=pa$PA)
  #Residuals bend a lot.
  #Qqs diverge at 1.5 theoretical quantiles.
  #leverage plots bend backward.
ozark<-augment(glm.firepa$`Ozark Highlands`)
ozark.excluded<-ozark %>% 
  subset(.cooksd > 0.01)
#excludes four observations
ozark.excluded.mod<-glm(PA ~ prct_woody, data = ozark.excluded, family = binomial)
vcdExtra::LRstats(object = ozark.excluded.mod)
#304 observations
#283 remaining
#4/304 = 0.01
plot(x = ozark.excluded.mod,
     which = c(1:6))
#the new leverage plots are REALLY WIERD. 

plot(glm.firepa$Piedmont)
  #residuals curve
  #QQs diverge at 1.5 theoretical quantiles.
  #leverage plots bend backwards
plot(glm.firepa$`Ridge and Valley`)
  #residuals curve like a bow.
  #qqs diverge at 2.0 theoretical quantiles.
  #Leverage plots bend backward
plot(glm.firepa$`South Central Plains`)
  #residuals bend like a bow
  #qq plot not fantastic below 0.5 and above 2.0 theoretical quantiles.
  #leverage plots bend backward.
plot(glm.firepa$`Southeastern Plains`)
  #the residuals bend very little. Look good to me.
  #the qq plots bend off the whazoo. 
  #the residuals vs leverage plots bend backwards
plot(glm.firepa$`Southern Coastal Plain`)
plot(x = glm.firepa$`Southern Coastal Plain`,
     which = c(1:6),
     col =pa$PA)

cooks.southerncoastplain<-as.data.frame(cooks.distance(glm.firepa$`Southern Coastal Plain`))


cooks.southerncoastplain<-cooks.southerncoastplain %>% 
  rename(cooks = `cooks.distance(glm.firepa$\`Southern Coastal Plain\`)`) %>% 
  rownames_to_column(var = "object_id") %>% 
  subset(cooks < 0.005)

pa.coast<-pa %>% 
  filter(NA_L3NAME == "Southern Coastal Plain") %>% 
  filter(object_id %in% cooks.southerncoastplain$object_id) #%>% 
  mutate(PA = as.numeric(as.character(PA)))

#now run the model with the subset data
pa.coast.glm<-glm(PA ~ prct_woody, data = pa.coast, family = binomial)
summary(pa.coast.glm)
vcdExtra::LRstats(object = pa.coast.glm)
plot(x = pa.coast.glm,
     which = c(1:6),
     col =pa$PA)


#OKAY. Maybe try this on a simpler plot than the crazy one. 
# Eventually I should find a way to have one big vector with all the non-excluded
#points, so I can run the grouped GLM again with subset data. 



 #residuals bow a little in the middle.
  #Qq goes off the rails at 1.5 theoretical quantiles.
  #leverage plots bend backward.
plot(glm.firepa$`Southwestern Appalachians`)
  #residuals bow in the middle a bit.
  #qqs diverage at upper and lower values. 
  #leverage plots bend backwards.
plot(glm.firepa$`Western Allegheny Plateau`)
  #residuals not great
  #qq okay except for 3 outliers. 
  #leverage plots look great. 


#find range of values for Mcfadden (psquared.)
#Ranges from 0 to 1 BUT, much smaller than typical rsquared. 
#values of 0.2 to 0.4 for ρ2
#represent EXCELLENT fit."
#should NOT exceed 1 if calculated correctly.
#Behavioural Travel Modelling. Edited by David Hensher and Peter Stopher. 1979. McFadden contributed Ch. 15 "Quantitative Methods for Analyzing Travel Behaviour on Individuals: Some Recent Developments". Discussion of model evaluation (in the context of multinomial logit models) begins on page 306 where he introduces ρ2
#(McFadden's pseudo R2
#). McFadden states "while the R2
# index is a more familiar concept to planner who are experienced in OLS, it is not as well behaved as the ρ2
# measure, for ML estimation. Those unfamiliar with ρ2
# should be forewarned that its values tend to be considerably lower than those of the R2
# index...For example, values of 0.2 to 0.4 for ρ2
# represent EXCELLENT fit."


