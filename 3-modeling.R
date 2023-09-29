
#load all functions
devtools::load_all()


#---------------------------------------------------------------- DATA PREPARATION ----------------------------------------------------------

#read predictor rasters

percent_rubble = raster::raster(here::here("data", "processed_data", "predictors", "percent_rubble.grd"))
percent_coral_algae = raster::raster(here::here("data", "processed_data", "predictors", "percent_coral_algae.grd"))
percent_microalgal_mats = raster::raster(here::here("data", "processed_data", "predictors", "percent_microalgal_mats.grd"))
percent_seagrass = raster::raster(here::here("data", "processed_data", "predictors", "percent_seagrass.grd"))
percent_sand = raster::raster(here::here("data", "processed_data", "predictors", "percent_sand.grd"))
percent_rock = raster::raster(here::here("data", "processed_data", "predictors", "percent_rock.grd"))

percent_back_reef_slope = raster::raster(here::here("data", "processed_data", "predictors", "percent_back_reef_slope.grd"))
percent_deep_lagoon = raster::raster(here::here("data", "processed_data", "predictors", "percent_deep_lagoon.grd"))
percent_inner_reef_flat = raster::raster(here::here("data", "processed_data", "predictors", "percent_inner_reef_flat.grd"))
percent_outer_reef_flat = raster::raster(here::here("data", "processed_data", "predictors", "percent_outer_reef_flat.grd"))
percent_plateau = raster::raster(here::here("data", "processed_data", "predictors", "percent_plateau.grd"))
percent_reef_crest = raster::raster(here::here("data", "processed_data", "predictors", "percent_reef_crest.grd"))
percent_reef_slope = raster::raster(here::here("data", "processed_data", "predictors", "percent_reef_slope.grd"))
percent_shallow_lagoon = raster::raster(here::here("data", "processed_data", "predictors", "percent_shallow_lagoon.grd"))
percent_terrestrial_reef_flat = raster::raster(here::here("data", "processed_data", "predictors", "percent_terrestrial_reef_flat.grd"))

turbidity = raster::raster(here::here("data", "processed_data", "predictors", "turbidity.grd")) #better than population and pop_desnity to express human impacts
travel_time = raster::raster(here::here("data", "processed_data", "predictors", "travel_time.grd")) #not used
population = raster::raster(here::here("data", "processed_data", "predictors", "population.grd")) #not used
pop_density = raster::raster(here::here("data", "processed_data", "predictors", "pop_density.grd")) 
mpa_type = raster::raster(here::here("data", "processed_data", "predictors", "mpa_type.grd")) #-1 no mpa / 1 no-take mpa / 2 partial mpa
mpa_type_no_take_only = raster::raster(here::here("data", "processed_data", "predictors", "mpa_type_no_take_only.grd")) #-1 no mpa / 1 no-take mpa
mpa_pres = raster::raster(here::here("data", "processed_data", "predictors", "mpa_pres.grd")) #1 = mpa presence / 0 = absence
dist_land = raster::raster(here::here("data", "processed_data", "predictors", "dist_land.grd"))
dist_reef = raster::raster(here::here("data", "processed_data", "predictors", "dist_reef.grd"))
depth = raster::raster(here::here("data", "processed_data", "predictors", "depth.grd"))
dist_seagrass = raster::raster(here::here("data", "processed_data", "predictors", "dist_seagrass.grd"))
dist_passes = raster::raster(here::here("data", "processed_data", "predictors", "dist_passes.grd"))
dist_barrier_reef = raster::raster(here::here("data", "processed_data", "predictors", "dist_barrier_reef.grd"))
dist_intermediate_reef = raster::raster(here::here("data", "processed_data", "predictors", "dist_intermediate_reef.grd"))
percent_deep_lagoon_millenium = raster::raster(here::here("data", "processed_data", "predictors", "percent_deep_lagoon_millenium.grd"))
percent_shallow_terrace_millenium = raster::raster(here::here("data", "processed_data", "predictors", "percent_shallow_terrace_millenium.grd"))



#read dugong abundance and area surveyed rasters
abundance_dugong = raster::raster(here::here("data", "processed_data", "abundance", "dugong_abundance.grd"))
area_surveyed = raster::raster(here::here("data", "processed_data", "abundance", "area_surveyed.grd"))



#stack predictors
stack_pred = raster::stack(percent_rubble, percent_coral_algae, percent_microalgal_mats, percent_seagrass, percent_sand, percent_rock, 
                           percent_back_reef_slope, percent_deep_lagoon, percent_inner_reef_flat, percent_outer_reef_flat, 
                           percent_plateau, percent_reef_crest, percent_reef_slope, percent_shallow_lagoon, percent_terrestrial_reef_flat,
                           turbidity, travel_time, population, pop_density, mpa_type, mpa_type_no_take_only,
                           mpa_pres, dist_land, dist_reef, depth, dist_seagrass, 
                           dist_passes, dist_barrier_reef, dist_intermediate_reef, percent_deep_lagoon_millenium, percent_shallow_terrace_millenium)


#remove upper left corner of predictors stack (east coast) based on turbidity raster
r = raster::crop(turbidity,  raster::extent(c( 350000, 380000, 310000, 340000)))
pol = raster::rasterToPolygons(r)
stack_pred = raster::mask(stack_pred, pol, inverse = T)

#mask stack pred
stack_pred <- raster::mask(stack_pred, stack_pred[[1]])


#stack all rasters
stack = raster::stack(abundance_dugong, area_surveyed, percent_rubble, percent_coral_algae, percent_microalgal_mats, percent_seagrass, percent_sand, percent_rock,
                      percent_back_reef_slope, percent_deep_lagoon, percent_inner_reef_flat, percent_outer_reef_flat,
                      percent_plateau, percent_reef_crest, percent_reef_slope, percent_shallow_lagoon, percent_terrestrial_reef_flat,
                      turbidity, travel_time, population, pop_density, mpa_type, mpa_type_no_take_only,
                      mpa_pres, dist_land, dist_reef, depth, dist_seagrass,
                      dist_passes, dist_barrier_reef, dist_intermediate_reef, percent_deep_lagoon_millenium, percent_shallow_terrace_millenium)


#mask this stack with dugong abundance raster so that NAs are assigned where there are NAs in dugong raster
#this is the surveyed stack
stack_surv = raster::mask(stack, abundance_dugong)



#quick stats
raster::cellStats(stack_surv$n_Dugong_certain_probable, sum) # (200.62) 152.4 counted individuals (after availability correction)
raster::cellStats(stack_surv$area_surveyed_m2, sum)*10^-6 # (101.6526) 61.7 km2 surface surveyed
raster::freq(stack_surv$n_Dugong_certain_probable, value=0) # (1859) 1708 cells with zeros 
raster::freq(stack_surv$n_Dugong_certain_probable, value=NA) # (38865) 39029 cells with nas
raster::ncell(stack_surv$n_Dugong_certain_probable) #40762 total nb of cells
raster::ncell(stack_surv$n_Dugong_certain_probable) - raster::freq(stack_surv$n_Dugong_certain_probable, value=NA) #(1897) 1733 non na cells
#proportion of cells with zeros 1708/1733 = 0.98

#get polygon of total surveyed area (for now using 500m buffer) *** pb polygon should be between outer transect so taking abnundance raster and just filling between hole
#surv_bloc = get_polygon_surveyed_area(abundance_dugong)
#surv_bloc_area = raster::area(a) * 10^-6 # 822.93 km2

#OR read and project surveyed bloc (created in qgis)
surv_block = read_project_surveyed_block()
surv_block_area = sf::st_area(surv_block) * 10^-6 # 752.9366 km2



#converting stacks to data frames for modeling (surveyed stack and predictor stack)
data_surv = stack_surv %>% 
  raster::as.data.frame(xy = T)

data_pred = stack_pred %>% 
  raster::as.data.frame(xy = T)



#map predictor stack **************REDO

stack_pred_for_mapping <- stack_pred
new_caledonia <- raster::getData("GADM", country = "NCL", level = 1)
new_caledonia_proj = sp::spTransform(new_caledonia, CRS("+init=epsg:3163")) #NC projection

png(here::here("outputs/models/predictor_stack1.png"), width = 960, height = 960)
par(mfrow=c(2,2))
raster::plot(stack_pred_for_mapping[[7]], axes = F, main = "Percentage of back reef slope coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[8]], axes = F, main = "Percentage of deep lagoon coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[9]], axes = F, main = "Percentage of inner reef flat coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[10]], axes = F, main = "Percentage of outer reef flat coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

png(here::here("outputs/models/predictor_stack2.png"), width = 960, height = 960)
par(mfrow=c(2,2))
raster::plot(stack_pred_for_mapping[[11]], axes = F, main = "Percentage of plateau coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[12]], axes = F, main = "Percentage of reef crest coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[13]], axes = F, main = "Percentage of reef slope coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[14]], axes = F, main = "Percentage of shallow lagoon coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

png(here::here("outputs/models/predictor_stack3.png"), width = 960, height = 960)
par(mfrow=c(2,2))
raster::plot(stack_pred_for_mapping[[15]], axes = F, main = "Percentage of terrestrial reef flat coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[1]], axes = F, main = "Percentage of rubble coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[2]], axes = F, main = "Percentage of coral/algae coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[3]], axes = F, main = "Percentage of microalgal mats coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

png(here::here("outputs/models/predictor_stack4.png"), width = 960, height = 960)
par(mfrow=c(2,2))
raster::plot(stack_pred_for_mapping[[4]], axes = F, main = "Percentage of seagrass coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[5]], axes = F, main = "Percentage of sand coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[6]], axes = F, main = "Percentage of rock coverage", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[24]], axes = F, main = "Distance to reef", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

png(here::here("outputs/models/predictor_stack5.png"), width = 960, height = 960)
par(mfrow=c(2,2))
raster::plot(stack_pred_for_mapping[[26]], axes = F, main = "Distance to seagrass", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[23]], axes = F, main = "Distance to land", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[25]], axes = F, main = "Depth", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[21]], axes = F, main = "Presence / absence of no-take marine protected area", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

png(here::here("outputs/models/predictor_stack6.png"), width = 960, height = 960)
par(mfrow=c(2,2))
raster::plot(stack_pred_for_mapping[[16]], axes = F, main = "Turbidity", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[19]], axes = F, main = "Human population density", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

#save(r, file = here::here("3-modeling.RData"))

#---------------------------------------------------------------- CHECKING CORRELATIONS ----------------------------------------------------------

#Correlogram
data_corr = data_surv %>%
  dplyr::select_if(is.double) %>% 
  dplyr::select(-c(x, y, n_Dugong_certain_probable, area_surveyed_m2)) %>% 
  tidyr::drop_na()

coef <- cor(data_corr) #pearson by default

png(here::here("outputs/models/corrplot_circle.png"))
corrplot::corrplot(coef, method="circle")
dev.off()


png(here::here("outputs/models/corrplot_color.png"))
corrplot::corrplot(coef, method="color")
dev.off()


#preparing data for modeling

#pop_dens and dist_land are very correlated, removing pop_dens
#dist_seagrass and dist_land are very correlated, removing dist_land
#terrestrial_reef_flat and seagrass are correlated, removing terrestrial_reef_flat
data_surv = data_surv %>%
  #removing correlated variables
  dplyr::select(-c(pop_dens, dist_land, terrestrial_reef_flat)) %>% 
  #Keep mpa pres as MPA variable (removing the others), removing population which is not correct and better replaced by turbidity
  dplyr::select(-c(mpa_type_no_take_only_mpa_type, mpa_type_mpa_type, population)) %>%
  #removing depth higher than 0 
  dplyr::filter(!depth > 0) %>%
  #renaming
  dplyr::rename("abundance" = n_Dugong_certain_probable,
                "mpa_pres" = mpa_pres_mpa_pres) %>% 
  #create abundance rounded (for zip models)
  dplyr::mutate(abundance_round = round(abundance)) 





#---------------------------------------------------------------- MODEL FIT ----------------------------------------------------------
#useful wiki: https://osf.io/wgc4f/wiki/home/



####################  fit full abudance GAM with area surveyed as offset and unlimited k

library(mgcv)

#tw distrib
mod_tw <- fit_full_abundance_model("tw", data_surv)

#nb distrib
mod_nb <- fit_full_abundance_model("nb", data_surv)

#zip distrib
mod_zip <- fit_full_abundance_model("zip", data_surv)


#tw distrib
mod_tw_meaningful_covar <- fit_full_abundance_model_meaningful_covar("tw", data_surv)

#nb distrib
mod_nb_meaningful_covar <- fit_full_abundance_model_meaningful_covar("nb", data_surv)

#zip distrib
mod_zip_meaningful_covar <- fit_full_abundance_model_meaningful_covar("zip", data_surv)


#################### model selection

# based on AIC
AIC(mod_zip) 
AIC(mod_tw)
AIC(mod_nb) #best AIC but tw prefered based on better diagnostic plots


# based on AIC
AIC(mod_zip_meaningful_covar) 
AIC(mod_tw_meaningful_covar)  #best AIC
AIC(mod_nb_meaningful_covar)






#################### fit zip reduced model (removing NS variables *** sequencially with threshold 0.01)

mod_zip_reduced <- gam(abundance_round ~  s(travel_time) + s(dist_seagrass) + s(outer_reef_flat) + s(shallow_lagoon)+
             offset(log(area_surveyed_m2)), family = ziP(), data = data_surv, method="REML")



#summary
sink(here::here("outputs/models/summary_reduced_zip.txt"))
print(summary(mod_zip_reduced))
sink()

#terms plot
png(here::here("outputs/models/terms_plot_reduced_zip.png"))
plot(mod_zip_reduced, pages = 1) 
dev.off()

#gam check
png(here::here("outputs/models/gam_check_reduced_zip.png"))
qq.gam(mod_zip_reduced, type="deviance")
dev.off()

#diagnostics plot
png(here::here("outputs/models/diagnostics_plot_reduced_zip.png"))
par(mfrow=c(2,2))
gam.check(mod_zip_reduced) 
dev.off()

#autocorrelation plot
png(here::here("outputs/models/autocorrelogram_plot_reduced_zip.png"))
acf(mod_zip_reduced$residuals)
dev.off()











#################### fit nb reduced model (removing NS variables *** sequencially with threshold 0.05)

mod_nb_reduced <- gam(abundance ~ s(shallow_lagoon)+ s(turbidity) + s(outer_reef_flat) + s(dist_seagrass) +
                      offset(log(area_surveyed_m2)), family = nb(), data = data_surv, method="REML") 

#summary
sink(here::here("outputs/models/summary_reduced_nb.txt"))
print(summary(mod_nb_reduced))
sink()

#terms plot
png(here::here("outputs/models/terms_plot_reduced_nb.png"))
plot(mod_nb_reduced, pages = 1) 
dev.off()

#gam check
png(here::here("outputs/models/gam_check_reduced_nb.png"))
qq.gam(mod_nb_reduced, type="deviance")
dev.off()

#diagnostics plot
png(here::here("outputs/models/diagnostics_plot_reduced_nb.png"))
par(mfrow=c(2,2))
gam.check(mod_nb_reduced) 
dev.off()

#autocorrelation plot
png(here::here("outputs/models/autocorrelogram_plot_reduced_nb.png"))
acf(mod_nb_reduced$residuals)
dev.off()






#################### fit tw reduced model (removing NS variables - sequentially with threshold 0.05 less stringent) ********* best model **********

mod_tw_reduced <- gam(abundance ~ s(outer_reef_flat) + s(shallow_lagoon) + s(depth) + s(dist_seagrass) +
                        offset(log(area_surveyed_m2)), family = tw(), data = data_surv, method="REML") 

#summary
sink(here::here("outputs/models/summary_reduced_tw.txt"))
print(summary(mod_tw_reduced))
sink()

#terms plot
png(here::here("outputs/models/terms_plot_reduced_tw.png"))
plot(mod_tw_reduced, pages = 1, scale = 0) 
dev.off()

#gam check
png(here::here("outputs/models/gam_check_reduced_tw.png"))
qq.gam(mod_tw_reduced, type="deviance")
dev.off()

#diagnostics plot
png(here::here("outputs/models/diagnostics_plot_reduced_tw.png"))
par(mfrow=c(2,2))
gam.check(mod_tw_reduced) 
dev.off()

#autocorrelation plot
png(here::here("outputs/models/autocorrelogram_plot_reduced_tw.png"))
acf(mod_tw_reduced$residuals)
dev.off()







#---------------------------------------------------------------- MODEL PREDICTIONS (unrestricted)----------------------------------------------------------



#Define study area coordinates
#big region
lat1 =  -21.2 ; lat2 = -21.9
lon1 = 164.8 ; lon2 = 165.8

#open street map
maplatlon = osm_map(lat1, lon1, lat2, lon2)

#project osm for density mapping
maplatlon_proj = osm_mapproj(maplatlon)




#### make prediction based on best model (mod_tw_reduced)

covariates = c("outer_reef_flat", "shallow_lagoon", "depth", "dist_seagrass")

# predictions and standard errors on surveyed area (Ind / 0.25 km2) using mean of surveyed area per cell as constant offset
pred_surveyed = raster::predict(stack_surv[[covariates]], mod_tw_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = c(mean(data_surv$area_surveyed_m2)))) 
pred_se_surveyed = raster::predict(stack_surv[[covariates]], mod_tw_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = c(mean(data_surv$area_surveyed_m2))))   

map_abundance_predictions_surveyed(maplatlon_proj, pred_surveyed, pred_se_surveyed)
raster::cellStats(pred_surveyed, sum) #97 ind

# IMPORTANT 
# - we set offset as mean of surveyed area in the data (rather than 0.25 km2) - the predicted abundance is highly dependent on the offset as a result it should not be taken as an absolute abundance
# - prediction map in indiv / 0.25 km2 cells, not ind / 0.25 km2 surveyed area

# write rasters
raster::writeRaster(pred_surveyed, here::here("data", "processed_data", "predictions", "predictions_surveyed"), overwrite=TRUE)
raster::writeRaster(pred_se_surveyed, here::here("data", "processed_data", "predictions", "predictions_se_surveyed"), overwrite=TRUE)



             
# predictions and standard errors on region (Ind / 0.25 km2)
pred_region = raster::predict(stack_pred[[covariates]], mod_tw_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = c(mean(data_surv$area_surveyed_m2)))) 
pred_se_region = raster::predict(stack_pred[[covariates]], mod_tw_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = c(mean(data_surv$area_surveyed_m2)))) 
map_abundance_predictions_region(maplatlon_proj, pred_region, pred_se_region)
raster::cellStats(pred_region, sum) #198 ind

# write rasters
raster::writeRaster(pred_region, here::here("data", "processed_data", "predictions", "predictions_region"), overwrite=TRUE)
raster::writeRaster(pred_se_region, here::here("data", "processed_data", "predictions", "predictions_se_region"), overwrite=TRUE)




#---------------------------------------------------------------- MODEL PERFORMANCE EVALUATION ----------------------------------------------------------


#IMPORTANT observations are comparable to predictions only if predictions are done on surveyed area as offset -> so need to redo the predictions with no constant
pred_surveyed_for_comp = raster::predict(stack_surv[[c( "area_surveyed_m2", covariates)]], mod_tw_reduced, type="response", se.fit = TRUE, index = 1)


############### spearman rank correlations --------------------------------------

#spearman rank correlations measures how well the abundance estimates rank the observed abundances. The SRC ranges from −1.0 to 1.0 
#with values above zero indicating a positive association
#between estimated and observed abundances with a value of 1.0 indicating perfect ranking. 


#### correlations between observations and predictions - non zeros only -

#replace zeros with nas
abundance_dugong2 = abundance_dugong
abundance_dugong2[abundance_dugong2 == 0] <- NA

#mask prediction raster 
pred_surveyed_for_comp2 = raster::mask(pred_surveyed_for_comp, abundance_dugong2)

#transform to vectors to caculate speartman coef
obs = as.vector(abundance_dugong2)
obs = obs[!is.na(obs)]  

pred = as.vector(pred_surveyed_for_comp2)
pred = pred[!is.na(pred)]  

cor(obs, pred, method = 'spearman', use = "everything") #0.28
#cor.test(obs, pred, method = 'spearman', use = "everything") 

#plot
plot(obs, pred)



#### spearman rank correlations between observations and predictions - all data -

#mask dugong raster to get same NAs
abundance_dugong2 = raster::mask(abundance_dugong, pred_surveyed_for_comp)
raster::freq(abundance_dugong2, value = NA)
raster::freq(pred_surveyed_for_comp, value = NA) #same na numbers

#transform to vectors to caculate speartman coef
obs = as.vector(abundance_dugong2)
obs = obs[!is.na(obs)]  

pred = as.vector(pred_surveyed_for_comp)
pred = pred[!is.na(pred)]  

cor(obs, pred, method = 'spearman', use = "everything") #0.13

#plot
plot(obs, pred)





############### percentage of mean absolute error  --------------------------------------

# i.e., the mean absolute error divided by the mean abundance (a PMAE > 100% would indicate an average error higher than the average abundance
# and thus a poor fit).
# measures the average magnitude of error produced by a model, or how far off predictions are on average

#first way to calculate
(mean(abs(pred-obs)) / mean (obs))*100 #89

#second way
(sum(abs(obs-pred) / obs) * 100 / length(obs)) #93







############### TSS  --------------------------------------

# From Becker et al 2020: AUC and TSS measure the discriminatory ability of an SDM and can be calculated using any type of prediction value. 
# TSS is the maximum value of the True Skill Statistic over range of threshold values 
# TSS =  Sensitivity + Specificity - 1
# From Allouche et al 2006 : TSS takes into account both omission and commission errors, and success as a result of random
# guessing, and ranges from 1 to +1, where +1 indicates perfect agreement and values of zero or less indicate
# a performance no better than random.

#converting obs (nb of indiv) to presence - absence
obs2 <- obs
obs2[obs2 == 0] <- "absence"
obs2[obs2 != "absence"] <- "presence"
obs2 <- as.factor(obs2)

# From Becker et al 2020: to calculate TSS for the GAM density models, we used the sensitivity–specificity sum maximization approach (Liu, Berry, Dawson,
# & Pearson, 2005) to obtain thresholds for species presence

# using thresholds for converting to presence in predictions

library(caret)

thres <- seq(0.01, 0.1, 0.01)
sens_spec < NA

for (i in 1:length(thres)){

  #converting pred to presence - absence based on given threshold
  pred2 <- pred
  pred2[pred2 < thres[i]] <- "absence"
  pred2[pred2 != "absence"] <- "presence"
  pred2 <- as.factor(pred2)
  
  #table(obs2, pred2)
  
  #calculating sensitivity (or true positive rate or recall) and sensitivity (or true negative rate) and their sum
  sens <- sensitivity(pred2, obs2)
  spec <- specificity(pred2, obs2)
  sens_spec[i] <- sens + spec
  
}
index <- which.max(sens_spec)
selected_thres <- thres[index] # selected threshold maximizing the sum of specificity and sensitivity : 0.2


#converting pred to presence - absence baseds on selected threshold
pred2 <- pred
pred2[pred2 < selected_thres] <- "absence"
pred2[pred2 != "absence"] <- "presence"
pred2 <- as.factor(pred2)

#table(obs2, pred2)

#calculating sensitivity (or true positive rate or recall) and sensitivity (or true negative rate)
sens <- sensitivity(pred2, obs2)
spec <- specificity(pred2, obs2)

tss <-  sens + spec - 1 #0.516



############### AUC  --------------------------------------

#ROC Curves plot the true positive rate (sensitivity) against the false positive rate (1-specificity)
#AUC is the area under the ROC curv - the closer to 1 the better
#From Allouche et al 2006 AUC was shown to be independent of prevalence and is considered a highly effective
#measure for the performance of ordinal score models.
#AUC is also threshold invariant
#The value of AUC ranges from 0 to 1, which means an excellent model will have AUC near 1, and hence it will show a good measure of discrmination between classes
#Although the AUC-ROC curve is only used for binary classification problems, we can also use it for multiclass classification problems. (? multiclass.roc)

library(pROC)

#calculating auc
roc <- multiclass.roc(obs, pred)
auc(roc) #0.92




############### Spearman per strata  - all data --------------------------------------

#read strata
strat1 <- read_project_surveyed_strata("west_coast_nc_strata1.shp")
strat2 <- read_project_surveyed_strata("west_coast_nc_strata2.shp")
strat3 <- read_project_surveyed_strata("west_coast_nc_strata3.shp")

pred_surveyed_for_comp2 <- pred_surveyed_for_comp

#mask abundance raster 
abundance_dugong2 = raster::mask(abundance_dugong, pred_surveyed_for_comp2)

#mask obs and pred rasters with strata
abundance_dugong2_strat1 <- raster::mask(abundance_dugong2, strat1)
pred_surveyed_for_comp2_strat1 <- raster::mask(pred_surveyed_for_comp2, strat1)
abundance_dugong2_strat2 <- raster::mask(abundance_dugong2, strat2)
pred_surveyed_for_comp2_strat2 <- raster::mask(pred_surveyed_for_comp2, strat2)
abundance_dugong2_strat3 <- raster::mask(abundance_dugong2, strat3)
pred_surveyed_for_comp2_strat3 <- raster::mask(pred_surveyed_for_comp2, strat3)

#calculate spearman coef per strata
cor(na.omit(as.vector(abundance_dugong2_strat1)), na.omit(as.vector(pred_surveyed_for_comp2_strat1))) # 0.33
cor(na.omit(as.vector(abundance_dugong2_strat2)), na.omit(as.vector(pred_surveyed_for_comp2_strat2))) # 0.96
cor(na.omit(as.vector(abundance_dugong2_strat3)), na.omit(as.vector(pred_surveyed_for_comp2_strat3))) # 0.17

#calculate mean abundance per strata
mean_obs_strat1 <- mean(as.vector(abundance_dugong2_strat1), na.rm = TRUE)
mean_pred_strat1 <- mean(as.vector(pred_surveyed_for_comp2_strat1), na.rm = TRUE)
mean_obs_strat2 <- mean(as.vector(abundance_dugong2_strat2), na.rm = TRUE)
mean_pred_strat2 <- mean(as.vector(pred_surveyed_for_comp2_strat2), na.rm = TRUE)
mean_obs_strat3 <- mean(as.vector(abundance_dugong2_strat3), na.rm = TRUE)
mean_pred_strat3 <- mean(as.vector(pred_surveyed_for_comp2_strat3), na.rm = TRUE)

#calculate speartman coef across the 3 stratas
mean_obs <- c(mean_obs_strat1, mean_obs_strat2, mean_obs_strat3)
mean_pred <- c(mean_pred_strat1, mean_pred_strat2, mean_pred_strat3)
cor(mean_obs, mean_pred, method = 'spearman', use = "everything") #0.5

#calculate mean obs : mean pred per strata
mean_obs / mean_pred #1.259622 2.003509 1.007262 - > underestimation by half in strata 2 ?

#calculate obs : pred per strata
mean(na.omit(as.vector(abundance_dugong2_strat1)) / na.omit(as.vector(pred_surveyed_for_comp2_strat1))) # 1.627365
mean(na.omit(as.vector(abundance_dugong2_strat2)) / na.omit(as.vector(pred_surveyed_for_comp2_strat2))) # 0.2349727 ?
mean(na.omit(as.vector(abundance_dugong2_strat3)) / na.omit(as.vector(pred_surveyed_for_comp2_strat3))) # 0.4420843







############### Spearman per strata  - non zeros only --------------------------------------

#read strata
strat1 <- read_project_surveyed_strata("west_coast_nc_strata1.shp")
strat2 <- read_project_surveyed_strata("west_coast_nc_strata2.shp")
strat3 <- read_project_surveyed_strata("west_coast_nc_strata3.shp")

#replace zeros with nas
abundance_dugong2 = abundance_dugong
abundance_dugong2[abundance_dugong2 == 0] <- NA

#mask prediction raster 
pred_surveyed_for_comp2 = raster::mask(pred_surveyed_for_comp, abundance_dugong2)

#mask obs and pred rasters with strata
abundance_dugong2_strat1 <- raster::mask(abundance_dugong2, strat1)
pred_surveyed_for_comp2_strat1 <- raster::mask(pred_surveyed_for_comp2, strat1)
abundance_dugong2_strat2 <- raster::mask(abundance_dugong2, strat2)
pred_surveyed_for_comp2_strat2 <- raster::mask(pred_surveyed_for_comp2, strat2)
abundance_dugong2_strat3 <- raster::mask(abundance_dugong2, strat3)
pred_surveyed_for_comp2_strat3 <- raster::mask(pred_surveyed_for_comp2, strat3)

#calculate spearman coef per strata
cor(na.omit(as.vector(abundance_dugong2_strat1)), na.omit(as.vector(pred_surveyed_for_comp2_strat1))) # 0.99
cor(na.omit(as.vector(abundance_dugong2_strat2)), na.omit(as.vector(pred_surveyed_for_comp2_strat2))) # 0.99
cor(na.omit(as.vector(abundance_dugong2_strat3)), na.omit(as.vector(pred_surveyed_for_comp2_strat3))) # 0.65

#calculate mean abundance per strata
mean_obs_strat1 <- mean(as.vector(abundance_dugong2_strat1), na.rm = TRUE)
mean_pred_strat1 <- mean(as.vector(pred_surveyed_for_comp2_strat1), na.rm = TRUE)
mean_obs_strat2 <- mean(as.vector(abundance_dugong2_strat2), na.rm = TRUE)
mean_pred_strat2 <- mean(as.vector(pred_surveyed_for_comp2_strat2), na.rm = TRUE)
mean_obs_strat3 <- mean(as.vector(abundance_dugong2_strat3), na.rm = TRUE)
mean_pred_strat3 <- mean(as.vector(pred_surveyed_for_comp2_strat3), na.rm = TRUE)

#calculate speartman coef across the 3 stratas
mean_obs <- c(mean_obs_strat1, mean_obs_strat2, mean_obs_strat3)
mean_pred <- c(mean_pred_strat1, mean_pred_strat2, mean_pred_strat3)
cor(mean_obs, mean_pred, method = 'spearman', use = "everything") #0.5




#---------------------------------------------------------------- EXTRAPOLATION EVALUATION ----------------------------------------------------------


# install dsm extra
# remotes::install_github("densitymodelling/dsmextra", dependencies = TRUE)


# Other required libraries
library(dsmextra)     # Extrapolation toolkit for ecological models
library(raster)       # Geographic data analysis and modeling
library(tidyverse)    # Packages for data science
library(magrittr)     # Pipe operator


# Define environmental covariates of interest
covariates

# Get surveyed segments (here pixels) and prediction grid
segs <- data_surv[c("x","y", covariates)]
predgrid <- data_pred[c("x","y", covariates)]



# Define projected coordinate system
crs <- sp::CRS("+init=epsg:3163") #lambert new caledonia


# Compute extrapolation
extrapolation <- compute_extrapolation(samples = segs,
                                       covariate.names = covariates,
                                       prediction.grid = predgrid,
                                       coordinate.system = crs)

summary(extrapolation) #univariate extrapolation 0.58 - no combinational extrapolation

sink(here::here("outputs/models/extrapolation_summary.txt"))
print(summary(extrapolation))
sink()



# Compare covariates
sink(here::here("outputs/models/extrapolation_covariates_comparison.txt"))
compare_covariates(extrapolation.type = "both",
                   extrapolation.object = extrapolation,
                   n.covariates = NULL,
                   create.plots = TRUE,
                   display.percent = TRUE)
sink()



# Map extrapolation (NB: this will create interactive html map which we have to save manually)
map_extrapolation(map.type = "extrapolation",
                  extrapolation.object = extrapolation)

map_extrapolation(map.type = "mic",
                  extrapolation.object = extrapolation)




#---------------------------------------------------------------- MODEL PREDICTIONS (restricted to univariate interpolation)----------------------------------------------------------


# restrict predictors stack to range of predictors in the data to avoid univariate extrapolation
stack_pred_interp = stack_pred
stack_pred_interp$shallow_lagoon[stack_pred_interp$shallow_lagoon > max(segs$shallow_lagoon, na.rm = T) | stack_pred_interp$shallow_lagoon < min(segs$shallow_lagoon, na.rm = T)] <- NA
stack_pred_interp$outer_reef_flat[stack_pred_interp$outer_reef_flat > max(segs$outer_reef_flat, na.rm = T) | stack_pred_interp$outer_reef_flat < min(segs$outer_reef_flat, na.rm = T)] <- NA
stack_pred_interp$dist_seagrass[stack_pred_interp$dist_seagrass > max(segs$dist_seagrass, na.rm = T) | stack_pred_interp$dist_seagrass < min(segs$dist_seagrass, na.rm = T)] <- NA
stack_pred_interp$depth[stack_pred_interp$depth > max(segs$depth, na.rm = T) | stack_pred_interp$depth < min(segs$depth, na.rm = T)] <- NA


# predictions and standard errors on region (Ind / 0.25 km2) in univariate interpolation situations 
pred_region_interp = raster::predict(stack_pred_interp, mod_tw_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = c(mean(data_surv$area_surveyed_m2)))) 
pred_se_region_interp = raster::predict(stack_pred_interp, mod_tw_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = c(mean(data_surv$area_surveyed_m2)))) 

map_abundance_predictions_region_interp(maplatlon_proj, pred_region_interp, pred_se_region_interp)
map_abundance_predictions_region_interp_new(maplatlon_proj, pred_region_interp) #new improved abundance map
raster::cellStats(pred_region_interp, sum) #191 ind

summary(pred_region_interp)
mean(values(pred_region_interp),na.rm=T) #0.057 ind / 0.25km2

#restrict predictions to surveyed bloc
pred_region_interp_surv = raster::mask(pred_region_interp, surv_block)
pred_se_region_interp_surv = raster::mask(pred_se_region_interp, surv_block)

map_abundance_predictions_region_interp_surv(maplatlon_proj, pred_region_interp_surv, pred_se_region_interp_surv)
raster::cellStats(pred_region_interp_surv, sum) #149 ind


# write rasters
raster::writeRaster(pred_region_interp, here::here("data", "processed_data", "predictions", "predictions_region_interp"), overwrite=TRUE)
raster::writeRaster(pred_se_region_interp, here::here("data", "processed_data", "predictions", "predictions_se_region_interp"), overwrite=TRUE)
raster::writeRaster(pred_region_interp_surv, here::here("data", "processed_data", "predictions", "predictions_region_interp_surv"), overwrite=TRUE)
raster::writeRaster(pred_se_region_interp_surv, here::here("data", "processed_data", "predictions", "predictions_se_region_interp_surv"), overwrite=TRUE)





#---------------------------------------------------------------- BLOCK ABUNDANCE ESTIMATE ----------------------------------------------------------

# abondance on surveyed area
sum(data_surv$abundance) #152

# global density on surveyed area (total nb observed individuals after correction / total surveyed area)
density = sum(data_surv$abundance) / (sum(data_surv$area_surveyed_m2) * 10^-6) #2.5 indiv / km2

# abundance on surveyed block
surveyed_block_abundance = density * surv_block_area # 1893 indiv ***why this number so high ? -> high block area compared to surveyed area ?

# abondance on surveyed pixels (1733 is the nb of surveyed pixels and 0.25 km2 is the area of each pixel)
surveyed_pixels_abundance = density * 1733 * 0.25 # 1089 indiv 




#TODO


#crossval (spatial cv en utilisant la date ?) - cf RMSS cleguer ?
#+++ie pred vs observed nonparametric Spearman rank correlation test per block (eg poe, s poe, n poe) cf becker

#cv block estimates see buckland

#dsm vs gam package ? cf pour dsm https://github.com/pjbouchet/sousa_dsm/blob/main/sousa_dsm_main.R

#add random effect transect to account for double survey and autor between pixels of same transect  - cf raudino et al 2023


#relevant papers

#becker et al 2020
#raudino et al 2023