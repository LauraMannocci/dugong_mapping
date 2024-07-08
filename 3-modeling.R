
#load all functions
devtools::load_all()


#---------------------------------------------------------------- DATA PREPARATION ----------------------------------------------------------

#read mpas
mpas <- read_mpanc()

#read predictor rasters

dist_land = raster::raster(here::here("data", "processed_data", "predictors", "dist_land.grd"))
depth = raster::raster(here::here("data", "processed_data", "predictors", "depth.grd"))
slope = raster::raster(here::here("data", "processed_data", "predictors", "slope.grd"))
dist_seagrass = raster::raster(here::here("data", "processed_data", "predictors", "dist_seagrass.grd"))
dist_passes = raster::raster(here::here("data", "processed_data", "predictors", "dist_passes.grd"))
dist_all_reef = raster::raster(here::here("data", "processed_data", "predictors", "dist_all_reef.grd"))
deep_lagoon_coverage = raster::raster(here::here("data", "processed_data", "predictors", "percent_deep_lagoon.grd"))
reefflat_coverage = raster::raster(here::here("data", "processed_data", "predictors", "percent_reefflat.grd"))
seagrass_coverage = raster::raster(here::here("data", "processed_data", "predictors", "percent_seagrass.grd"))

#transform some predictors
deep_lagoon_coverage = 100  * deep_lagoon_coverage
reefflat_coverage = 100  * reefflat_coverage
seagrass_coverage = 100  * seagrass_coverage
depth = -depth

#renaming
names(deep_lagoon_coverage) = "deep_lagoon_coverage" 
names(reefflat_coverage) = "reefflat_coverage" 
names(seagrass_coverage) = "seagrass_coverage" 


#read dugong abundance and area surveyed rasters
abundance_dugong = raster::raster(here::here("data", "processed_data", "abundance", "dugong_abundance.grd"))
area_surveyed = raster::raster(here::here("data", "processed_data", "abundance", "area_surveyed.grd"))



#stack predictors
stack_pred = raster::stack(dist_land, depth, slope, dist_seagrass, 
                           dist_passes, dist_all_reef, 
                           deep_lagoon_coverage, reefflat_coverage, seagrass_coverage)


#remove upper left corner of predictors stack based on seagrass raster
r = raster::crop(depth,  raster::extent(c( 350000, 380000, 310000, 350000)))
pol = raster::rasterToPolygons(r)
stack_pred = raster::mask(stack_pred, pol, inverse = T)

#mask stack pred
stack_pred <- raster::mask(stack_pred, stack_pred[[2]])


#stack all rasters
stack = raster::stack(abundance_dugong, area_surveyed, 
                      dist_land, depth, slope, dist_seagrass, 
                      dist_passes, dist_all_reef, 
                      deep_lagoon_coverage, reefflat_coverage, seagrass_coverage)


#mask this stack with dugong abundance raster so that NAs are assigned where there are NAs in dugong raster
#this is the surveyed stack
stack_surv = raster::mask(stack, abundance_dugong)



#quick stats
raster::cellStats(stack_surv$n_Dugong_certain_probable, sum) # (200.62) 147.7875 counted individuals (after availability correction)
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
surv_block_area = sf::st_area(surv_block) * 10^-6 # 753 km2



#converting stacks to data frames for modeling (surveyed stack and predictor stack)
data_surv = stack_surv %>% 
  raster::as.data.frame(xy = T)

data_pred = stack_pred %>% 
  raster::as.data.frame(xy = T)



#map predictor stack 

stack_pred_for_mapping <- stack_pred
new_caledonia <- raster::getData("GADM", country = "NCL", level = 1)
new_caledonia_proj = sp::spTransform(new_caledonia, CRS("+init=epsg:3163")) #NC projection

png(here::here("outputs/models/predictor_stack1.png"), width = 960, height = 960)
par(mfrow=c(2,2), mar = c(2,2,5,5))
raster::plot(stack_pred_for_mapping[[1]], axes = F, main = "Distance to land (m)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[4]], axes = F, main = "Distance to shallow seagrass (m)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[5]], axes = F, main = "Distance to reef pass (m)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[6]], axes = F, main = "Distance to reef (m)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

png(here::here("outputs/models/predictor_stack2.png"), width = 960, height = 960)
par(mfrow=c(2,2), mar = c(2,2,5,5))
raster::plot(stack_pred_for_mapping[[9]], axes = F, main = "Shallow seagrass coverage (%)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[7]], axes = F, main = "Deep lagoon coverage (%)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[8]], axes = F, main = "Reef flat coverage (%)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

png(here::here("outputs/models/predictor_stack3.png"), width = 960, height = 960)
par(mfrow=c(2,2), mar = c(2,2,5,5))
raster::plot(stack_pred_for_mapping[[2]], axes = F, main = "Depth (m)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
raster::plot(stack_pred_for_mapping[[3]], axes = F, main = "Slope (degrees)", cex.main = 1.8)
raster::plot(new_caledonia_proj, add =T, col = "grey")
dev.off()

#save(r, file = here::here("3-modeling.RData"))



#---------------------------------------------------------------- CHECKING PREDICTORS CORRELATIONS ----------------------------------------------------------

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


#dist_seagrass and dist_land are very correlated (>0.8), removing dist_land
#slope and depth are very correlated (>0.8), removing slope
data_surv = data_surv %>%
  #removing correlated variables
  dplyr::select(-c(slope, dist_land)) %>% 
  #removing depth higher than 0 and some outliers
  dplyr::filter(!depth > 260) %>%
  dplyr::filter(!depth < 0) %>%
  #renaming
  dplyr::rename("abundance" = n_Dugong_certain_probable) %>% 
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



#################### model selection


AIC(mod_tw)
AIC(mod_nb) # best aic





#################### fit nb reduced model (backward stepwise selection removing NS variables with threshold 0.05) 


mod_nb_reduced <- gam(abundance ~ s(dist_pass) + 
                        s(depth) + s(seagrass_coverage) + 
                        offset(log(area_surveyed_m2)), 
                        family = nb(), data = data_surv, method="REML")

#summary
sink(here::here("outputs/models/summary_reduced_nb.txt"))
print(summary(mod_nb_reduced))
sink()

#terms plot
png(here::here("outputs/models/terms_plot_reduced_nb.png"))
plot(mod_nb_reduced, pages = 1, scale = 0) 
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

#customized term plot
make_customized_term_plot(mod_nb_reduced)







#---------------------------------------------------------------- MODEL PREDICTIONS (unrestricted)----------------------------------------------------------



#Define study area coordinates
#big region
lat1 =  -21.2 ; lat2 = -21.9
lon1 = 164.8 ; lon2 = 165.8

#open street map
maplatlon = osm_map(lat1, lon1, lat2, lon2)

#project osm for density mapping
maplatlon_proj = osm_mapproj(maplatlon)




#### make prediction based on best model (mod_nb_reduced)

covariates = c("dist_pass", "depth", "seagrass_coverage")

# predictions and standard errors on surveyed area (Ind / 0.25 km2) using mean of surveyed area per cell as constant offset
pred_surveyed = raster::predict(stack_surv[[covariates]], mod_nb_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = 500 * 500)) 
pred_se_surveyed = raster::predict(stack_surv[[covariates]], mod_nb_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = 500 * 500)) 

map_abundance_predictions_surveyed(maplatlon_proj, pred_surveyed, pred_se_surveyed)
raster::cellStats(pred_surveyed, sum) #540 ind



# write rasters
raster::writeRaster(pred_surveyed, here::here("data", "processed_data", "predictions", "predictions_surveyed"), overwrite=TRUE)
raster::writeRaster(pred_se_surveyed, here::here("data", "processed_data", "predictions", "predictions_se_surveyed"), overwrite=TRUE)



             
# predictions and standard errors on region (Ind / 0.25 km2)
pred_region = raster::predict(stack_pred[[covariates]], mod_nb_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = 500 * 500)) 
pred_se_region = raster::predict(stack_pred[[covariates]], mod_nb_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = 500 * 500)) 
map_abundance_predictions_region(maplatlon_proj, pred_region, pred_se_region)
raster::cellStats(pred_region, sum) #1041 ind

# write rasters
raster::writeRaster(pred_region, here::here("data", "processed_data", "predictions", "predictions_region"), overwrite=TRUE)
raster::writeRaster(pred_se_region, here::here("data", "processed_data", "predictions", "predictions_se_region"), overwrite=TRUE)




#---------------------------------------------------------------- MODEL PERFORMANCE EVALUATION ----------------------------------------------------------


#IMPORTANT observations are comparable to predictions only if predictions are done on surveyed area as offset -> so need to redo the predictions with no constant
pred_surveyed_for_comp = raster::predict(stack_surv[[c( "area_surveyed_m2", covariates)]], mod_nb_reduced, type="response", se.fit = TRUE, index = 1)


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

cor(obs, pred, method = 'spearman', use = "everything") #0.14
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

cor(obs, pred, method = 'spearman', use = "everything") #0.138

#plot
plot(obs, pred)





############### percentage of mean absolute error  --------------------------------------

# i.e., the mean absolute error divided by the mean abundance (a PMAE > 100% would indicate an average error higher than the average abundance
# and thus a poor fit).
# measures the average magnitude of error produced by a model, or how far off predictions are on average

#first way to calculate
(mean(abs(pred-obs)) / mean (obs))*100 #147

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
selected_thres <- thres[index] # selected threshold maximizing the sum of specificity and sensitivity : 0.03


#converting pred to presence - absence baseds on selected threshold
pred2 <- pred
pred2[pred2 < selected_thres] <- "absence"
pred2[pred2 != "absence"] <- "presence"
pred2 <- as.factor(pred2)

#table(obs2, pred2)

#calculating sensitivity (or true positive rate or recall) and specificity (or true negative rate)
sens <- sensitivity(pred2, obs2)
spec <- specificity(pred2, obs2)

tss <-  sens + spec - 1 #0.483



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
auc(roc) #0.9135




############### Spearman and ratio obs:pred per strata  - all data --------------------------------------

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
cor(na.omit(as.vector(abundance_dugong2_strat1)), na.omit(as.vector(pred_surveyed_for_comp2_strat1))) # 0.459
cor(na.omit(as.vector(abundance_dugong2_strat2)), na.omit(as.vector(pred_surveyed_for_comp2_strat2))) # 0.334
cor(na.omit(as.vector(abundance_dugong2_strat3)), na.omit(as.vector(pred_surveyed_for_comp2_strat3))) # 0.177

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
mean_obs / mean_pred #1.035005 1.653685 1.518397 - > underestimation by half in strata 2 ?

#calculate obs : pred per strata
mean(na.omit(as.vector(abundance_dugong2_strat1)) / na.omit(as.vector(pred_surveyed_for_comp2_strat1))) # 0.682
mean(na.omit(as.vector(abundance_dugong2_strat2)) / na.omit(as.vector(pred_surveyed_for_comp2_strat2))) # 0.844
mean(na.omit(as.vector(abundance_dugong2_strat3)) / na.omit(as.vector(pred_surveyed_for_comp2_strat3))) # 0.713







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
cor(na.omit(as.vector(abundance_dugong2_strat1)), na.omit(as.vector(pred_surveyed_for_comp2_strat1))) # 0.954
cor(na.omit(as.vector(abundance_dugong2_strat2)), na.omit(as.vector(pred_surveyed_for_comp2_strat2))) # 0.844
cor(na.omit(as.vector(abundance_dugong2_strat3)), na.omit(as.vector(pred_surveyed_for_comp2_strat3))) # 0.753

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
stack_pred_interp$dist_pass[stack_pred_interp$dist_pass > max(segs$dist_pass, na.rm = T) | stack_pred_interp$dist_pass < min(segs$dist_pass, na.rm = T)] <- NA
stack_pred_interp$seagrass_coverage[stack_pred_interp$seagrass_coverage > max(segs$seagrass_coverage, na.rm = T) | stack_pred_interp$seagrass_coverage < min(segs$seagrass_coverage, na.rm = T)] <- NA
stack_pred_interp$depth[stack_pred_interp$depth > max(segs$depth, na.rm = T) | stack_pred_interp$depth < min(segs$depth, na.rm = T)] <- NA


# predictions and standard errors on region (Ind / 0.25 km2) in univariate interpolation situations 
pred_region_interp = raster::predict(stack_pred_interp, mod_nb_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = 500 * 500)) 
pred_se_region_interp = raster::predict(stack_pred_interp, mod_nb_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = 500 * 500)) 

map_abundance_predictions_region_interp(maplatlon_proj, pred_region_interp, pred_se_region_interp)
map_abundance_predictions_region_interp_mpas(maplatlon_proj, pred_region_interp, pred_se_region_interp, mpas) #same map with mpa
map_abundance_predictions_region_interp_new(maplatlon_proj, pred_region_interp) #new improved abundance map


raster::cellStats(pred_region_interp, sum) #1001 ind

summary(pred_region_interp)
mean(values(pred_region_interp),na.rm=T) #0.0307 ind / 0.25km2


#Histogram of predicted abundances
hist_predicted_abundance(pred_region_interp)


#restrict predictions to surveyed bloc
pred_region_interp_surv = raster::mask(pred_region_interp, surv_block)
pred_se_region_interp_surv = raster::mask(pred_se_region_interp, surv_block)

map_abundance_predictions_region_interp_surv(maplatlon_proj, pred_region_interp_surv, pred_se_region_interp_surv)
raster::cellStats(pred_region_interp_surv, sum) #868 ind


# write rasters
raster::writeRaster(pred_region_interp, here::here("data", "processed_data", "predictions", "predictions_region_interp"), overwrite=TRUE)
raster::writeRaster(pred_se_region_interp, here::here("data", "processed_data", "predictions", "predictions_se_region_interp"), overwrite=TRUE)
raster::writeRaster(pred_region_interp_surv, here::here("data", "processed_data", "predictions", "predictions_region_interp_surv"), overwrite=TRUE)
raster::writeRaster(pred_se_region_interp_surv, here::here("data", "processed_data", "predictions", "predictions_se_region_interp_surv"), overwrite=TRUE)





#---------------------------------------------------------------- BLOCK ABUNDANCE ESTIMATE ----------------------------------------------------------

# abondance on surveyed area
sum(data_surv$abundance) #148

# global density on surveyed area (total nb observed individuals after correction / total surveyed area)
density = sum(data_surv$abundance) / (sum(data_surv$area_surveyed_m2) * 10^-6) #2.5 indiv / km2

# abundance on surveyed block
surveyed_block_abundance = density * surv_block_area # 1818 indiv ***why this number so high ? -> high block area compared to surveyed area ?

# abondance on surveyed pixels (1733 is the nb of surveyed pixels and 0.25 km2 is the area of each pixel)
surveyed_pixels_abundance = density * 1733 * 0.25 # 1104 indiv 


