
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
travel_time = raster::raster(here::here("data", "processed_data", "predictors", "travel_time.grd"))
population = raster::raster(here::here("data", "processed_data", "predictors", "population.grd")) #unsure
pop_density = raster::raster(here::here("data", "processed_data", "predictors", "pop_density.grd")) #unsure
mpa_type = raster::raster(here::here("data", "processed_data", "predictors", "mpa_type.grd")) #-1 no mpa / 1 no-take mpa / 2 partial mpa
mpa_type_no_take_only = raster::raster(here::here("data", "processed_data", "predictors", "mpa_type_no_take_only.grd")) #-1 no mpa / 1 no-take mpa
mpa_pres = raster::raster(here::here("data", "processed_data", "predictors", "mpa_pres.grd")) #1 = mpa presence / 0 = absence
dist_land = raster::raster(here::here("data", "processed_data", "predictors", "dist_land.grd"))
dist_reef = raster::raster(here::here("data", "processed_data", "predictors", "dist_reef.grd"))
depth = raster::raster(here::here("data", "processed_data", "predictors", "depth.grd"))
dist_seagrass = raster::raster(here::here("data", "processed_data", "predictors", "dist_seagrass.grd"))



#read dugong abundance and area surveyed rasters
abundance_dugong = raster::raster(here::here("data", "processed_data", "abundance", "dugong_abundance.grd"))
area_surveyed = raster::raster(here::here("data", "processed_data", "abundance", "area_surveyed.grd"))



#stack predictors
stack_pred = raster::stack(percent_rubble, percent_coral_algae, percent_microalgal_mats, percent_seagrass, percent_sand, percent_rock, 
                           percent_back_reef_slope, percent_deep_lagoon, percent_inner_reef_flat, percent_outer_reef_flat, 
                           percent_plateau, percent_reef_crest, percent_reef_slope, percent_shallow_lagoon, percent_terrestrial_reef_flat,
                           turbidity, travel_time, population, pop_density, mpa_type, mpa_type_no_take_only,
                           mpa_pres, dist_land, dist_reef, depth, dist_seagrass)


#remove upper left corner of predictors stack (east coast) based on turbidity raster
r = raster::crop(turbidity,  raster::extent(c( 350000, 380000, 310000, 340000)))
pol = raster::rasterToPolygons(r)
stack_pred = raster::mask(stack_pred, pol, inverse = T)



#stack all rasters
stack = raster::stack(abundance_dugong, area_surveyed, percent_rubble, percent_coral_algae, percent_microalgal_mats, percent_seagrass, percent_sand, percent_rock, 
                      percent_back_reef_slope, percent_deep_lagoon, percent_inner_reef_flat, percent_outer_reef_flat, 
                      percent_plateau, percent_reef_crest, percent_reef_slope, percent_shallow_lagoon, percent_terrestrial_reef_flat,
                      turbidity, travel_time, population, pop_density, mpa_type, mpa_type_no_take_only,
                      mpa_pres, dist_land, dist_reef, depth, dist_seagrass)


#mask this stack with dugong abundance raster so that NAs are assigned where there are NAs in dugong raster
#this is the surveyed stack
stack_surv = raster::mask(stack, abundance_dugong)



#quick stats
raster::cellStats(stack_surv$n_Dugong_certain_probable, sum) # 200.62 counted individuals (after availability correction)
raster::cellStats(stack_surv$area_surveyed_m2, sum)*10^-6 # 101.6526 km2 surface surveyed
raster::freq(stack_surv$n_Dugong_certain_probable, value=0) #1859 cells with zeros 
raster::freq(stack_surv$n_Dugong_certain_probable, value=NA) #38865 cells with nas
raster::ncell(stack_surv$n_Dugong_certain_probable) #40762 total nb of cells
raster::ncell(stack_surv$n_Dugong_certain_probable) - raster::freq(stack_surv$n_Dugong_certain_probable, value=NA) #1897 non na cells
#proportion of cells with zeros 1859/1897 = 0.98

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



#save(r, file = here::here("3-modeling.RData"))

#---------------------------------------------------------------- CHECKING CORRELATIONS ----------------------------------------------------------

#Correlogram
data_corr = data_surv %>%
  dplyr::select_if(is.double) %>% 
  dplyr::select(-c(x, y, n_Dugong_certain_probable, area_surveyed_m2)) %>% 
  tidyr::drop_na()

coef <- cor(data_corr)

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
  #Transforming depth
  dplyr::mutate(depth = abs(depth)) %>% 
  #renaming
  dplyr::rename("abundance" = n_Dugong_certain_probable,
                "mpa_pres" = mpa_pres_mpa_pres) %>% 
  #create abundance rounded (for zip models)
  dplyr::mutate(abundance_round = round(abundance)) 





#---------------------------------------------------------------- MODEL FIT ----------------------------------------------------------
#useful wiki: https://osf.io/wgc4f/wiki/home/



####################  fit full abudance GAM with area surveyed as offset and 
# automated term selection thanks to select = T (https://stats.stackexchange.com/questions/405129/model-selection-for-gam-in-r)
# unlimited k

library(mgcv)

#tw distrib
mod_tw <- fit_full_abundance_model("tw", data_surv)

#nb distrib
mod_nb <- fit_full_abundance_model("nb", data_surv)

#zip distrib
mod_zip <- fit_full_abundance_model("zip", data_surv)




#################### model selection

# based on AIC
AIC(mod_zip) #best AIC but non parcimonious model 
AIC(mod_tw)
AIC(mod_nb) #second best AIC 


# based on residual diagnostics -> tw




#################### fit zip reduced model (removing NS variables)

mod_zip_reduced <- gam(abundance_round ~ s(seagrass) + s(deep_lagoon) + s(outer_reef_flat) +  s(reef_crest) + 
             s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass) +
             offset(log(area_surveyed_m2)), family = ziP(), data = data_surv, method="REML")
#too many covariates, non parcimonious





#################### fit nb reduced model (removing NS variables)

mod_nb_reduced <- gam(abundance ~ s(deep_lagoon)+ s(turbidity) + s(dist_seagrass) +
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






#################### fit tw reduced model (removing NS variables - sequentially) ********* best model **********

mod_tw_reduced <- gam(abundance ~ s(deep_lagoon)+ s(turbidity) + s(dist_seagrass) + s(reef_slope) + 
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




####################  fit full abudance GAM with area surveyed as offset and 
# automated term selection thanks to select = T (https://stats.stackexchange.com/questions/405129/model-selection-for-gam-in-r)
# k set to 5

#tw distrib
mod_tw_k5 <- fit_full_abundance_model_k5("tw", data_surv)

#nb distrib
mod_nb_k5 <- fit_full_abundance_model_k5("nb", data_surv)

#zip distrib
mod_zip_k5 <- fit_full_abundance_model_k5("zip", data_surv)


# model selection
AIC(mod_zip_k5) 
AIC(mod_tw_k5)
AIC(mod_nb_k5) #best AIC -> select nb





#################### fit nb reduced model k = 5 (removing NS variables)

mod_nb_reduced_k5 <- gam(abundance ~ s(deep_lagoon, k = 5)+ s(shallow_lagoon, k = 5) + s(turbidity, k = 5) + s(dist_seagrass, k = 5) +
                        offset(log(area_surveyed_m2)), family = nb(), data = data_surv, method="REML") 


#summary
sink(here::here("outputs/models/summary_reduced_nb_k5.txt"))
print(summary(mod_nb_reduced_k5))
sink()

#terms plot
png(here::here("outputs/models/terms_plot_reduced_nb_k5.png"))
plot(mod_nb_reduced_k5, pages = 1) 
dev.off()

#diagnostics plot
png(here::here("outputs/models/diagnostics_plot_reduced_nb_k5.png"))
par(mfrow=c(2,2))
gam.check(mod_nb_reduced_k5) 
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


# predictions and standard errors on surveyed area (Ind / 0.25 km2)
pred_surveyed = raster::predict(stack_surv, mod_tw_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = c(250000)))  #offset = 500x500 = 250000 km2
pred_se_surveyed = raster::predict(stack_surv, mod_tw_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = c(250000)))  #offset = 500x500 = 250000 km2

map_abundance_predictions_surveyed(maplatlon_proj, pred_surveyed, pred_se_surveyed)
raster::cellStats(pred_surveyed, sum) #165.4 ind

# write rasters
raster::writeRaster(pred_surveyed, here::here("data", "processed_data", "predictions", "predictions_surveyed"), overwrite=TRUE)
raster::writeRaster(pred_se_surveyed, here::here("data", "processed_data", "predictions", "predictions_se_surveyed"), overwrite=TRUE)



             
# predictions and standard errors on region (Ind / 0.25 km2)
pred_region = raster::predict(stack_pred, mod_tw_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = c(250000))) #offset = 500x500 = 250000 km2
pred_se_region = raster::predict(stack_pred, mod_tw_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = c(250000))) #offset = 500x500 = 250000 km2

map_abundance_predictions_region(maplatlon_proj, pred_region, pred_se_region)
raster::cellStats(pred_region, sum) #1222.9 ind

# write rasters
raster::writeRaster(pred_region, here::here("data", "processed_data", "predictions", "predictions_region"), overwrite=TRUE)
raster::writeRaster(pred_se_region, here::here("data", "processed_data", "predictions", "predictions_se_region"), overwrite=TRUE)




#---------------------------------------------------------------- MODEL PERFORMANCE ----------------------------------------------------------


#### correlations between observations and predictions - all data -

#mask dugong raster to get same NAs
abundance_dugong2 = raster::mask(abundance_dugong, pred_surveyed)
raster::freq(abundance_dugong2, value = NA)
raster::freq(pred_surveyed, value = NA) #same na numbers

#transform to vectors to caculate speartman coef
obs = as.vector(abundance_dugong2)
obs = obs[!is.na(obs)]  

pred = as.vector(pred_surveyed)
pred = pred[!is.na(pred)]  

cor(obs, pred, method = 'spearman', use = "everything") #0.17

#plot
plot(obs, pred)



#### correlations between observations and predictions - non zeros -

#replace nas with zeros
abundance_dugong2[abundance_dugong2 == 0] <- NA

#mask prediction raster 
pred_surveyed2 = raster::mask(pred_surveyed, abundance_dugong2)
  
  
#transform to vectors to caculate speartman coef
obs = as.vector(abundance_dugong2)
obs = obs[!is.na(obs)]  

pred = as.vector(pred_surveyed2)
pred = pred[!is.na(pred)]  

cor(obs, pred, method = 'spearman', use = "everything") #0.28
#cor.test(obs, pred, method = 'spearman', use = "everything") 

#plot
plot(obs, pred)





#---------------------------------------------------------------- EXTRAPOLATION ----------------------------------------------------------


# install dsm extra
# remotes::install_github("densitymodelling/dsmextra", dependencies = TRUE)


# Other required libraries
library(dsmextra)     # Extrapolation toolkit for ecological models
library(raster)       # Geographic data analysis and modeling
library(tidyverse)    # Packages for data science
library(magrittr)     # Pipe operator


# Define environmental covariates of interest
covariates <- c("deep_lagoon", "turbidity", "dist_seagrass", "reef_slope")


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
stack_pred_interp$deep_lagoon[stack_pred_interp$deep_lagoon > max(segs$deep_lagoon, na.rm = T) | stack_pred_interp$deep_lagoon < min(segs$deep_lagoon, na.rm = T)] <- NA
stack_pred_interp$turbidity[stack_pred_interp$turbidity > max(segs$turbidity, na.rm = T) | stack_pred_interp$turbidity < min(segs$turbidity, na.rm = T)] <- NA
stack_pred_interp$dist_seagrass[stack_pred_interp$dist_seagrass > max(segs$dist_seagrass, na.rm = T) | stack_pred_interp$dist_seagrass < min(segs$dist_seagrass, na.rm = T)] <- NA
stack_pred_interp$reef_slope[stack_pred_interp$reef_slope > max(segs$reef_slope, na.rm = T) | stack_pred_interp$reef_slope < min(segs$reef_slope, na.rm = T)] <- NA


# predictions and standard errors on region (Ind / 0.25 km2) in univariate interpolation situations 
pred_region_interp = raster::predict(stack_pred_interp, mod_tw_reduced, type="response", se.fit = TRUE, index = 1, const=data.frame(area_surveyed_m2 = c(250000))) #offset = 500x500 = 250000 km2
pred_se_region_interp = raster::predict(stack_pred_interp, mod_tw_reduced, type="response", se.fit = TRUE, index = 2, const=data.frame(area_surveyed_m2 = c(250000))) #offset = 500x500 = 250000 km2

map_abundance_predictions_region_interp(maplatlon_proj, pred_region_interp, pred_se_region_interp)
raster::cellStats(pred_region_interp, sum) #1215.076 ind

#restrict predictions to surveyed bloc
pred_region_interp_surv = raster::mask(pred_region_interp, surv_block)
pred_se_region_interp_surv = raster::mask(pred_se_region_interp, surv_block)

map_abundance_predictions_region_interp_surv(maplatlon_proj, pred_region_interp_surv, pred_se_region_interp_surv)
raster::cellStats(pred_region_interp_surv, sum) #1016.822 ind


# write rasters
raster::writeRaster(pred_region_interp_surv, here::here("data", "processed_data", "predictions", "predictions_region_interp_surv"), overwrite=TRUE)
raster::writeRaster(pred_se_region_interp_surv, here::here("data", "processed_data", "predictions", "predictions_se_region_interp_surv"), overwrite=TRUE)





#---------------------------------------------------------------- BLOCK ABUNDANCE ESTIMATE ----------------------------------------------------------

# abondance on surveyed area
sum(data_surv$abundance) #200.6294

# global density on surveyed area (total nb observed individuals after correction / total surveyed area)
density = sum(data_surv$abundance) / (sum(data_surv$area_surveyed_m2) * 10^-6) #2.01 indiv / km2

# abundance on surveyed block
surveyed_block_abundance = density * surv_block_area # 1520.5 indiv ***why this number so high ? -> high block area compared to surveyed area ?

# abondance on surveyed pixels (1897 is the nb of surveyed pixels and 0.25 km2 is the area of each pixel)
surveyed_pixels_abundance = density * 1897 * 0.25 # 957.7129 indiv 




#TODO

#remove duplicated area north for accurate abundance estimation or keep all data for best density modeling (dont matter if duplicated areas) 
#- > focus study on modeling ***relative*** density rather than n estimation

#crossval (spatial cv en utilisant la date ?) - cf RMSS cleguer ?
#+++ie pred vs observed nonparametric Spearman rank correlation test per block (eg poe, s poe, n poe) cf becker

#all west coast prediction +

#cv block estimates see buckland

#dsm vs gam package ? cf pour dsm https://github.com/pjbouchet/sousa_dsm/blob/main/sousa_dsm_main.R

#change seagrass variable - andrefouet et al 2021

#msp to protect 50 % of pop ?

#add random effect transect to account for double survey and autor between pixels of same transect  - cf raudino et al 2023


#relevant papers

#becker et al 2020
#raudino et al 2023