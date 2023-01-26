
#load all functions
devtools::load_all()


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




#read dugong density raster
density_dugong = raster::raster(here::here("data", "processed_data", "density", "dugong_density.grd"))


#read dugong abundance and area surveyed rasters
abundance_dugong = raster::raster(here::here("data", "processed_data", "abundance", "dugong_abundance.grd"))
area_surveyed = raster::raster(here::here("data", "processed_data", "abundance", "area_surveyed.grd"))

#stack predictors
stack_pred = raster::stack(percent_rubble, percent_coral_algae, percent_microalgal_mats, percent_seagrass, percent_sand, percent_rock, 
                           percent_back_reef_slope, percent_deep_lagoon, percent_inner_reef_flat, percent_outer_reef_flat, 
                           percent_plateau, percent_reef_crest, percent_reef_slope, percent_shallow_lagoon, percent_terrestrial_reef_flat,
                           turbidity, travel_time, population, pop_density, mpa_type, mpa_type_no_take_only,
                           mpa_pres, dist_land, dist_reef, depth, dist_seagrass)

#remove upper end corner
r = raster::crop(turbidity,  raster::extent(c( 350000, 380000, 310000, 340000)))
pol = raster::rasterToPolygons(r)
stack_pred = raster::mask(stack_pred, pol, inverse = T)

#stack all rasters
stack = raster::stack(density_dugong, abundance_dugong, area_surveyed, percent_rubble, percent_coral_algae, percent_microalgal_mats, percent_seagrass, percent_sand, percent_rock, 
                      percent_back_reef_slope, percent_deep_lagoon, percent_inner_reef_flat, percent_outer_reef_flat, 
                      percent_plateau, percent_reef_crest, percent_reef_slope, percent_shallow_lagoon, percent_terrestrial_reef_flat,
                      turbidity, travel_time, population, pop_density, mpa_type, mpa_type_no_take_only,
                      mpa_pres, dist_land, dist_reef, depth, dist_seagrass)


#mask with dugong raster
r = raster::mask(stack, density_dugong)

#save(r, file = here::here("3-modeling.RData"))





#Modelling framework
data_model = r %>% 
  raster::as.data.frame(xy = T)

#CHECK CORRELATED VARIABLES
#Correlogram
data_corr = data_model %>%
  dplyr::select_if(is.double) %>% 
  dplyr::select(-c(x, y,density_Dugong_certain_probable, n_Dugong_certain_probable, area_surveyed_m2)) %>% 
  tidyr::drop_na()

M <- cor(data_corr)

corrplot::corrplot(M, method="circle")
corrplot::corrplot(M, method="color")

#pop_dens and dist_land are very correlated, removing pop_dens
#dist_seagrass and dist_land are very correlated, removing dist_land
#terrestrial_reef_flat and seagrass are correlated, removing terrestrial_reef_flat
data_model = data_model %>%
  #removing correlated variables
  dplyr::select(-c(pop_dens, dist_land, terrestrial_reef_flat)) %>% 
  #Keep mpa pres as MPA variable (removing the others), removing population which is not correct and better replaced by turbidity
  dplyr::select(-c(mpa_type_no_take_only_mpa_type, mpa_type_mpa_type, population)) %>%
  #removing depth higher than 0 
  dplyr::filter(!depth > 0) %>%
  #Transforming a few variables
  # dplyr::mutate(travel_time = log10(travel_time + 1),
  #               turbidity = log10(turbidity + 1),
  #               depth = abs(depth),
  #               depth = log10(depth + 1),
  #               dist_reef = log10(dist_reef + 1)) %>% 
  dplyr::mutate(depth = abs(depth)) %>% 
  #rename
  dplyr::rename("density" = density_Dugong_certain_probable,
                "abundance" = n_Dugong_certain_probable,
                "mpa_pres" = mpa_pres_mpa_pres) %>% 
  #create abundance rounded
  dplyr::mutate(abundance_round = round(abundance)) 


# fit GAM desnity
mod <- mgcv::gam(density ~  s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                   s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                   s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass), data = data_model, method="REML")
summary(mod)
plot(mod, pages = 1)




# fit GAM abudance with offset
library(mgcv) #for tw() to work
mod_tw <- gam(abundance ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass) +
                offset(log(area_surveyed_m2)), 
              family = tw(), data = data_model, method="REML") 
summary(mod_tw)
plot(mod_tw, pages = 1)    
gam.check(mod_tw)

mod_nb <- gam(abundance ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass) +
                offset(log(area_surveyed_m2)), 
              family = nb(), data = data_model, method="REML")
summary(mod_nb)
plot(mod_nb, pages = 1)    
gam.check(mod_nb)

mod_zip <- gam(abundance_round ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                 s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                 s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass) +
                 offset(log(area_surveyed_m2)), 
               family = ziP(), data = data_model, method="REML") 
summary(mod_zip)
plot(mod_zip, pages = 1)    
gam.check(mod_zip)





mod_reduced <- gam(abundance ~ s(deep_lagoon) + s(turbidity) +  s(dist_seagrass) + s(depth) +
                     offset(log(area_surveyed_m2)), 
                   family = tw(), data = data_model, method="REML")

summary(mod_reduced)
plot(mod_reduced, pages = 1, scale=0)       

#s(dat[,C2[1,i]],k=4,bs="ts")+s(dat[,C2[2,i]],k=4,bs="ts")+offset(log(area_km2)), family=tw(), data=dat, method="REML"))








# prediction 

# on surveyed area (Ind / 0.25 km2)
pred = raster::predict(stack, mod_reduced, type="response", const=data.frame(area_surveyed_m2 = c(1)))  #constant does not matter
plot(pred)
raster::cellStats(pred, sum)

#on region (Ind / 0.25 km2)
pred = raster::predict(stack_pred2, mod_reduced, type="response", const=data.frame(area_surveyed_m2 = c(1))) #constant does not matter
plot(pred)
raster::cellStats(pred, sum)






#TODO
#crossval
#residual diagnostocs + autocorrelagram
#model selection (distrib, covar)
#cv
#dsm extra to look for extrap ?
#dsm vs gam package
