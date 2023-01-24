
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




#read dugong raster
density_dugong = raster::raster(here::here("data", "processed_data", "density", "dugong_density.grd"))


#stack all rasters
r = raster::stack(density_dugong, percent_rubble, percent_coral_algae, percent_microalgal_mats, percent_seagrass, percent_sand, percent_rock, 
                  percent_back_reef_slope, percent_deep_lagoon, percent_inner_reef_flat, percent_outer_reef_flat, 
                  percent_plateau, percent_reef_crest, percent_reef_slope, percent_shallow_lagoon, percent_terrestrial_reef_flat,
                  turbidity, travel_time, population, pop_density, mpa_type, mpa_type_no_take_only,
                  mpa_pres, dist_land, dist_reef, depth, dist_seagrass)


#mask with dugong raster
r = raster::mask(r, density_dugong)

#save(r, file = here::here("3-modeling.RData"))





#Modelling framework
data_model = r %>% 
  raster::as.data.frame(xy = T)

#CHECK CORRELATED VARIABLES
#Correlarogram
data_corr = data_model %>%
  dplyr::select_if(is.double) %>% 
  dplyr::select(-c(x, y)) %>% 
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
  dplyr::mutate(travel_time = log10(travel_time + 1),
                turbidity = log10(turbidity + 1),
                depth = abs(depth),
                depth = log10(depth + 1),
                dist_reef = log10(dist_reef + 1)) %>% 
  #rename
  dplyr::rename("density" = density_Dugong_certain_probable,
                "mpa_pres" = mpa_pres_mpa_pres)



mod <- mgcv::gam(density ~  s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
           s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
           s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass), data = data_model, method="REML")

plot(mod, pages = 1)
