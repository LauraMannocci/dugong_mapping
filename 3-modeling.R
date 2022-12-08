
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

travel_time = raster::raster(here::here("data", "processed_data", "predictors", "travel_time.grd"))
population = raster::raster(here::here("data", "processed_data", "predictors", "population.grd")) #unsure
pop_density = raster::raster(here::here("data", "processed_data", "predictors", "pop_density.grd")) #unsure
mpa_type = raster::raster(here::here("data", "processed_data", "predictors", "mpa_type.grd")) #-1 no mpa / 1 no-take mpa / 2 partial mpa
mpa_type_no_take_only = raster::raster(here::here("data", "processed_data", "predictors", "mpa_type_no_take_only.grd")) #-1 no mpa / 1 no-take mpa
mpa_pres = raster::raster(here::here("data", "processed_data", "predictors", "mpa_pres.grd")) #1 = mpa presence / 0 = absence
dist_land = raster::raster(here::here("data", "processed_data", "predictors", "dist_land.grd"))
dist_reef = raster::raster(here::here("data", "processed_data", "predictors", "dist_reef.grd"))
 


#read dugong raster
density_dugong = raster::raster(here::here("data", "processed_data", "density", "dugong_density.grd"))


#stack all rasters
r = raster::stack(density_dugong, percent_rubble, percent_coral_algae, percent_microalgal_mats, percent_seagrass,
                  percent_sand, percent_rock, 
                  percent_back_reef_slope, percent_deep_lagoon, percent_inner_reef_flat, percent_outer_reef_flat, 
                  percent_plateau, percent_reef_crest, percent_reef_slope, percent_shallow_lagoon, percent_terrestrial_reef_flat,
                  travel_time, population, pop_density, mpa_type, mpa_type_no_take_only,
                  mpa_pres, dist_land, dist_reef)


#mask with dugong raster
r = raster::mask(r, density_dugong)

save(r, file = here::here("3-modeling.RData"))


