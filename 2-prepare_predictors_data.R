
#load all functions
devtools::load_all()




######################################## DEFINE STUDY AREA AND CORRESPONDING RASTERS ########################################

#Define study area coordinates
#big region
lat1 =  -21.2 ; lat2 = -21.9
lon1 = 164.8 ; lon2 = 165.8

#study area raster resolution in meters
raster_res_m = 500

# make study area raster in lat lon  (resolution 0.01 degrees) 
rast_latlon = make_area_raster_latlon(lat1, lon1, lat2, lon2, 0.01)

# make study area raster projected to lambert New caledonia in meters with given resolution in meters (resolution 500 meters) 
rast_xy = make_area_raster_xy(lat1, lon1, lat2, lon2, raster_res_m)


#open street map
maplatlon = osm_map(lat1, lon1, lat2, lon2)





######################################## DISTANCE TO MAINLAND ########################################

# Read New Caledonia coral shapefile (for distances computation)
coral_millenium = read_original_coralnc_millenium()

# Make distance to mainland raster
dist_land = make_dist_to_land_raster(coral_millenium, rast_xy)

# Map distance to mainland raster
map_dist_to_land(dist_land)

# write raster
raster::writeRaster(dist_land, here::here("data", "processed_data", "predictors", "dist_land.grd"), overwrite=TRUE)




######################################## CORAL (ALLEN ATLAS) ########################################

#read Allen coral benthic polygon
coral = read_convert_coral_benthic(lon1, lon2, lat2, lat1)

#Map Allen coral polygons on OSM
map_allen_coral_osm(maplatlon, coral, lon1, lon2, lat1, lat2)

#Reproject
coral_projected = sp::spTransform(coral, sp::CRS("+init=epsg:3163"))



#setup parallel processing

library(foreach)
library(raster)

#1-create the cluster
my.cluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = "PSOCK")

#2-register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#make coverage raster for each habitat type
percent_rubble = make_raster_coral_benthic_type(coral_projected, rast_xy, "Rubble")
percent_coral_algae = make_raster_coral_benthic_type(coral_projected, rast_xy, "Coral_algae")
percent_microalgal_mats = make_raster_coral_benthic_type(coral_projected, rast_xy, "Microalgal_mats")
percent_seagrass = make_raster_coral_benthic_type(coral_projected, rast_xy, "Seagrass")
percent_sand = make_raster_coral_benthic_type(coral_projected, rast_xy, "Sand")
percent_rock = make_raster_coral_benthic_type(coral_projected, rast_xy, "Rock")


#extend all rasters to study area raster
percent_rubble = extend_raster(percent_rubble, rast_xy)
percent_coral_algae = extend_raster(percent_coral_algae, rast_xy)
percent_microalgal_mats = extend_raster(percent_microalgal_mats, rast_xy)
percent_seagrass = extend_raster(percent_seagrass, rast_xy)
percent_sand = extend_raster(percent_sand, rast_xy)
percent_rock = extend_raster(percent_rock, rast_xy)


# mask based on dist to mainland raster
percent_rubble = mask_with_land(percent_rubble, dist_land)
percent_coral_algae = mask_with_land(percent_coral_algae, dist_land)
percent_microalgal_mats = mask_with_land(percent_microalgal_mats, dist_land)
percent_seagrass = mask_with_land(percent_seagrass, dist_land)
percent_sand = mask_with_land(percent_sand, dist_land)
percent_rock = mask_with_land(percent_rock, dist_land)


# assign zeros where habitat/geomorpho type is absent
percent_rubble = assign_zeros_where_absent_type(percent_rubble, geom_projected, rastxy) 
percent_coral_algae = assign_zeros_where_absent_type(percent_coral_algae, geom_projected, rastxy) 
percent_microalgal_mats = assign_zeros_where_absent_type(percent_microalgal_mats, geom_projected, rastxy) 
percent_seagrass = assign_zeros_where_absent_type(percent_seagrass, geom_projected, rastxy) 
percent_sand = assign_zeros_where_absent_type(percent_sand, geom_projected, rastxy) 
percent_rock = assign_zeros_where_absent_type(percent_rock, geom_projected, rastxy) 


#map
map_coral_habitat(percent_rubble, "percent_rubble")
map_coral_habitat(percent_coral_algae, "percent_coral_algae")
map_coral_habitat(percent_microalgal_mats, "percent_microalgal_mats")
map_coral_habitat(percent_seagrass, "percent_seagrass")
map_coral_habitat(percent_sand, "percent_sand")
map_coral_habitat(percent_rock, "percent_rock")


#write rasters
raster::writeRaster(percent_rubble, here::here("data", "processed_data", "predictors", "percent_rubble.grd"), overwrite=TRUE)
raster::writeRaster(percent_coral_algae, here::here("data", "processed_data", "predictors", "percent_coral_algae.grd"), overwrite=TRUE)
raster::writeRaster(percent_microalgal_mats, here::here("data", "processed_data", "predictors", "percent_microalgal_mats.grd"), overwrite=TRUE)
raster::writeRaster(percent_seagrass, here::here("data", "processed_data", "predictors", "percent_seagrass.grd"), overwrite=TRUE)
raster::writeRaster(percent_sand, here::here("data", "processed_data", "predictors", "percent_sand.grd"), overwrite=TRUE)
raster::writeRaster(percent_rock, here::here("data", "processed_data", "predictors", "percent_rock.grd"), overwrite=TRUE)





######################################## GEOMORPHO (ALLEN ATLAS) ########################################


#read Allen coral geomorpho polygon
geom = read_convert_coral_geomorpho(lon1, lon2, lat2, lat1)

#Map Allen geomorpho polygons on OSM
map_allen_geomorpho_osm(maplatlon, geom, lon1, lon2, lat1, lat2)

#Reproject
geom_projected = sp::spTransform(geom, sp::CRS("+init=epsg:3163"))



#setup parallel processing

library(foreach)
library(raster)

#1-create the cluster
my.cluster <- parallel::makeCluster(
  parallel::detectCores() - 1,
  type = "PSOCK")

#2-register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#make coverage raster for each geomorpho type
percent_back_reef_slope = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Back_Reef_Slope")
percent_deep_lagoon = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Deep_Lagoon")
percent_inner_reef_flat = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Inner_Reef_Flat")
percent_outer_reef_flat = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Outer_Reef_Flat")
percent_plateau = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Plateau")
percent_reef_crest = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Reef_Crest")
percent_reef_slope = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Reef_Slope")
percent_shallow_lagoon = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Shallow_Lagoon")
percent_terrestrial_reef_flat = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Terrestrial_Reef_Flat")



#extend all rasters to study area raster
percent_back_reef_slope = extend_raster(percent_back_reef_slope, rast_xy)
percent_deep_lagoon = extend_raster(percent_deep_lagoon, rast_xy)
percent_inner_reef_flat = extend_raster(percent_inner_reef_flat, rast_xy)
percent_outer_reef_flat = extend_raster(percent_outer_reef_flat, rast_xy)
percent_plateau = extend_raster(percent_plateau, rast_xy)
percent_reef_crest = extend_raster(percent_reef_crest, rast_xy)
percent_reef_slope = extend_raster(percent_reef_slope, rast_xy)
percent_shallow_lagoon = extend_raster(percent_shallow_lagoon, rast_xy)
percent_terrestrial_reef_flat = extend_raster(percent_terrestrial_reef_flat, rast_xy)



# mask based on dist to mainland raster
percent_back_reef_slope = mask_with_land(percent_back_reef_slope, dist_land)
percent_deep_lagoon = mask_with_land(percent_deep_lagoon, dist_land)
percent_inner_reef_flat = mask_with_land(percent_inner_reef_flat, dist_land)
percent_outer_reef_flat = mask_with_land(percent_outer_reef_flat, dist_land)
percent_plateau = mask_with_land(percent_plateau, dist_land)
percent_reef_crest = mask_with_land(percent_reef_crest, dist_land)
percent_reef_slope = mask_with_land(percent_reef_slope, dist_land)
percent_shallow_lagoon = mask_with_land(percent_shallow_lagoon, dist_land)
percent_terrestrial_reef_flat = mask_with_land(percent_terrestrial_reef_flat, dist_land)


# assign zeros where habitat/geomorpho type is absent
percent_back_reef_slope = assign_zeros_where_absent_type(percent_back_reef_slope, geom_projected, rastxy) 
percent_deep_lagoon = assign_zeros_where_absent_type(percent_deep_lagoon, geom_projected, rastxy) 
percent_inner_reef_flat = assign_zeros_where_absent_type(percent_inner_reef_flat, geom_projected, rastxy) 
percent_outer_reef_flat = assign_zeros_where_absent_type(percent_outer_reef_flat, geom_projected, rastxy) 
percent_plateau = assign_zeros_where_absent_type(percent_plateau, geom_projected, rastxy) 
percent_reef_crest = assign_zeros_where_absent_type(percent_reef_crest, geom_projected, rastxy) 
percent_reef_slope = assign_zeros_where_absent_type(percent_reef_slope, geom_projected, rastxy) 
percent_shallow_lagoon = assign_zeros_where_absent_type(percent_shallow_lagoon, geom_projected, rastxy) 
percent_terrestrial_reef_flat = assign_zeros_where_absent_type(percent_terrestrial_reef_flat, geom_projected, rastxy) 

  

#map                
map_coral_habitat(percent_back_reef_slope, "percent_back_reef_slope")
map_coral_habitat(percent_deep_lagoon, "percent_deep_lagoon")
map_coral_habitat(percent_inner_reef_flat, "percent_inner_reef_flat")
map_coral_habitat(percent_outer_reef_flat, "percent_outer_reef_flat")
map_coral_habitat(percent_plateau, "percent_plateau")
map_coral_habitat(percent_reef_crest, "percent_reef_crest")
map_coral_habitat(percent_reef_slope, "percent_reef_slope")
map_coral_habitat(percent_shallow_lagoon, "percent_shallow_lagoon")
map_coral_habitat(percent_terrestrial_reef_flat, "percent_back_reef_slope")


#write rasters     
raster::writeRaster(percent_back_reef_slope, here::here("data", "processed_data", "predictors", "percent_back_reef_slope.grd"), overwrite=TRUE)
raster::writeRaster(percent_deep_lagoon, here::here("data", "processed_data", "predictors", "percent_deep_lagoon.grd"), overwrite=TRUE)
raster::writeRaster(percent_inner_reef_flat, here::here("data", "processed_data", "predictors", "percent_inner_reef_flat.grd"), overwrite=TRUE)
raster::writeRaster(percent_outer_reef_flat, here::here("data", "processed_data", "predictors", "percent_outer_reef_flat.grd"), overwrite=TRUE)
raster::writeRaster(percent_plateau, here::here("data", "processed_data", "predictors", "percent_plateau.grd"), overwrite=TRUE)
raster::writeRaster(percent_reef_crest, here::here("data", "processed_data", "predictors", "percent_reef_crest.grd"), overwrite=TRUE)
raster::writeRaster(percent_reef_slope, here::here("data", "processed_data", "predictors", "percent_reef_slope.grd"), overwrite=TRUE)
raster::writeRaster(percent_shallow_lagoon, here::here("data", "processed_data", "predictors", "percent_shallow_lagoon.grd"), overwrite=TRUE)
raster::writeRaster(percent_terrestrial_reef_flat, here::here("data", "processed_data", "predictors", "percent_terrestrial_reef_flat.grd"), overwrite=TRUE)




######################################## TRAVEL TIME ########################################

# load travel time raster from FB (corresponding to travel times from Nouméa (seconds) in 1 x 1 km cell)
# and resample
travel_time = load_travel_time_raster(rast_xy) # **** reextract based on full survey coverage ??

# mask based on dist to mainland raster
travel_time = mask_with_land(travel_time, dist_land)

# Map original travel time
map_travel_time(travel_time)

# write raster
raster::writeRaster(travel_time, here::here("data", "processed_data", "predictors", "travel_time.grd"), overwrite=TRUE)




######################################## POPULATION ########################################

# Make population raster
population = make_population_raster(rast_xy) 

# mask based on dist to mainland raster
population = mask_with_land(population, dist_land)

# Map population raster
map_population(population)

# write raster
raster::writeRaster(population, here::here("data", "processed_data", "predictors", "population.grd"), overwrite=TRUE)








######################################## POPULATION DENSITY ########################################

# Prepare population density raster (worldwide data, 2020 year and 5 x 5 km buffer)
pop_density = make_pop_density(rast_xy, lon1, lon2, lat1, lat2) 

# mask based on dist to mainland raster
pop_density = mask_with_land(pop_density, dist_land)

# Map population desnity raster
map_pop_density(pop_density)

# write raster
raster::writeRaster(pop_density, here::here("data", "processed_data", "predictors", "pop_density.grd"), overwrite=TRUE)






######################################## DEPTH ########################################

# read depth from Jean roger (IRD) and resample to study area
depth = read_resample_depth(rast_latlon, rast_xy)

# mask based on dist to mainland raster
depth = mask_with_land(depth, dist_land)

# Map depth raster
map_depth(depth)

# write raster
raster::writeRaster(depth, here::here("data", "processed_data", "predictors", "depth.grd"), overwrite=TRUE)








######################################## MPA ########################################

# read mpa polygon
mpa = read_mpanc()


# Make mpa type raster
library(raster) #for good functionning of levels in function
mpa_type = make_mpa_type_raster(rast_xy) #ran outside function to avoid bug 

# mask based on dist to mainland raster
mpa_type = mask_with_land(mpa_type, dist_land)

# Map mpa type
map_mpa_type(mpa_type)

# write raster
raster::writeRaster(mpa_type, here::here("data", "processed_data", "predictors", "mpa_type.grd"), overwrite=TRUE)




# Make mpa type raster no-take only (for msp)
library(raster) #for good functionning of levels in function
mpa_type_no_take_only = make_mpa_type_raster_no_take_only(rast_latlon) #ran outside function to avoi dbug

# mask based on dist to mainland raster
mpa_type_no_take_only = mask_with_land(mpa_type_no_take_only, dist_land)

# Map mpa type
map_mpa_type_no_take_only(mpa_type_no_take_only)

# write raster
raster::writeRaster(mpa_type_no_take_only, here::here("data", "processed_data", "predictors", "mpa_type_no_take_only.grd"), overwrite=TRUE)




# Make mpa presence raster
library(raster) #for good functionning of levels in function
mpa_pres = make_mpa_pres_raster(rast_xy) #ran outside function to avoi dbug

# mask based on dist to mainland raster
mpa_pres = mask_with_land(mpa_pres, dist_land)

# Map mpa type
map_mpa_pres(mpa_pres)

# write raster
raster::writeRaster(mpa_pres, here::here("data", "processed_data", "predictors", "mpa_pres.grd"), overwrite=TRUE)







######################################## DISTANCE TO REEF ########################################


# Make distance to reef raster (first aggregate to avoid overly long computation time)
rast_xy_lr = raster::aggregate(rast_xy, fact = 2)
dist_reef = make_dist_to_reef_raster(coral_millenium, rast_xy_lr)
dist_reef = raster::resample(dist_reef, rast_xy)

# mask based on dist to mainland raster
dist_reef = mask_with_land(dist_reef, dist_land)

# Map distance to reef raster
map_dist_to_reef(dist_reef)

# write raster
raster::writeRaster(dist_reef, here::here("data", "processed_data", "predictors", "dist_reef.grd"), overwrite=TRUE)



### TO DO DEPTH SST others?
