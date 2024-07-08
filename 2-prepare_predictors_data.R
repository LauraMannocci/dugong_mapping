
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






######################################## DISTANCE TO MAINLAND (MILLENIUM) ########################################

# Read New Caledonia coral shapefile (for distances computation)
coral_millenium = read_original_coralnc_millenium()

# Make distance to mainland raster
dist_land = make_dist_to_land_raster(coral_millenium, rast_xy)

# write raster
raster::writeRaster(dist_land, here::here("data", "processed_data", "predictors", "dist_land_no_mask.grd"), overwrite=TRUE)

# mask to remove land
dist_land = mask_with_land(dist_land, dist_land)

# Map distance to mainland raster
map_dist_to_land(dist_land)

# write raster
raster::writeRaster(dist_land, here::here("data", "processed_data", "predictors", "dist_land.grd"), overwrite=TRUE)








######################################## CORAL COVER (MILLENIUM) ########################################

#make coral cover raster
coral_cover = make_coral_cover_raster(coral_millenium, rast_xy) 

# mask to remove land
coral_cover = mask_with_land2(coral_cover, dist_land)

# Map distance to mainland raster
map_coral_cover(coral_cover)

# write raster
raster::writeRaster(coral_cover, here::here("data", "processed_data", "predictors", "coral_cover.grd"), overwrite=TRUE)





######################################## MEDIAN DEPTH ########################################

# read depth from Jean roger (IRD) and resample to study area
depth_med = read_median_depth(rast_latlon, rast_xy)

# mask based on dist to mainland raster
depth = mask_with_land2(depth_med, dist_land)

# Map depth raster
map_depth(depth)

# write raster
raster::writeRaster(depth, here::here("data", "processed_data", "predictors", "depth.grd"), overwrite=TRUE)

# calculate depth in surveyed block
# block <- read_project_surveyed_block()
depth_mask <- raster::mask(depth, block)
depth_mask[depth_mask < -20] <- NA
raster::plot(depth_mask)

depth_mask <- raster::mask(depth, block)
depth_mask[depth_mask < -30] <- NA
raster::plot(depth_mask)



######################################## MEDIAN SLOPE ########################################


# Calculate median slope from median depth
slope = calculate_median_slope(depth_med)

# mask based on dist to mainland raster
slope = mask_with_land2(slope, dist_land)

# Map slope raster
map_slope(slope)

# write raster
raster::writeRaster(slope, here::here("data", "processed_data", "predictors", "slope.grd"), overwrite=TRUE)



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
mpa_type_no_take_only = make_mpa_type_raster_no_take_only(rast_xy) #ran outside function to avoi dbug

# mask based on dist to mainland raster
mpa_type_no_take_only = mask_with_land(mpa_type_no_take_only, dist_land)

# Map mpa type
map_mpa_type_no_take_only(mpa_type_no_take_only)

# write raster
raster::writeRaster(mpa_type_no_take_only, here::here("data", "processed_data", "predictors", "mpa_type_no_take_only.grd"), overwrite=TRUE)




# Make mpa type raster no-take only poe / ile verte (for msp)
library(raster) #for good functionning of levels in function
mpa_type_no_take_only_poe_ile_verte = make_mpa_type_raster_no_take_only_poe_ile_verte(rast_xy) #ran outside function to avoi dbug

# mask based on dist to mainland raster
mpa_type_no_take_only_poe_ile_verte = mask_with_land(mpa_type_no_take_only_poe_ile_verte, dist_land)

# write raster
raster::writeRaster(mpa_type_no_take_only_poe_ile_verte, here::here("data", "processed_data", "predictors", "mpa_type_no_take_only_poe_ile_verte.grd"), overwrite=TRUE)




# Make mpa presence raster
library(raster) #for good functionning of levels in function
mpa_pres = make_mpa_pres_raster(rast_xy) #ran outside function to avoi dbug

# mask based on dist to mainland raster
mpa_pres = mask_with_land(mpa_pres, dist_land)

# Map mpa type
map_mpa_pres(mpa_pres)

# write raster
raster::writeRaster(mpa_pres, here::here("data", "processed_data", "predictors", "mpa_pres.grd"), overwrite=TRUE)









######################################## DISTANCE TO REEF PASSES (Breckwood et al 2022)########################################

#read reef passes (from Breckwood et al 2022: https://doi.pangaea.de/10.1594/PANGAEA.942568)
passes = read_passes()

# Make distance to pass raster 
dist_passes = make_dist_to_pass_raster(passes, rast_xy)

# mask based on dist to mainland raster
dist_passes = mask_with_land(dist_passes, dist_land)

# Map distance to passes raster
map_dist_to_passes(dist_passes)

# write raster
raster::writeRaster(dist_passes, here::here("data", "processed_data", "predictors", "dist_passes.grd"), overwrite=TRUE)




######################################## DISTANCE TO BARRIER REEF (MILLENIUM)########################################

# Make distance to barrier reef raster
dist_barrier_reef = make_dist_to_barrier_reef_raster(coral_millenium, rast_xy)

# mask based on dist to mainland raster
dist_barrier_reef = mask_with_land(dist_barrier_reef, dist_land)

# Map distance to mainland raster
map_dist_to_barrier_reef(dist_barrier_reef)

# write raster
raster::writeRaster(dist_barrier_reef, here::here("data", "processed_data", "predictors", "dist_barrier_reef.grd"), overwrite=TRUE)





######################################## DISTANCE TO INTERMEDIATE REEF PATCH (MILLENIUM)########################################


# Make distance to mainland raster
dist_intermediate_reef = make_dist_to_intermediate_reef_raster(coral_millenium, rast_xy)

# mask based on dist to mainland raster
dist_intermediate_reef = mask_with_land(dist_intermediate_reef, dist_land)

# Map distance to mainland raster
map_dist_to_intermediate_reef(dist_intermediate_reef)

# write raster
raster::writeRaster(dist_intermediate_reef, here::here("data", "processed_data", "predictors", "dist_intermediate_reef.grd"), overwrite=TRUE)








######################################## DISTANCE TO ALL REEF (MILLENIUM)########################################


# Make distance to mainland raster
dist_all_reef = make_dist_to_all_reef_raster(coral_millenium, rast_xy)

# mask based on dist to mainland raster
dist_all_reef = mask_with_land(dist_all_reef, dist_land)

# Map distance to mainland raster
map_dist_to_all_reef(dist_all_reef)

# write raster
raster::writeRaster(dist_all_reef, here::here("data", "processed_data", "predictors", "dist_all_reef.grd"), overwrite=TRUE)






##################################### PERCENT COVERAGE DEEP LAGOON (MILLENIUM)##################################### 



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
percent_deep_lagoon_millenium = make_raster_coral_millenium_type(coral_millenium, rast_xy, "deep_lagoon_millenium")

#extend all rasters to study area raster
percent_deep_lagoon_millenium = extend_raster(percent_deep_lagoon_millenium, rast_xy)

# mask based on dist to mainland raster
percent_deep_lagoon_millenium = mask_with_land(percent_deep_lagoon_millenium, dist_land)


# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
percent_deep_lagoon_millenium = assign_zeros_where_absent_type(percent_deep_lagoon_millenium, coral_cover_for_assignment) 


#map                
map_coral_habitat(percent_deep_lagoon_millenium, "percent_deep_lagoon_millenium")


#write rasters 
names(percent_deep_lagoon_millenium) <- "deep_lagoon"
raster::writeRaster(percent_deep_lagoon_millenium, here::here("data", "processed_data", "predictors", "percent_deep_lagoon.grd"), overwrite=TRUE)




##################################### PERCENT COVERAGE SHALLOW TERRACE (MILLENIUM)##################################### 



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
percent_shallow_terrace_millenium = make_raster_coral_millenium_type(coral_millenium, rast_xy, "shallow_terrace_millenium")

#extend all rasters to study area raster
percent_shallow_terrace_millenium = extend_raster(percent_shallow_terrace_millenium, rast_xy)

# mask based on dist to mainland raster
percent_shallow_terrace_millenium = mask_with_land(percent_shallow_terrace_millenium, dist_land)


# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
percent_shallow_terrace_millenium = assign_zeros_where_absent_type(percent_shallow_terrace_millenium, coral_cover_for_assignment) 


#map                
map_coral_habitat(percent_shallow_terrace_millenium, "percent_shallow_terrace_millenium")


#write rasters 
names(percent_shallow_terrace_millenium) <- "shallow_terrace"
raster::writeRaster(percent_shallow_terrace_millenium, here::here("data", "processed_data", "predictors", "percent_shallow_terrace.grd"), overwrite=TRUE)






##################################### PERCENT COVERAGE FOREREEF (MILLENIUM)##################################### 



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
percent_forereef_millenium = make_raster_coral_millenium_type(coral_millenium, rast_xy, "forereef_millenium")

#extend all rasters to study area raster
percent_forereef_millenium = extend_raster(percent_forereef_millenium, rast_xy)

# mask based on dist to mainland raster
percent_forereef_millenium = mask_with_land(percent_forereef_millenium, dist_land)


# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
percent_forereef_millenium = assign_zeros_where_absent_type(percent_forereef_millenium, coral_cover_for_assignment) 


#map                
map_coral_habitat(percent_forereef_millenium, "percent_forereef_millenium")


#write rasters 
names(percent_forereef_millenium) <- "forereef"
raster::writeRaster(percent_forereef_millenium, here::here("data", "processed_data", "predictors", "percent_forereef.grd"), overwrite=TRUE)









##################################### PERCENT COVERAGE reefflat (MILLENIUM)##################################### 



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
percent_reefflat_millenium = make_raster_coral_millenium_type(coral_millenium, rast_xy, "reefflat_millenium")

#extend all rasters to study area raster
percent_reefflat_millenium = extend_raster(percent_reefflat_millenium, rast_xy)

# mask based on dist to mainland raster
percent_reefflat_millenium = mask_with_land(percent_reefflat_millenium, dist_land)


# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
percent_reefflat_millenium = assign_zeros_where_absent_type(percent_reefflat_millenium, coral_cover_for_assignment) 


#map                
map_coral_habitat(percent_reefflat_millenium, "percent_reefflat_millenium")


#write rasters 
names(percent_reefflat_millenium) <- "reefflat"
raster::writeRaster(percent_reefflat_millenium, here::here("data", "processed_data", "predictors", "percent_reefflat.grd"), overwrite=TRUE)









##################################### PERCENT COVERAGE intermreefflat (MILLENIUM)##################################### 



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
percent_intermreefflat_millenium = make_raster_coral_millenium_type(coral_millenium, rast_xy, "intermreefflat_millenium")

#extend all rasters to study area raster
percent_intermreefflat_millenium = extend_raster(percent_intermreefflat_millenium, rast_xy)

# mask based on dist to mainland raster
percent_intermreefflat_millenium = mask_with_land(percent_intermreefflat_millenium, dist_land)


# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
percent_intermreefflat_millenium = assign_zeros_where_absent_type(percent_intermreefflat_millenium, coral_cover_for_assignment) 


#map                
map_coral_habitat(percent_intermreefflat_millenium, "percent_intermreefflat_millenium")


#write rasters 
names(percent_intermreefflat_millenium) <- "intermreefflat"
raster::writeRaster(percent_intermreefflat_millenium, here::here("data", "processed_data", "predictors", "percent_intermreefflat.grd"), overwrite=TRUE)








##################################### PERCENT COVERAGE diffusefringing (MILLENIUM)##################################### 



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
percent_diffusefringing_millenium = make_raster_coral_millenium_type(coral_millenium, rast_xy, "diffusefringing_millenium")

#extend all rasters to study area raster
percent_diffusefringing_millenium = extend_raster(percent_diffusefringing_millenium, rast_xy)

# mask based on dist to mainland raster
percent_diffusefringing_millenium = mask_with_land(percent_diffusefringing_millenium, dist_land)


# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
percent_diffusefringing_millenium = assign_zeros_where_absent_type(percent_diffusefringing_millenium, coral_cover_for_assignment) 


#map                
map_coral_habitat(percent_diffusefringing_millenium, "percent_diffusefringing_millenium")


#write rasters 
names(percent_diffusefringing_millenium) <- "diffusefringing"
raster::writeRaster(percent_diffusefringing_millenium, here::here("data", "processed_data", "predictors", "percent_diffusefringing.grd"), overwrite=TRUE)







##################################### PERCENT COVERAGE channel (MILLENIUM)##################################### 



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
percent_channel_millenium = make_raster_coral_millenium_type(coral_millenium, rast_xy, "channel_millenium")

#extend all rasters to study area raster
percent_channel_millenium = extend_raster(percent_channel_millenium, rast_xy)

# mask based on dist to mainland raster
percent_channel_millenium = mask_with_land(percent_channel_millenium, dist_land)


# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
percent_channel_millenium = assign_zeros_where_absent_type(percent_channel_millenium, coral_cover_for_assignment) 


#map                
map_coral_habitat(percent_channel_millenium, "percent_channel_millenium")


#write rasters 
names(percent_channel_millenium) <- "channel"
raster::writeRaster(percent_channel_millenium, here::here("data", "processed_data", "predictors", "percent_channel.grd"), overwrite=TRUE)





##################################### PERCENT COVERAGE SEAGRASS (ANDREFOUET) ##################################### 



# Read seagrass shapefile 
seagrass_shp = read_convert_seagrass_nc()


#make coverage raster 
percent_seagrass = make_raster_seagrass(seagrass_shp, rast_xy)

#extend all rasters to study area raster
percent_seagrass = extend_raster(percent_seagrass, rast_xy)

# mask based on dist to mainland raster
percent_seagrass = mask_with_land(percent_seagrass, dist_land)

# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
percent_seagrass = assign_zeros_where_absent_type(percent_seagrass, coral_cover_for_assignment) 


#map                
map_coral_habitat(percent_seagrass, "percent_seagrass")


#write rasters 
names(percent_seagrass) <- "seagrass"
raster::writeRaster(percent_seagrass, here::here("data", "processed_data", "predictors", "percent_seagrass.grd"), overwrite=TRUE)






######################################## DISTANCE TO SEAGRASS (ANDREFOUET) ########################################

# Make distance to seagrass raster (meters)
dist_seagrass <- make_dist_to_seagrass_raster(seagrass_shp, rast_xy)

# mask to remove land
dist_seagrass = mask_with_land(dist_seagrass, dist_land)

# Map distance to seagrass raster
map_dist_to_seagrass(dist_seagrass)

# write raster
raster::writeRaster(dist_seagrass, here::here("data", "processed_data", "predictors", "dist_seagrass.grd"), overwrite=TRUE)

