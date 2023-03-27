save.image(file='untitled1.RData')
# load(file='untitled1.RData')
# cgwtools::resave(coral, file='untitled1.Rdata')


#load all functions
devtools::load_all()




######################################## DEFINE STUDY AREA AND CORRESPONDING RASTERS ########################################

#Define study area coordinates
#big region
lat1 =  -20 ; lat2 = -22.5
lon1 = 163.5 ; lon2 = 167

#study area raster resolution in meters
raster_res_m = 500

# make study area raster in lat lon  (resolution 0.01 degrees) 
rast_latlon = make_area_raster_latlon(lat1, lon1, lat2, lon2, 0.01)

# make study area raster projected to lambert New caledonia in meters with given resolution in meters (resolution 500 meters) 
rast_xy = make_area_raster_xy(lat1, lon1, lat2, lon2, raster_res_m)


#open street map
# maplatlon = osm_map(lat1, lon1, lat2, lon2)





######################################## DISTANCE TO MAINLAND ########################################

# Read New Caledonia coral shapefile (for distances computation)
coral_millenium = read_original_coralnc_millenium()

# Make distance to mainland raster
dist_land = make_dist_to_land_raster(coral_millenium, rast_xy)

# mask to remove land
dist_land = mask_with_land(dist_land, dist_land)
# 
# # Map distance to mainland raster
# map_dist_to_land(dist_land)
# 
# # write raster
# raster::writeRaster(dist_land, here::here("data", "processed_data", "predictors", "dist_land.grd"), overwrite=TRUE)
# 
# 
# 
# 
# ######################################## TRAVEL TIME ########################################
# 
# # load travel time raster from FB (corresponding to travel times from Nouméa (seconds) in 1 x 1 km cell)
# # and resample
# travel_time = load_travel_time_raster(rast_xy) # **** reextract based on full survey coverage ??
# 
# # mask based on dist to mainland raster
# travel_time = mask_with_land(travel_time, dist_land)
# 
# # Map original travel time
# map_travel_time(travel_time)
# 
# # write raster
# raster::writeRaster(travel_time, here::here("data", "processed_data", "predictors", "travel_time.grd"), overwrite=TRUE)
# 



######################################## BENTHIC CORAL SUBSTRATE (ALLEN ATLAS) ########################################

#read Allen coral benthic polygon
coral = read_convert_coral_benthic(lon1, lon2, lat2, lat1)
# 
# #Map Allen coral polygons on OSM
# map_allen_coral_osm(maplatlon, coral, lon1, lon2, lat1, lat2)

#Reproject
coral_projected = sp::spTransform(coral, sp::CRS("+init=epsg:3163"))

#create coral_cover raster to be used in assign_zeros_where_absent_type function below
#coral_cover = raster::rasterize(coral_projected, rast_xy, getCover = T)


# #setup parallel processing
# 
# library(foreach)
# library(raster)
# 
# #1-create the cluster
# my.cluster <- parallel::makeCluster(
#   parallel::detectCores() - 1,
#   type = "PSOCK")
# 
# #2-register it to be used by %dopar%
# doParallel::registerDoParallel(cl = my.cluster)
# 
# #make coverage raster for each habitat type
# percent_rubble = make_raster_coral_benthic_type(coral_projected, rast_xy, "Rubble")
# percent_coral_algae = make_raster_coral_benthic_type(coral_projected, rast_xy, "Coral_algae")
# percent_microalgal_mats = make_raster_coral_benthic_type(coral_projected, rast_xy, "Microalgal_mats")
# percent_seagrass = make_raster_coral_benthic_type(coral_projected, rast_xy, "Seagrass")
# percent_sand = make_raster_coral_benthic_type(coral_projected, rast_xy, "Sand")
# percent_rock = make_raster_coral_benthic_type(coral_projected, rast_xy, "Rock")
# 
# 
# #extend all rasters to study area raster
# percent_rubble = extend_raster(percent_rubble, rast_xy)
# percent_coral_algae = extend_raster(percent_coral_algae, rast_xy)
# percent_microalgal_mats = extend_raster(percent_microalgal_mats, rast_xy)
# percent_seagrass = extend_raster(percent_seagrass, rast_xy)
# percent_sand = extend_raster(percent_sand, rast_xy)
# percent_rock = extend_raster(percent_rock, rast_xy)
# 
# 
# # mask based on dist to mainland raster
# percent_rubble = mask_with_land(percent_rubble, dist_land)
# percent_coral_algae = mask_with_land(percent_coral_algae, dist_land)
# percent_microalgal_mats = mask_with_land(percent_microalgal_mats, dist_land)
# percent_seagrass = mask_with_land(percent_seagrass, dist_land)
# percent_sand = mask_with_land(percent_sand, dist_land)
# percent_rock = mask_with_land(percent_rock, dist_land)
# 
# 
# # assign zeros where habitat/geomorpho type is absent using coral_cover raster as mask
# percent_rubble = assign_zeros_where_absent_type(percent_rubble, coral_cover) 
# percent_coral_algae = assign_zeros_where_absent_type(percent_coral_algae, coral_cover) 
# percent_microalgal_mats = assign_zeros_where_absent_type(percent_microalgal_mats, coral_cover) 
# percent_seagrass = assign_zeros_where_absent_type(percent_seagrass, coral_cover) 
# percent_sand = assign_zeros_where_absent_type(percent_sand, coral_cover) 
# percent_rock = assign_zeros_where_absent_type(percent_rock, coral_cover) 
# 
# 
# #map
# map_coral_habitat(percent_rubble, "percent_rubble")
# map_coral_habitat(percent_coral_algae, "percent_coral_algae")
# map_coral_habitat(percent_microalgal_mats, "percent_microalgal_mats")
# map_coral_habitat(percent_seagrass, "percent_seagrass")
# map_coral_habitat(percent_sand, "percent_sand")
# map_coral_habitat(percent_rock, "percent_rock")
# 
# 
# #write rasters
# raster::writeRaster(percent_rubble, here::here("data", "processed_data", "predictors", "percent_rubble.grd"), overwrite=TRUE)
# raster::writeRaster(percent_coral_algae, here::here("data", "processed_data", "predictors", "percent_coral_algae.grd"), overwrite=TRUE)
# raster::writeRaster(percent_microalgal_mats, here::here("data", "processed_data", "predictors", "percent_microalgal_mats.grd"), overwrite=TRUE)
# raster::writeRaster(percent_seagrass, here::here("data", "processed_data", "predictors", "percent_seagrass.grd"), overwrite=TRUE)
# raster::writeRaster(percent_sand, here::here("data", "processed_data", "predictors", "percent_sand.grd"), overwrite=TRUE)
# raster::writeRaster(percent_rock, here::here("data", "processed_data", "predictors", "percent_rock.grd"), overwrite=TRUE)
# 




######################################## GEOMORPHOLOGY (ALLEN ATLAS) ########################################


#read Allen coral geomorpho polygon
geom = read_convert_coral_geomorpho(lon1, lon2, lat2, lat1)

#Map Allen geomorpho polygons on OSM
#map_allen_geomorpho_osm(maplatlon, geom, lon1, lon2, lat1, lat2)

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
#percent_back_reef_slope = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Back_Reef_Slope")
percent_deep_lagoon = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Deep_Lagoon")
#percent_inner_reef_flat = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Inner_Reef_Flat")
#percent_outer_reef_flat = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Outer_Reef_Flat")
#percent_plateau = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Plateau")
#percent_reef_crest = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Reef_Crest")
percent_reef_slope = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Reef_Slope")
#percent_shallow_lagoon = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Shallow_Lagoon")
#percent_terrestrial_reef_flat = make_raster_coral_geomorpho_type(geom_projected, rast_xy, "Terrestrial_Reef_Flat")



#extend all rasters to study area raster
#percent_back_reef_slope = extend_raster(percent_back_reef_slope, rast_xy)
percent_deep_lagoon = extend_raster(percent_deep_lagoon, rast_xy)
# percent_inner_reef_flat = extend_raster(percent_inner_reef_flat, rast_xy)
# percent_outer_reef_flat = extend_raster(percent_outer_reef_flat, rast_xy)
# percent_plateau = extend_raster(percent_plateau, rast_xy)
# percent_reef_crest = extend_raster(percent_reef_crest, rast_xy)
percent_reef_slope = extend_raster(percent_reef_slope, rast_xy)
# percent_shallow_lagoon = extend_raster(percent_shallow_lagoon, rast_xy)
# percent_terrestrial_reef_flat = extend_raster(percent_terrestrial_reef_flat, rast_xy)



# mask based on dist to mainland raster
# percent_back_reef_slope = mask_with_land(percent_back_reef_slope, dist_land)
percent_deep_lagoon = mask_with_land(percent_deep_lagoon, dist_land)
# percent_inner_reef_flat = mask_with_land(percent_inner_reef_flat, dist_land)
# percent_outer_reef_flat = mask_with_land(percent_outer_reef_flat, dist_land)
# percent_plateau = mask_with_land(percent_plateau, dist_land)
# percent_reef_crest = mask_with_land(percent_reef_crest, dist_land)
percent_reef_slope = mask_with_land(percent_reef_slope, dist_land)
# percent_shallow_lagoon = mask_with_land(percent_shallow_lagoon, dist_land)
# percent_terrestrial_reef_flat = mask_with_land(percent_terrestrial_reef_flat, dist_land)


# assign zeros where habitat/geomorpho type is absent based on coral_cover raster
# percent_back_reef_slope = assign_zeros_where_absent_type(percent_back_reef_slope, coral_cover) 
# percent_deep_lagoon = assign_zeros_where_absent_type(percent_deep_lagoon, coral_cover) 
# percent_inner_reef_flat = assign_zeros_where_absent_type(percent_inner_reef_flat, coral_cover) 
# percent_outer_reef_flat = assign_zeros_where_absent_type(percent_outer_reef_flat, coral_cover) 
# percent_plateau = assign_zeros_where_absent_type(percent_plateau, coral_cover) 
# percent_reef_crest = assign_zeros_where_absent_type(percent_reef_crest, coral_cover) 
# percent_reef_slope = assign_zeros_where_absent_type(percent_reef_slope, coral_cover) 
# percent_shallow_lagoon = assign_zeros_where_absent_type(percent_shallow_lagoon, coral_cover) 
# percent_terrestrial_reef_flat = assign_zeros_where_absent_type(percent_terrestrial_reef_flat, coral_cover) 



#map                
# map_coral_habitat(percent_back_reef_slope, "percent_back_reef_slope")
# map_coral_habitat(percent_deep_lagoon, "percent_deep_lagoon")
# map_coral_habitat(percent_inner_reef_flat, "percent_inner_reef_flat")
# map_coral_habitat(percent_outer_reef_flat, "percent_outer_reef_flat")
# map_coral_habitat(percent_plateau, "percent_plateau")
# map_coral_habitat(percent_reef_crest, "percent_reef_crest")
# map_coral_habitat(percent_reef_slope, "percent_reef_slope")
# map_coral_habitat(percent_shallow_lagoon, "percent_shallow_lagoon")
# map_coral_habitat(percent_terrestrial_reef_flat, "percent_back_reef_slope")


#write rasters     
# raster::writeRaster(percent_back_reef_slope, here::here("data", "processed_data", "predictors", "percent_back_reef_slope.grd"), overwrite=TRUE)
raster::writeRaster(percent_deep_lagoon, here::here("data", "processed_data", "predictors", "percent_deep_lagoon_large.grd"), overwrite=TRUE)
# raster::writeRaster(percent_inner_reef_flat, here::here("data", "processed_data", "predictors", "percent_inner_reef_flat.grd"), overwrite=TRUE)
# raster::writeRaster(percent_outer_reef_flat, here::here("data", "processed_data", "predictors", "percent_outer_reef_flat.grd"), overwrite=TRUE)
# raster::writeRaster(percent_plateau, here::here("data", "processed_data", "predictors", "percent_plateau.grd"), overwrite=TRUE)
# raster::writeRaster(percent_reef_crest, here::here("data", "processed_data", "predictors", "percent_reef_crest.grd"), overwrite=TRUE)
raster::writeRaster(percent_reef_slope, here::here("data", "processed_data", "predictors", "percent_reef_slope_large.grd"), overwrite=TRUE)
# raster::writeRaster(percent_shallow_lagoon, here::here("data", "processed_data", "predictors", "percent_shallow_lagoon.grd"), overwrite=TRUE)
# raster::writeRaster(percent_terrestrial_reef_flat, here::here("data", "processed_data", "predictors", "percent_terrestrial_reef_flat.grd"), overwrite=TRUE)





######################################## TURBIDITY (ALLEN ATLAS) ########################################

# read quaterly turbidity and produce average raster
turb = read_average_resample_turbidity(lon1, lon2, lat2, lat1, rast_xy)

# mask based on dist to mainland raster
turb = mask_with_land(turb, dist_land)

#map turbidity
# map_turbidity(turb)

# write raster
raster::writeRaster(turb, here::here("data", "processed_data", "predictors", "turbidity_large.grd"), overwrite=TRUE)





######################################## POPULATION ########################################









######################################## POPULATION DENSITY ########################################





######################################## DEPTH ########################################








######################################## MPA ########################################






######################################## DISTANCE TO REEF ########################################



######################################## DISTANCE TO SEAGRASS ########################################




#################################### first try

# split study area in 4 quarters to avoid failure

#----q1

# Make study area raster and read allen coral benthic polygon
# lat1_q1 =  -20 ; lat2_q1 = -21.25 ; lon1_q1 = 163.5 ; lon2_q1 = 165.25
lat1_q1 =  -20 ; lat2_q1 = -21.25 ; lon1_q1 = 163.5 ; lon2_q1 = 164.75
raster_res_m = 500
rast_xy_q1 = make_area_raster_xy(lat1_q1, lon1_q1, lat2_q1, lon2_q1, raster_res_m)
coral_q1 = read_convert_coral_benthic(lon1_q1, lon2_q1, lat2_q1, lat1_q1)

# Make distance to seagrass raster 
dist_seagrass_q1 = make_dist_to_seagrass_raster(coral_q1, rast_xy_q1)

# mask based on dist to mainland raster
dist_seagrass_q1 = mask_with_land(dist_seagrass_q1, dist_land_q1)

# Map distance to seagrass raster
# map_dist_to_seagrass(dist_seagrass)


# write raster
raster::writeRaster(dist_seagrass, here::here("data", "processed_data", "predictors", "dist_seagrass_large.grd"), overwrite=TRUE)




#################################### second try


# Extract polygon of seagrass from coral shapefil
seagrass <- coral[coral@data$class ==  "Seagrass", ]

#project
seagrass = sp::spTransform(seagrass, sp::CRS("+init=epsg:3163")) #project to lambert NC

rast_xy2 <- raster::aggregate(rast_xy, 4)

pts = as(rast_xy2, "SpatialPoints")


set.seed(1)
#Create the clusters
library(doParallel)
cl <- makeCluster(detectCores()-1) 
registerDoParallel(cl)
#Export the environment variables to each cluster
clusterExport(cl,ls())
#Load the library "rgeos" to each cluster
clusterEvalQ(cl, library(rgeos))
#Split the data
ID.Split<-clusterSplit(cl, 1:nrow(seagrass))
#Define a function that calculates the distance of one ID in relation to the poly2
a<-function(x) gDistance(spgeom1 = seagrass[x,], spgeom2 = pts, byid=TRUE)
#Run the function in each cluster
system.time(m<-clusterApply(cl, x=ID.Split, fun=a))
#Cluster close
stopCluster(cl)
#Merge the results
output<- do.call("cbind", m)



#################################### third try

# function adapted from https://www.spatialanalytics.co.nz/post/2017/09/11/a-parallel-function-for-spatial-analysis-in-r/
# Paralise any simple features analysis.
st_par <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    parallel::mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  # Combine results back together. 
  result <- do.call("rbind", split_results)
  
  # Return result
  return(result)
}




#################################### fourth try

#setup parallel processing

library(foreach)
library(raster)

#1-create the cluster
my.cluster <- parallel::makeCluster(
  parallel::detectCores() - 1,
  type = "PSOCK")

#2-register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)



make_raster_coral_geomorpho_type <- function(rast_xy, seagrass){
  
  # Calculate distance of raster cells to seagrass 
  cat('calculating distance \n')
  pts = as(rast_xy, "SpatialPoints")
  
  #Loop to get coverage per polygon
  raster_loop <- foreach(i = 1:nrow(seagrass)) %dopar% 
    
    rgeos::gDistance(seagrass[i,], pts, byid=TRUE)
    # This creates a matrix with a column for each feature in seagrass
  
  #merge raster lists into unique raster
  print("merging ...")
  r_merged <- do.call(raster::merge, raster_loop)
  
  # To get the nearest distance (meters) to any feature, apply min over rows:
  raster[] = apply(dd, 1, min) #---------warning message
  # raster::plot(raster)
  
  # rename
  names(raster) = "dist_seagrass"
  
  return(raster)
  
}




#################################### last try

rm(coral, coral_projected, geom, geom_projected, percent_deep_lagoon, percent_reef_slope, turb, dist_land, pts)

# Extract polygon of seagrass from coral shapefil
seagrass <- coral[coral@data$class ==  "Seagrass", ]

#project
seagrass = sp::spTransform(seagrass, sp::CRS("+init=epsg:3163")) #project to lambert NC


seagrass = sf::st_as_sf(seagrass)

#rast_xy_lr = raster::aggregate(rast_xy, fact = 4) 

pts = as(rast_xy, "SpatialPoints")

pts = sf::st_as_sf(pts)

rm(coral, rast_xy, rast_latlon)
save.image(file='for_laure.RData')

dist = sf::st_distance(seagrass, pts)

#dist = raster::resample(dist, rast_xy)

