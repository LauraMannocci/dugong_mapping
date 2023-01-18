
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




######################################## CALCULATE FLIGHT PARAMETERS ########################################

#image footprint
mean_altitude = 47 # in meters (in ft: 155) from ulm altimeter (gopro altitude unreliable)
fov = 86.7 # field of view in degrees from gopro hero 7 black manual
footprint_width = 2 * tanpi(fov / (2*180)) * mean_altitude # in meters, trigonometry formula
image_height = 1520 #in pixels (2.7k gopro format)
image_width = 2704 #in pixels (2.7k gopro format)
footprint_height = footprint_width * image_height/image_width #in meters

#overlap between successive images (given hyp of linear flight)
mean_speed = 30.55 # in m/s (in km/h: 110) from ulm OR 33 # in m/s (in km/h: 120) from ulm
frame_per_s = 3 # extraction rate in megafauna-project
overlap = (mean_speed / frame_per_s) / footprint_height




######################################## READ and CLEAN TELEMETRY DATA ########################################

#read telemetry data
telemetry = read_telem()

#clean telemetry data
telemetry = clean_telem(telemetry, lat1, lon1, lat2, lon2)

#merge dugong certain and dugong probable observations
levels(telemetry$object)[levels(telemetry$object) %in% c("Dugong_certain", "Dugong_probable")] <- "Dugong_certain_probable"

#Read video information
videos = read_video_info()

#Clean video information
videos = clean_video_info(videos)

#Join video information to telemetry
telemetry = join_video_info_telem(videos, telemetry)

#Get list of west coast videos
ls_videos = list_wcoast_videos(videos)

#select video information for west coast
videos = select_wcoast_videos(videos, ls_videos)

#select west coast telemetry
telemetry = select_telemetry_wcoast(telemetry, ls_videos)

#select dugong observations
telemetry_obs = subset(telemetry, telemetry$object == "Dugong_certain_probable" )

#count total number of individuals observed per species
telemetry_obs %>%
  dplyr::group_by(object) %>%
  dplyr::summarise(n_tot = dplyr::n())









######################################## READ TRANSECT DATA ########################################

#read transect pts
points1 = read_transects_points("megafauna1_points_latlon")
points2 = read_transects_points("megafauna2_points_latlon")
points3 = read_transects_points("megafauna3_points_latlon")

#make transect lines
library(sp)
lines1 = make_transect_lines(points1, "1")
lines2 = make_transect_lines(points2, "2")
lines3 = make_transect_lines(points3, "3")
#lapply does not work in function *****RUN OUTSIDE FUNCTION

#merge transect lines
lines = merge_transect_lines(lines1, lines2, lines3)







###################################### SELECT ON EFFORT TELEMETRY DATA #####################################

#read off effort portions west coast
off_effort = read_off_effort_wcoast()

#clean effort portions related to transit west coast
off_effort_transit = clean_off_effort_transit_wcoast(off_effort)

#clean effort portions related to loop west coast
off_effort_loop = clean_off_effort_loop_wcoast(off_effort)

#select on effort telemetry and observations west coast
telemetry_on = select_on_effort_wcoast(telemetry, off_effort_transit, off_effort_loop)
telemetry_obs_on = select_on_effort_wcoast(telemetry_obs, off_effort_transit, off_effort_loop)


###################################### REMOVE DUPLICATED DUGONG OBSERVATIONS #####################################

#IMPORTANT this step is partly done outside or R 

#export observations with duplicates
xlsx::write.xlsx(telemetry_obs_on, here::here("data", "processed_data", "density", "obs_with_duplicates.xlsx"))

#outside of R: copy this xlsx file, rename it to obs_duplicates_flagged.xlsx and 
#fill out new column "duplicate" (yes/no) by looking at each observation in megafauna-project
#also fill out column "stage" (adult/juvenile)
#also fill out column "eci" (1/2/3/4 ie Environmental Conditions Index following Hagihara et al 2016)

#import observations with duplicates flagged
telemetry_obs_on_dupl_flag = xlsx::read.xlsx(here::here("data", "processed_data", "density", "obs_duplicates_flagged.xlsx"), sheetIndex = 1)

#remove duplicate observations
telemetry_obs_on_dupl_rm = subset(telemetry_obs_on_dupl_flag, telemetry_obs_on_dupl_flag$duplicate == "no")
dim(telemetry_obs_on_dupl_rm) #162 unique observations
table(telemetry_obs_on_dupl_rm$stage) # 139 adults 23 juveniles

#correct number of observations for availability bias following Hagihara et al 2016
#proba of availability for New Caledonia (hour 8:00-16:00, depth <5 and 5-20 averaged)
#1 for eci1
#0.58 for eci2 -> divide by 0.58
#0.825 for eci 3 -> divide by 0.825
#0.39 for eci4 -> divide by 0.39
telemetry_obs_on_dupl_rm_avail_cor = telemetry_obs_on_dupl_rm %>% 
  dplyr::mutate(n_count_avail_corrected = dplyr::case_when(eci == 1 ~ 1,
                                                           eci == 2 ~ 1 / 0.58,
                                                           eci == 3 ~ 1 / 0.825,
                                                           eci == 4 ~ 1 / 0.39))  
telemetry_obs_on = telemetry_obs_on_dupl_rm_avail_cor

############################################ MAKE BASIC MAPS ON EFFORT #############################################################


#open street map
maplatlon = osm_map(lat1, lon1, lat2, lon2)



###### telemetry only
map_telemetry(maplatlon, telemetry_on)

###### telemetry with one color per date
map_telemetry_date(maplatlon, telemetry_on)

###### telemetry with separate map per date
map_telemetry_date_separate(maplatlon, telemetry_on)



####### telemetry with individual dugong observations (1 dot = 1 obs ; dots can be clustered)
map_indiv_dugong_telemetry(maplatlon, telemetry_on, telemetry_obs_on)

####### telemetry with individual dugong observations differenciating stage (1 dot = 1 obs ; dots can be clustered)
map_indiv_dugong_stage_telemetry(maplatlon, telemetry_on, telemetry_obs_on)


###### telemetry and transects lines
map_telemetry_transects(maplatlon, telemetry_on, lines)





############################################ MAKE DENSITY MAPS ON REGULAR GRID ON EFFORT #############################################################



#project osm for density mapping
maplatlon_proj = osm_mapproj(maplatlon)


#Make grid from xy study area raster 
grid = make_grid(rast_xy)

#get survey dates
telemetry %>%  
  dplyr::count(date) %>% 
  dplyr::pull(date) %>% 
  as.character() -> dates

#restrict telemetry to dates of surveys 
telemetry_on = restrict_telem_dates(telemetry_on, dates)

#convert telemetry points to lines
list_lines = convert_telemetry_points_to_lines(telemetry_on)
#lapply does not work in function convert_telemetry_points_to_lines() SO NEED TO RUN OUTSIDE FUNCTION 



### Track length

#Sum length of tracks (m) in grid cells per date
grid_tracks_per_date = sum_length_per_grid_per_date(grid, list_lines, dates)

#Total surveyed length
sum(grid_tracks_per_date$length, na.rm=T) #1145561  m

#Mean surveyed length per survey date
sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #163651.6 m

#Total surveyed area
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) # 101652630 m2 =  101.6 km2

#Mean surveyed area per survey date
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #14521804 m2 = 14.2 km2

#Map length of tracks per grid cell per date
map_tracklen_per_grid_per_date(maplatlon_proj, grid_tracks_per_date, 0.5)

#Map length of tracks per grid cell (all dates)
map_tracklen_per_grid(maplatlon_proj, grid_tracks_per_date, 0.5)



### Observations

#Count total number of observations per grid cell per date
grid_obs_per_date = count_obs_per_grid_per_date(grid, telemetry_obs_on)

#Map number of species observations per grid cell per date
map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Dugong_certain_probable", 0.2)
# map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Turtle", 0.2)
# map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Shark", 0.2)
# map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Round_ray", 0.2)
# map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Eagle_ray", 0.2)

#Map number of species observations per grid cell per date with zeros
map_obs_per_grid_per_date_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Dugong_certain_probable", 0.2)
# map_obs_per_grid_per_date_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Turtle", 0.2)
# map_obs_per_grid_per_date_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Shark", 0.2)
# map_obs_per_grid_per_date_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Round_ray", 0.2)
# map_obs_per_grid_per_date_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Eagle_ray", 0.2)

#Map number of species observations per grid cell (all dates)
map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Dugong_certain_probable", 0.5)
# map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Turtle", 0.5)
# map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Shark", 0.5)
# map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Round_ray", 0.5)
# map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Eagle_ray", 0.5)

#Map number of species observations per grid cell (all dates) with zeros 
map_obs_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Dugong_certain_probable", 0.5)
# map_obs_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Turtle", 0.5)
# map_obs_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Shark", 0.5)
# map_obs_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Round_ray", 0.5)
# map_obs_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Eagle_ray", 0.5)


### Densities

#Map species densities per grid cell (per date)
map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain_probable", 0.2)
# map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", 0.2)
# map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", 0.2)
# map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", 0.2)
# map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", 0.2)

#Map species densities per grid cell (all dates) LOG scale with zero
map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain_probable", 0.5)
# map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", 0.5)
# map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", 0.5)
# map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", 0.5)
# map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", 0.5)

#Map species densities per grid cell (all dates) with zero
map_dens_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain_probable", 0.5)
# map_dens_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", 0.5)
# map_dens_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", 0.5)
# map_dens_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", 0.5)
# map_dens_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", 0.5)



############################################ CREATE DENSITY RASTERS #############################################################


#Make dataframe per grid cell centers (including empty cell centers) of observations, track length and densities per date and per species 
df_all_species = make_df_all_species(grid_obs_per_date, grid_tracks_per_date, footprint_width) #desnity in indiv / 10000m2 (=0.1 km2 or 100m x 100m)

#get raster stack of density for given species
r_density_dugong = get_density_stack_species(df_all_species, "Dugong_certain_probable")

#extend raster to study area raster
r_density_dugong = extend_raster(r_density_dugong, rast_xy)

#write
raster::writeRaster(r_density_dugong[[7]], here::here("data", "processed_data", "density", "dugong_density.grd"), overwrite=TRUE)

