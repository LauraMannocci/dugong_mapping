
#load all functions
devtools::load_all()




######################################## DEFINE STUDY AREA AND CORRESPONDING RASTERS ########################################

#Define study area coordinates
#big region
lat1 =  -21.2 ; lat2 = -22
lon1 = 164.6 ; lon2 = 166

# make study area raster in lat lon  (resolution 0.01 degrees) 
rast_latlon = make_area_raster_latlon(lat1, lon1, lat2, lon2, 0.01)

# make study area raster projected to lambert New caledonia in meters with given resolution in meters (resolution 500 meters) 
rast_xy = make_area_raster_xy(lat1, lon1, lat2, lon2, 500)




######################################## CALCULATE FLIGHT PARAMETERS ########################################

#image footprint
mean_altitude = 47 # in meters (in ft: 155) from ulm altimeter (gopro altitude unreliable)
fov = 86.7 # field of view in degrees from gopro hero 7 black manual
footprint_width = 2 * tanpi(fov / (2*180)) * mean_altitude # in meters, trigonometry formula
image_height = 1520 #in pixels (2.7k gopro format)
image_width = 2704 #in pixels (2.7k gopro format)
footprint_height = footprint_width * image_height/image_width #in meters

#overlap between successive images (given hyp of linear flight)
mean_speed = 30.55 # in m/s (in km/h: 110) from ulm
frame_per_s = 3 # extraction rate in megafauna-project
overlap = (mean_speed / frame_per_s) / footprint_height




######################################## READ and CLEAN TELEMETRY DATA ########################################

#read telemetry data
telemetry = read_telem()

#clean telemetry data
telemetry = clean_telem(telemetry, lat1, lon1, lat2, lon2)

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

#Clean duplicated obs from telemetry
#telemetry_obs = clean_duplicated_obs_telemetry(telemetry, overlap, image_height) #(this code takes about 20 minutes to process)
#alternatively read the Rdata to read the processed data directly (telemetry_obs)
load("data/processed_data/telemetry_obs.RData") 

#count total number of individuals observed per species
telemetry_obs %>%
  dplyr::group_by(object) %>%
  dplyr::summarise(n_tot = dplyr::n())







######################################## READ CORAL DATA ########################################

#read Allen coral benthic polygon
coral = read_convert_allen_coral_benthic(lon1, lon2, lat2, lat1)

# make coral raster h ?????
t = raster::rasterize(coral, rast_latlon)




######################################## READ MPA DATA ########################################

# read mpa polygon
mpa = read_mpanc()




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




############################################ MAKE BASIC MAPS #############################################################


#open street map
maplatlon = osm_map(lat1, lon1, lat2, lon2)


###### Map Allen coral polygons benthic PROBLEM ?????
map_allen_coral_osm(maplatlon, mpa, allen_coral, lon1, lon2, lat1, lat2, dist = 2, offset_lon = 0.07, offset_lat = 0.01)


###### telemetry only
map_telemetry(maplatlon, telemetry)

###### telemetry with one color per date
map_telemetry_date(maplatlon, telemetry)

###### telemetry with separate map per date
map_telemetry_date_separate(maplatlon, telemetry)



####### telemetry with individual species
map_indiv_species_telemetry(maplatlon, telemetry, telemetry_obs)

####### telemetry with individual species with separate map per species
map_indiv_species_telemetry_separate(maplatlon, telemetry, telemetry_obs)



###### telemetry and transects lines
map_telemetry_transects(maplatlon, telemetry, lines)







###################################### SELECT ON EFFORT TELEMETRY DATA #####################################

#TO DO LAURA
telemetry_on = telemetry

telemetry_obs_on = telemetry_obs





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
sum(grid_tracks_per_date$length, na.rm=T) #1285219 m

#Mean surveyed length per survey date
sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #183602.7 m

#Total surveyed area
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) # 114045335 m2 =  114.04 km2

#Mean surveyed area per survey date
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #16292191 m2 = 16.2 km2

#Map length of tracks per grid cell per date
map_tracklen_per_grid_per_date(maplatlon_proj, grid_tracks_per_date, 0.5)

#Map length of tracks per grid cell (all dates)
map_tracklen_per_grid(maplatlon_proj, grid_tracks_per_date, 0.5)



### Observations

#Count total number of observations per grid cell per date
grid_obs_per_date = count_obs_per_grid_per_date(grid, telemetry_obs_on)

#Map number of species observations per grid cell per date
map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Dugong_certain", 0.05)
map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Turtle", 0.05)
map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Shark", 0.05)
map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Round_ray", 0.05)
map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Eagle_ray", 0.05)

#Map number of species observations per grid cell (all dates)
map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Dugong_certain", 0.2)
map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Turtle", 0.2)
map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Shark", 0.2)
map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Round_ray", 0.2)
map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Eagle_ray", 0.2)



### Densities

#Map species densities per grid cell (per date)
map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain", 0.05)
map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", 0.05)
map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", 0.05)
map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", 0.05)
map_dens_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", 0.05)

#Map species densities per grid cell (all dates) LOG with zero
map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain", 0.2)
map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", 0.2)
map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", 0.2)
map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", 0.2)
map_dens_per_grid_species_log_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", 0.2)





############################################ CREATE DENSITY RASTERS #############################################################


#Make dataframe per grid cell centers (including empty cell centers) of observations, track length and densities per date and per species 
df_all_species = make_df_all_species(grid_obs_per_date, grid_tracks_per_date, footprint_width) #desnity in ind / ha

#get raster stack of desnity for given species
r_density_Dugong_certain = get_density_stack_species(df_all_species, "Dugong_certain")
save(r_density_Dugong_certain, file = "data/processed_data/raster_Dugong_certain.RData")


#tO do 
#merge dugong probable and certain
#correct availability bias in density
