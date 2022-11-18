
#load all functions
devtools::load_all()




######################################## DEFINE STUDY AREA ########################################

#Define study area coordinates
#big region
lat1 =  -21.2 ; lat2 = -22
lon1 = 164.6 ; lon2 = 166


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

#Clean duplicated obs from telemetry
#telemetry_obs = clean_duplicated_obs_telemetry(telemetry, overlap, image_height) (this code takes about 20 minutes to process)
#alternatively read the Rdata to read the processed data directly (telemetry_obs)
load("data/processed_data/telemetry_obs.RData") 

#Read video information
videos = read_video_info()

#Clean video information
videos = clean_video_info(videos)

#Get list of west coast videos
ls_videos = list_wcoast_videos(videos)

#select video information for west coast
videos = select_wcoast_videos(videos, ls_videos)

#Join video information to telemetry
telemetry = join_wcoast_video_info_telem(videos, telemetry)

#select west coast observations
telemetry_obs = select_obs_telemetry_wcoast(telemetry_obs, ls_videos)

#count total number of individuals observed per species
telemetry_obs %>%
  dplyr::group_by(object) %>%
  dplyr::summarise(n_tot = dplyr::n())







######################################## READ CORAL DATA ########################################

#read Allen coral benthic polygon
allen_coral = read_crop_and_convert_allen_coralnc_benthic(lon1, lon2, lat2, lat1)

# make study area raster (resolution 0.005 degrees)
rast = make_area_raster(lat1, lon1, lat2, lon2, 0.05)

# make coral raster PROBLEM



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


###### Map Allen coral polygons benthic PROBLEM
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









############################################ MAKE DENSITY MAPS ON REGULAR GRID ON EFFORT #############################################################
