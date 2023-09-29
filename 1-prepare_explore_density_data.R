
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
# rast_latlon = make_area_raster_latlon(lat1, lon1, lat2, lon2, 0.01)

# make study area raster projected to lambert New caledonia in meters with given resolution in meters (resolution 500 meters) 
rast_xy = make_area_raster_xy(lat1, lon1, lat2, lon2, raster_res_m)


# make basic map of New caledonia weith surveyed block
new_caledonia <- raster::getData("GADM", country = "NCL", level = 1)
surv_block = read_surveyed_block()
map_new_caledonia_surveyed_block(new_caledonia, surv_block)




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

#select telemetry in dates
telemetry = subset(telemetry, telemetry$date %in% c("2021-06-04", "2021-06-05", "2021-07-29", "2021-08-04"))

length(unique(telemetry$image_id)) #74375 total images

#select dugong observations
telemetry_obs = subset(telemetry, telemetry$object == "Dugong_certain_probable" )


#count total number of individuals observed 
telemetry_obs %>%
  dplyr::group_by(object) %>%
  dplyr::summarise(n_tot = dplyr::n()) #1648





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

#total on effort images
nrow(telemetry_on) #68599 images

#number of images countaining dugongs
telemetry_on %>% 
  dplyr::filter(object == "Dugong_certain_probable") %>% 
  dplyr::distinct(image_id) -> distinct

nrow(distinct) #168




###################################### REMOVE DUPLICATED DUGONG OBSERVATIONS #####################################

#IMPORTANT this step is partly done outside or R 

#export observations with duplicates
xlsx::write.xlsx(telemetry_obs_on, here::here("data", "processed_data", "density", "obs_with_duplicates.xlsx"))

#outside of R: copy this xlsx file, rename it to obs_duplicates_flagged.xlsx and 
#fill out new column "duplicate" (yes/no) by looking at each observation in the custom annotation applicaion
#also fill out column "stage" (adult/juvenile)
#also fill out column "eci" (1/2/3/4 ie Environmental Conditions Index following Hagihara et al 2016)

#import observations with duplicates flagged
telemetry_obs_on_dupl_flag = xlsx::read.xlsx(here::here("data", "processed_data", "density", "obs_duplicates_flagged.xlsx"), sheetIndex = 1)

#remove duplicate observations
telemetry_obs_on_dupl_rm = subset(telemetry_obs_on_dupl_flag, telemetry_obs_on_dupl_flag$duplicate == "no")
nrow(telemetry_obs_on_dupl_rm) #119 unique observations
table(telemetry_obs_on_dupl_rm$stage) # 100 adults 19 juveniles


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
sum(telemetry_obs_on$n_count_avail_corrected) #corrected abundance
nrow(telemetry_obs_on) #uncorrected abundance

#number of images countaining dugongs
telemetry_obs_on %>% 
  # dplyr::group_by(image_id) %>% 
  dplyr::distinct(image_id) -> distinct

nrow(distinct) #35

############################################ MAKE BASIC MAPS ON EFFORT #############################################################


#open street map
maplatlon = osm_map(lat1, lon1, lat2, lon2)

#open street map
maplatlon_extended = osm_map(lat1, lon1, lat2, lon2+0.05)

###### telemetry only
map_telemetry(maplatlon, telemetry_on)

###### telemetry with one color per date
map_telemetry_date(maplatlon, telemetry_on)

###### telemetry with separate map per date
map_telemetry_date_separate(maplatlon, telemetry_on)


#NB the below maps are nb of indiv ***uncorrected for avail bias*** 
####### telemetry with individual dugong observations (1 dot = 1 obs centered on image so dots can be overlaid)
map_indiv_dugong_telemetry(maplatlon, telemetry_on, telemetry_obs_on)

####### telemetry with individual dugong observations (1 dot = 1 obs centered on image so dots can be overlaid) with number <=3 and >3
map_indiv_dugong_telemetry_inf_sup_3(maplatlon, telemetry_on, telemetry_obs_on)

####### telemetry with individual dugong observations differenciating stage  (1 dot = 1 obs centered on image so dots can be overlaid)
map_indiv_dugong_stage_telemetry(maplatlon, telemetry_on, telemetry_obs_on)

####### telemetry with individual dugong observations per image (obs summed per image, duplicates removed)
map_indiv_dugong_per_image_telemetry(maplatlon, telemetry_on, telemetry_obs_on)

####### telemetry with individual dugong observations per image (obs summed per image, duplicates removed) per date
map_indiv_dugong_per_image_telemetry_per_date(maplatlon, telemetry_on, telemetry_obs_on)

####### telemetry with individual dugong observations per image (obs summed per image, duplicates removed) with no-take maps
map_indiv_dugong_per_image_telemetry_no_take_mpas(maplatlon_extended, telemetry_on, telemetry_obs_on, mpas_notake)

####### telemetry with individual dugong observations per image (obs summed per image, duplicates removed) with no-take maps with number <= 3 and > 3
map_indiv_dugong_per_image_telemetry_no_take_mpas_inf_sup_3(maplatlon_extended, telemetry_on, telemetry_obs_on, mpas_notake)

####### telemetry with individual dugong observations per image (obs summed per image)
map_indiv_dugong_per_image_stage_telemetry(maplatlon, telemetry_on, telemetry_obs_on)



###### telemetry and transects lines
map_telemetry_transects(maplatlon, telemetry_on, lines)


###### surveyed transects 
lines_surveyed <- subset(lines, ! lines$id %in% c( "2_33","2_35","2_36", "2_37" ,"2_38", "3_49", "3_50", "3_51" ,"3_52" ,"3_53"))
map_transects(maplatlon, lines_surveyed)


###### surveyed transects with mpas
lines_surveyed <- subset(lines, lines$id %in% c(paste0("3_", 1:48), paste0("1_", 1:24), paste0("2_", 1:33)))
mpas <- read_mpanc()
mpas2 <- extract_mpas(mpas)
#extracted mpas
map_transects_mpas(maplatlon, lines_surveyed, mpas2)
#no take only
mpas_notake <- extract_notake_mpas(mpas)
map_transects_notake_mpas(maplatlon, lines_surveyed, mpas_notake)
#big mpas only (run outside function)
map_new_caledonia_big_mpas(mpas, lines_surveyed)



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
sum(grid_tracks_per_date$length, na.rm=T) #(1145561  m) 696093.7 m

#Mean surveyed length per survey date
sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #174023.4 m

#Total surveyed area
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) # (101652630 m2 =  101.6 km2) 61768657 m2 = 62 km2

#Mean surveyed area per survey date
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #14521804 m2 = 14.2 km2 (15442164 m2 = 15.4 km2)

#Map length of tracks per grid cell per date
map_tracklen_per_grid_per_date(maplatlon_proj, grid_tracks_per_date, 0.5)

#Map length of tracks per grid cell (all dates)
map_tracklen_per_grid(maplatlon_proj, grid_tracks_per_date, 0.5)



### Observations

#Count total number of observations per grid cell per date
grid_obs_per_date = count_obs_per_grid_per_date(grid, telemetry_obs_on)

#Map number of species observations per grid cell per date
map_obs_per_grid_per_date_species(maplatlon_proj, grid_obs_per_date, "Dugong_certain_probable", 0.2)

#Map number of species observations per grid cell per date with zeros
map_obs_per_grid_per_date_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Dugong_certain_probable", 0.2)

#Map number of species observations per grid cell (all dates)
map_obs_per_grid_species(maplatlon_proj, grid_obs_per_date, "Dugong_certain_probable", 0.5)

#Map number of species observations per grid cell (all dates) with zeros 
map_obs_per_grid_species_with_zero(maplatlon_proj, grid_obs_per_date, grid_tracks_per_date, "Dugong_certain_probable", 0.5)


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



############################################ CREATE DENSITY RASTER #############################################################


#Make dataframe per grid cell centers (including empty cell centers) of observations, track length and densities per date and per species 
df_all_species = make_density_df_all_species(grid_obs_per_date, grid_tracks_per_date, footprint_width) #density in indiv / 10000m2 (=0.1 km2 or 100m x 100m)

#get raster stack of density for given species
r_density_dugong = get_density_stack_species(df_all_species, "Dugong_certain_probable")

#extend raster to study area raster
r_density_dugong = extend_raster(r_density_dugong, rast_xy)

#write
#raster::writeRaster(r_density_dugong[[7]], here::here("data", "processed_data", "density", "dugong_density.grd"), overwrite=TRUE)
raster::writeRaster(r_density_dugong[[4]], here::here("data", "processed_data", "density", "dugong_density.grd"), overwrite=TRUE)




############################################ CREATE ABUNDANCE AND SURVEYED AREA RASTER #############################################################

#Make dataframe per grid cell centers (including empty cell centers) of abundance and surveyed area
df_abundance_dugong = make_abundance_df(grid_obs_per_date, grid_tracks_per_date, footprint_width) 

#convert to stack
r_abundance_dugong <- raster::stack()
r_abundance_dugong = raster::stack(r_abundance_dugong, raster::rasterFromXYZ(df_abundance_dugong[,c("lon", "lat", "n_Dugong_certain_probable", "area_surveyed_m2")],  crs="+init=epsg:3163"))

#map abundance
map_abundance_per_grid_species_with_zeros(maplatlon_proj, r_abundance_dugong[[1]], "Dugong_certain_probable", 0.5)

#mean and sd
mean(raster::values(r_abundance_dugong[[1]]), na.rm=T)
sd(raster::values(r_abundance_dugong[[1]]), na.rm=T)

#map surveyed area
map_surveyed_area_per_grid_species_with_zeros(maplatlon_proj, r_abundance_dugong[[2]], "Dugong_certain_probable", 0.5)

#extend raster to study area raster
r_abundance_dugong = extend_raster(r_abundance_dugong, rast_xy)

#write abundance and surveyed area rasters
raster::writeRaster(r_abundance_dugong[[1]], here::here("data", "processed_data", "abundance", "dugong_abundance.grd"), overwrite=TRUE)
raster::writeRaster(r_abundance_dugong[[2]], here::here("data", "processed_data", "abundance", "area_surveyed.grd"), overwrite=TRUE)



############################################ MAKE ABUNDANCE AND SURVEYED AREA MAPS #############################################################
