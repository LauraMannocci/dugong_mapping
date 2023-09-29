# tutoriel https://cran.r-project.org/web/packages/prioritizr/vignettes/prioritizr.html
# https://cran.r-project.org/web/packages/prioritizr/vignettes/saltspring.html

#gurobi installation
#manual download following instructions https://www.gurobi.com/academia/academic-program-and-licenses/ THEN
#install.packages('C:/gurobi911/win64/R/gurobi_9.1-1.zip', repos=NULL)
#install.packages('slam')


#load all functions
devtools::load_all()




######################################## LOAD RASTERS ######################################## 

#read NC land
shp = rgdal::readOGR(here::here("data", "raw_data", "nc_land", "NOUVELLE_CALEDONIE.shp"))
raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia

# regional prediction raster interp
pred_region = raster::raster(here::here("data", "processed_data", "predictions", "predictions_region_interp.grd"))
mean(raster::values(pred_region), na.rm = T)
sd(raster::values(pred_region), na.rm = T)
range(raster::values(pred_region), na.rm = T)

# distance to land
dist_land = raster::raster(here::here("data", "processed_data", "predictors", "dist_land_no_mask.grd"))

#shapefile mpas no take
mpas <- read_mpanc()
mpas_notake_shp <- extract_notake_mpas(mpas)

#proportion of densities covered by no take mpas 
pred_region_mpa = raster::mask(pred_region, mpa, maskvalue = 0)
raster::cellStats(pred_region_mpa, 'sum') #17 indiv
raster::cellStats(pred_region_mpa, 'sum') / raster::cellStats(pred_region, 'sum') #9.4%



######################################## DEFINE STUDY AREA AND CORRESPONDING RASTERS ########################################

#Define study area coordinates
#big region
lat1 =  -21.2 ; lat2 = -21.9
lon1 = 164.8 ; lon2 = 165.8

#study area raster resolution in meters
raster_res_m = 500

# make study area raster projected to lambert New caledonia in meters with given resolution in meters (resolution 500 meters) 
rast_xy = make_area_raster_xy(lat1, lon1, lat2, lon2, raster_res_m)

# read and project surveyed bloc (created in qgis)
surv_block = read_project_surveyed_block()

#open street map
maplatlon = osm_map(lat1, lon1, lat2, lon2)

#project osm for density mapping
maplatlon_proj = osm_mapproj(maplatlon)



######################################## PREPROCESSING ######################################## 

#set cost to 1 in study area
rast_xy[] <- 1 

#mask with land
rast_xy <- mask_with_land(rast_xy, dist_land)

# mask study area raster with prediction raster so that planning units are only in coral reefs area
rast_xy <- raster::mask(rast_xy, pred_region, updateValue = NA)




######################################## PRIORITISATION ON ABUNDANCE ######################################## 



###--------------------------------- minimum set objective, no penalty, **** varying target ***** ----------------------------


for (t in seq(0.1, 0.9, 0.1)){
  
  r <- design_reserves_minset_no_penalty(rast_xy, pred_region, t)
  assign(paste0("prioritisation_abundance_", t), r) 
  
  #number of cells in prioritization
  assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 
  
  #surface in prioritization
  assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 
  
  #map
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, ".png"))), width = 800, height = 480)
  raster::plot(r, main = paste("protecting", t*100, "% of abundance"), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
  #map reserves with map osm overlaid with predicted abundance
  map_reserves_minset_no_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, t)
  
  #map reserves with map osm overlaid with predicted abundance new
  map_reserves_minset_no_penalty_target_osm_abundance_new(maplatlon_proj, r, mpas_notake_shp, t)
  
  # write raster
  raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_no_penalty_", t*100)), overwrite=TRUE)
  
}




###--------------------------------- multimap 30 50 80 % ----------------------------


map_reserves_minset_no_penalty_target_osm_abundance_new_multi(maplatlonproj, prioritisation_abundance_0.3, prioritisation_abundance_0.5, prioritisation_abundance_0.8, mpa) #run outside function


###--------------------------------- multimap 10-90 %----------------------------


png(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_abundance_multi.png"))
par(mar = c(0,0,1.5,0), mfrow = c(3,3))
#10%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "10% abundance")
raster::plot(prioritisation_abundance_0.1, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#20%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "20% abundance")
raster::plot(prioritisation_abundance_0.2, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#30%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "30% abundance")
raster::plot(prioritisation_abundance_0.3, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#40%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "40% abundance")
raster::plot(prioritisation_abundance_0.4, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#50%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "50% abundance")
raster::plot(prioritisation_abundance_0.5, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#60%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "60% abundance")
raster::plot(prioritisation_abundance_0.6, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#70%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "70% abundance")
raster::plot(prioritisation_abundance_0.7, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#80%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "80% abundance")
raster::plot(prioritisation_abundance_0.8, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#90%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "90% abundance")
raster::plot(prioritisation_abundance_0.9, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
dev.off()





###--------------------------------- plot reserve surface vs target ----------------------------


reserve_surf_abundance_no_lockin_no_penalty <- c(reserve_surf_abundance_0.1, reserve_surf_abundance_0.2, reserve_surf_abundance_0.3, reserve_surf_abundance_0.4, reserve_surf_abundance_0.5,
                                                 reserve_surf_abundance_0.6, reserve_surf_abundance_0.7, reserve_surf_abundance_0.8, reserve_surf_abundance_0.9)

targets <- seq(10, 90, 10)
  
png(here::here("outputs/prioritisation/plot_reserve_surf_abundance_vs_target_no_lockin_no_penalty.png"), width = 960, height = 960)
par(mar = c(8,8,3,6))
plot(reserve_surf_abundance_no_lockin_no_penalty, targets,
     type = "b",
     las = 2,
     col = "cyan", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "")
axis(2, cex.axis=2.2, at = seq(10,90,10), labels= paste0(targets, "%"), las = 1)
title(xlab = expression(paste("Reserve surface (", "km"^"2", ")")), cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.2, las=1)
title(ylab = "% Abundance protected", cex.lab = 3, line = 5)
#add current MPA (70 km 2 protecting 7% of desnities)
# segments(-0.3, 70, 0.7, 70, col = "darkorange", lty = 3, lwd = 3)
# segments(0.7, -30, 0.7, 70, col = "darkorange", lty = 3, lwd = 3)
# mtext("Current", 2, 1, at = 78, las = 2, col = "darkorange", cex = 2.5)
# mtext("MPAs", 2, 2, at = 52, las = 2, col = "darkorange", cex = 2.5)
box()
dev.off()



