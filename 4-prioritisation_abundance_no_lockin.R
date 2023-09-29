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





###--------------------------------- BLM calibration (minimum set objective, with penalty, lock in **** varying target *****)  ----------------------------


# BLM Calibration
# Domisch et al 2018 appproach : identified the elbow, that is, the point from where an increase
# in the compactness (higher BLM values) has no major effect on the
# connectivity in spatial plans anymore. 

calibrate_blm_round1(pred_region, 0.1)
calibrate_blm_round2(pred_region, 0.1, seq(0.001, 0.01, 0.001))
#-> selected BLM 0.003

calibrate_blm_round1(pred_region, 0.2)
calibrate_blm_round2(pred_region, 0.2, seq(0.001, 0.01, 0.001))
#-> selected BLM 0.008

calibrate_blm_round1(pred_region, 0.3)
calibrate_blm_round2(pred_region, 0.3, seq(0.1, 1, 0.1))
#-> selected BLM 0.7

calibrate_blm_round1(pred_region, 0.4)
calibrate_blm_round2(pred_region, 0.4, seq(0.1, 1, 0.1))
#-> selected BLM 0.7

calibrate_blm_round1(pred_region, 0.5)
calibrate_blm_round2(pred_region, 0.5, seq(0.1, 1, 0.1))
#-> selected BLM 0.8

calibrate_blm_round1(pred_region, 0.6)
calibrate_blm_round2(pred_region, 0.6, seq(0.1, 1, 0.1))
#-> selected BLM 0.2

calibrate_blm_round1(pred_region, 0.7)
calibrate_blm_round2(pred_region, 0.7, seq(0.01, 0.1, 0.01))
#-> selected BLM 0.07

calibrate_blm_round1(pred_region, 0.8)
calibrate_blm_round2(pred_region, 0.8, seq(0.01, 0.1, 0.01))
#-> selected BLM 0.05

calibrate_blm_round1(pred_region, 0.9)
calibrate_blm_round2(pred_region, 0.9, seq(0.1, 1, 0.1))
# #-> selected BLM 0.2





###--------------------------------- minimum set objective, with penalty, locked in **** varying target ***** ----------------------------


#--------------target 0.1 and selected blm

t <- 0.1
p <- 0.003

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)


#--------------target 0.2 and selected blm

t <- 0.2
p <- 0.008

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)



#--------------target 0.3 and selected blm

t <- 0.3
p <- 0.7

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)




#--------------target 0.4 and selected blm

t <- 0.4
p <- 0.7

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)




#--------------target 0.5 and selected blm

t <- 0.5
p <- 0.8

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)




#--------------target 0.6 and selected blm

t <- 0.6
p <- 0.2

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)



#--------------target 0.7 and selected blm

t <- 0.7
p <- 0.07

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)



#--------------target 0.8 and selected blm

t <- 0.8
p <- 0.05

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)


#--------------target 0.9 and selected blm

t <- 0.9
p <- 0.2

r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

#number of cells in prioritization
assign(paste0("reserve_ncells_", t), raster::cellStats(r, "sum")) 

#surface in prioritization
assign(paste0("reserve_surf_abundance_", t), raster::cellStats(r, "sum") * 0.25) 

#map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, ".png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of abundance - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#map reserves with map osm overlaid with predicted abundance
map_reserves_minset_with_penalty_target_osm_abundance(maplatlon_proj, pred_region, r, mpas_notake_shp, p, t)

# write raster
raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_abondance_no_lockin_", t*100)), overwrite=TRUE)




###--------------------------------- plot reserve surface vs target ----------------------------


reserve_surf_abundance_no_lockin <- c(reserve_surf_abundance_0.1, reserve_surf_abundance_0.2, reserve_surf_abundance_0.3, reserve_surf_abundance_0.4, reserve_surf_abundance_0.5,
                                      reserve_surf_abundance_0.6, reserve_surf_abundance_0.7, reserve_surf_abundance_0.8, reserve_surf_abundance_0.9)

targets <- seq(10, 90, 10)
  
png(here::here("outputs/prioritisation/plot_reserve_surf_abundance_vs_target_no_lockin.png"), width = 960, height = 960)
par(mar = c(8,8,3,6))
plot(reserve_surf_abundance_no_lockin, targets,
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



