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
raster::cellStats(pred_region_mpa, 'sum') #118 indiv
prop_in_mpa = raster::cellStats(pred_region_mpa, 'sum') / raster::cellStats(pred_region, 'sum') 
prop_in_mpa #11.2%

pred_region_mpa[pred_region_mpa > 0] <- 1
cells_in_mpas = raster::cellStats(pred_region_mpa, "sum") #nb of cells in mpa 231
cells_in_mpas

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




######################################## --------- PRIORITISATION ON ABUNDANCE -------------- ######################################## 



###--------------------------------- minimum set objective, no penalty, no lockin **** varying target ***** ----------------------------


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




###--------------------------------- multimap 30 50 70 % ----------------------------


map_reserves_minset_no_penalty_target_osm_abundance_new_multi(maplatlonproj, prioritisation_abundance_0.3, prioritisation_abundance_0.5, prioritisation_abundance_0.7, mpa) #run outside function




###--------------------------------- multimap 10-90 %----------------------------


png(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_abundance_multi.png"))
par(mar = c(0,0,1.5,0), mfrow = c(3,3))
#10%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "10% target")
raster::plot(prioritisation_abundance_0.1, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#20%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "20% target")
raster::plot(prioritisation_abundance_0.2, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#30%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "30% target")
raster::plot(prioritisation_abundance_0.3, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#40%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "40% target")
raster::plot(prioritisation_abundance_0.4, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#50%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "50% target")
raster::plot(prioritisation_abundance_0.5, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#60%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "60% target")
raster::plot(prioritisation_abundance_0.6, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#70%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "70% target")
raster::plot(prioritisation_abundance_0.7, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#80%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "80% target")
raster::plot(prioritisation_abundance_0.8, legend = FALSE, add = TRUE, col = c("grey92", "dark blue"))
#90%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "90% target")
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
title(ylab = "% Population protected", cex.lab = 3, line = 5)
#add current MPA (70 km 2 protecting 7% of desnities)
# segments(-0.3, 70, 0.7, 70, col = "darkorange", lty = 3, lwd = 3)
# segments(0.7, -30, 0.7, 70, col = "darkorange", lty = 3, lwd = 3)
# mtext("Current", 2, 1, at = 78, las = 2, col = "darkorange", cex = 2.5)
# mtext("MPAs", 2, 2, at = 52, las = 2, col = "darkorange", cex = 2.5)
box()
dev.off()










######################################## --------- PRIORITISATION ON RANGE -------------- ######################################## 




#convert predicted abundance to range
range <- pred_region
range[range > 0] <- 1


###--------------------------------- minimum set objective, no penalty, no lock in **** varying target ***** ----------------------------

set.seed(1)

for (t in seq(0.1, 0.9, 0.1)){
  
  print('********************* target *********************')
  print(t)
  
  r <- design_reserves_minset_no_penalty_randomize(rast_xy, range, t) #output is a portofolio of 10 solutions
  
  #extract first solution of the portofolio
  assign(paste0("prioritisation_range_", t), r[[1]]) 
  
  #map all 10 solutions
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_randomize_range.png"))), width = 800, height = 480)
  raster::plot(r, main = paste("protecting", t*100, "% of range"), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
  #map solution 1 of the 10 solutions
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_randomize_range_sol1.png"))), width = 800, height = 480)
  raster::plot(r[[1]], main = paste("protecting", t*100, "% of range"), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
  #map reserves with map osm overlaid with range (solution 1 of 10 solutions)
  map_reserves_minset_no_penalty_target_osm_range(maplatlon_proj, range, r[[1]], mpas_notake_shp, t)
  
  #map reserves with map osm overlaid with range (solution 1 of 10 solutions) new
  map_reserves_minset_no_penalty_target_osm_range_new(maplatlon_proj, r[[1]], mpas_notake_shp, t)
  
  # write raster stack (10 solutions and solution 1 of 10)
  raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_range_no_lockin_", t*100)), overwrite=TRUE)
  raster::writeRaster(r[[1]], here::here("data", "processed_data", "prioritisation", paste0("prioritisation_range_no_lockin_sol1_", t*100)), overwrite=TRUE)
  
  #mask prediction with reserve to get percentage of abundance protected
  pred_region_mask <- raster::mask(pred_region, r[[1]], maskvalue = 1, inverse = T)
  
  
  #get percentage of protected abundnace per solution
  percent_protected <- sum(raster::values(pred_region_mask), na.rm = T) /  sum(raster::values(pred_region), na.rm = T)
  print('********************* percent of abundance protected *********************')
  print(percent_protected)
  assign(paste0("percent_protected_range_", t), percent_protected)
  
  
  #get surface in prioritization per solution
  surf <- raster::cellStats(r[[1]], "sum") * 0.25 #conversion in km2
  print('********************* surf *********************')
  print(surf)
  assign(paste0("reserve_surf_range_", t), surf) #compute the mean as the surface of all solutions is the same
  
}





###--------------------------------- multimap 30 50 70 % ----------------------------


map_reserves_minset_no_penalty_target_osm_range_new_multi(maplatlonproj, prioritisation_range_0.3, prioritisation_range_0.5, prioritisation_range_0.7, mpa) #run outside function




###--------------------------------- multimap 10-90 % ----------------------------


png(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_range_multi.png"))
par(mar = c(0,0,1.5,0), mfrow = c(3,3))
#10%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "10% target")
raster::plot(prioritisation_range_0.1, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#20%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "20% target")
raster::plot(prioritisation_range_0.2, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#30%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "30% target")
raster::plot(prioritisation_range_0.3, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#40%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "40% target")
raster::plot(prioritisation_range_0.4, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#50%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "50% target")
raster::plot(prioritisation_range_0.5, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#60%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "60% target")
raster::plot(prioritisation_range_0.6, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#70%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "70% target")
raster::plot(prioritisation_range_0.7, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#80%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "80% target")
raster::plot(prioritisation_range_0.8, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#90%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "90% target")
raster::plot(prioritisation_range_0.9, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
dev.off()





###--------------------------------- plot reserve surface vs target no lockin no penalty ----------------------------


reserve_surf_range_no_lockin <- c(reserve_surf_range_0.1, reserve_surf_range_0.2, reserve_surf_range_0.3, reserve_surf_range_0.4, reserve_surf_range_0.5,
                                  reserve_surf_range_0.6, reserve_surf_range_0.7, reserve_surf_range_0.8, reserve_surf_range_0.9)

percent_protected_range_no_lockin <- c(percent_protected_range_0.1, percent_protected_range_0.2, percent_protected_range_0.3, percent_protected_range_0.4, percent_protected_range_0.5,
                                       percent_protected_range_0.6, percent_protected_range_0.7, percent_protected_range_0.8, percent_protected_range_0.9)




png(here::here("outputs/prioritisation/plot_reserve_surf_abundance_range_vs_target_no_lockin_no_penalty.png"), width = 960, height = 960)
#grey backgroundp
r(bg="white")
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
u <- par("usr") # The coordinates of the plot area
rect(-0.874, -0.895, 0.875, 1.05, col="grey90", border=NA)
par(mar = c(8,8,3,6), new = TRUE)
plot(reserve_surf_range_no_lockin, percent_protected_range_no_lockin*100,
     type = "p",
     las = 2,
     col = "brown", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 800), ylim = c(0, 100))
axis(2, cex.axis=2.3, at = seq(0,100,10), labels= paste0( seq(0, 100, 10), "%"), las = 1, pos = 0)
title(xlab = expression(paste("Reserve surface area (", "km"^"2", ")")), cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.3, las=1, pos = 0, at = seq(0,800,100))
title(ylab = "% Population protected", cex.lab = 3, line = 6)
points(reserve_surf_abundance_no_lockin_no_penalty, targets, col = "yellow", pch = 16, cex = 3)
#add abundance protected in existing reserve surface  
points(cells_in_mpas*0.25, prop_in_mpa*100, col = "orange", pch = 17, cex = 3)
legend(400, 16, legend=c("Prioritization on abundance", "Prioritization on area", "Existing no-take MPAs"),
       col=c("yellow", "brown", "orange"), pch = c(16, 16, 17), cex=2)
dev.off()


mean(unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin_no_penalty)
reserve_surf_range_no_lockin/reserve_surf_abundance_no_lockin_no_penalty





###--------------------------------- plot nb reserve cells vs target no lockin no penalty ----------------------------


png(here::here("outputs/prioritisation/plot_reserve_nb_cells_abundance_range_vs_target_no_lockin_no_penalty.png"), width = 960, height = 960)
#grey backgroundp
r(bg="white")
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
u <- par("usr") # The coordinates of the plot area
rect(-0.935, -0.97, 0.9, 1.1, col="grey90", border=NA)
par(mar = c(8,8,3,6), new = TRUE)
plot(reserve_surf_range_no_lockin/0.25, percent_protected_range_no_lockin*100,
     type = "p",
     las = 2,
     col = "brown", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 3300), ylim = c(0, 100))

axis(2, cex.axis=2.2, at = seq(0,100,10), labels= paste0( seq(0, 100, 10), "%"), las = 1)
title(xlab = "Nb of reserve cells", cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.2, las=1)
title(ylab = "% Population protected", cex.lab = 3, line = 6)
points(reserve_surf_abundance_no_lockin_no_penalty/0.25, targets, col = "yellow", pch = 16, cex = 3)
#add abundance protected in existing reserve surface  
points(cells_in_mpas, prop_in_mpa*100, col = "orange", pch = 17, cex = 3)
legend(1, 98, legend=c("Prior. on abundance", "Prior. on range"),
       col=c("yellow", "brown"), pch = 16, cex=2)
dev.off()


mean(unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin_no_penalty)
unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin_no_penalty


mean(percent_protected_range_no_lockin[41:50])




###--------------------------------- plot % area protected vs target no lock in no penalty ----------------------------

#number of cells in study area
nbcells = raster::freq(rast_xy, value=1) # 3261

reserve_percent_range_no_lockin = 100*(reserve_surf_range_no_lockin/0.25)/nbcells
reserve_percent_abundance_no_lockin_no_penalty = 100*(reserve_surf_abundance_no_lockin_no_penalty/0.25)/nbcells

png(here::here("outputs/prioritisation/plot_reserve_percentage_abundance_range_vs_target_no_lockin_no_penalty.png"), width = 960, height = 960)
#grey backgroundp
r(bg="white")
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
u <- par("usr") # The coordinates of the plot area
rect(-0.874, -0.895, 0.875, 1.05, col="grey90", border=NA)
par(mar = c(8,8,3,6), new = TRUE)
plot(reserve_percent_range_no_lockin, percent_protected_range_no_lockin*100,
     type = "p",
     las = 2,
     col = "brown", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 100), ylim = c(0, 100))

axis(2, cex.axis=2.3, at = seq(0,100,10), las = 1, pos = 0)
title(xlab = "% Spatial protection coverage", cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.3, las=1, at = seq(0,100,10), pos = 0) 
title(ylab = "% Population protected", cex.lab = 3, line = 5)
points(reserve_percent_abundance_no_lockin_no_penalty, targets, col = "yellow", pch = 16, cex = 3)
#add abundance protected in existing reserve surface  
points(100*(cells_in_mpas/nbcells), prop_in_mpa*100, col = "orange", pch = 17, cex = 3)
legend(47, 16, legend=c("Prioritization on abundance", "Prioritization on area (control)", "Existing no-take MPAs"),
       col=c("yellow", "brown", "orange"), pch = c(16, 16, 17), cex=2)
dev.off()

targets/reserve_percent_abundance_no_lockin_no_penalty

reserve_percent_range_no_lockin/reserve_percent_abundance_no_lockin_no_penalty





########### calculating mean reserve patch surface


for (t in seq(0.1, 0.9, 0.1)){
  
  print('********************* target *********************')
  print(t)
  
  ### range
  prio = get(paste0("prioritisation_range_", t))
  
  surf_patch_range = calc_reserve_patch_surfaces(prio)
  assign(paste0("mean_surf_patch_range_", t), mean(surf_patch_range))
  assign(paste0("sd_surf_patch_range_", t), sd(surf_patch_range))
  
  ### abundance
  prio = get(paste0("prioritisation_abundance_", t))
  
  surf_patch_abundance = calc_reserve_patch_surfaces(prio)
  assign(paste0("mean_surf_patch_abundance_", t), mean(surf_patch_abundance))
  assign(paste0("sd_surf_patch_abundance_", t), sd(surf_patch_abundance))
  
}

mean_surf_patch_range = c(mean_surf_patch_range_0.1, mean_surf_patch_range_0.2, mean_surf_patch_range_0.3, mean_surf_patch_range_0.4,
                          mean_surf_patch_range_0.5, mean_surf_patch_range_0.6, mean_surf_patch_range_0.7,
                          mean_surf_patch_range_0.8, mean_surf_patch_range_0.9)

sd_surf_patch_range = c(sd_surf_patch_range_0.1, sd_surf_patch_range_0.2, sd_surf_patch_range_0.3, sd_surf_patch_range_0.4,
                        sd_surf_patch_range_0.5, sd_surf_patch_range_0.6, sd_surf_patch_range_0.7,
                        sd_surf_patch_range_0.8, sd_surf_patch_range_0.9)


mean_surf_patch_abundance = c(mean_surf_patch_abundance_0.1, mean_surf_patch_abundance_0.2, mean_surf_patch_abundance_0.3, mean_surf_patch_abundance_0.4,
                              mean_surf_patch_abundance_0.5, mean_surf_patch_abundance_0.6, mean_surf_patch_abundance_0.7,
                              mean_surf_patch_abundance_0.8, mean_surf_patch_abundance_0.9)

sd_surf_patch_abundance = c(sd_surf_patch_abundance_0.1, sd_surf_patch_abundance_0.2, sd_surf_patch_abundance_0.3, sd_surf_patch_abundance_0.4,
                            sd_surf_patch_abundance_0.5, sd_surf_patch_abundance_0.6, sd_surf_patch_abundance_0.7,
                            sd_surf_patch_abundance_0.8, sd_surf_patch_abundance_0.9)





png(here::here("outputs/prioritisation/plot_mean_reserve_patch surface.png"), width = 960, height = 960)
#grey backgroundp
r(bg="white")
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
u <- par("usr") # The coordinates of the plot area
rect(-0.874, -0.905, 0.875, 100, col="grey90", border=NA)
par(mar = c(8,8,3,6), new = TRUE)
plot(targets, mean_surf_patch_abundance,
     type = "p",
     las = 2,
     col = "yellow", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 100), ylim = c(min(mean_surf_patch_range, mean_surf_patch_abundance), max(mean_surf_patch_range, mean_surf_patch_abundance)))

axis(2, cex.axis=2.3, at = seq(0, 200, 10), las = 1, pos = 0)
title(xlab = "% Protected abundance or range", cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.3, las=1, at = seq(0,100,10), pos = 0)
title(ylab = "Mean reserve patch surface", cex.lab = 3, line = 6)
points(targets, mean_surf_patch_range, col = "brown", pch = 16, cex = 3)
legend(10, 170, legend=c("Prioritization on abundance", "Prioritization on area"),
       col=c("yellow", "brown"), pch = c(16, 16), cex=2)
dev.off()



########### calculating spatial aggregation index (for categorical data only)

# abundance prioritisation
landscapemetrics::lsm_l_ai(prioritisation_abundance_0.5) #87.4
landscapemetrics::lsm_c_ai(prioritisation_abundance_0.5) #0 (no reserve): 90.1 / 1 (reserve): 56.0 

# range prioritisation
landscapemetrics::lsm_l_ai(prioritisation_range_0.5) #48.3
landscapemetrics::lsm_c_ai(prioritisation_range_0.5) #0: 49.5 / 1:46.8





######### calculating compactness




for (t in seq(0.1, 0.9, 0.1)){
  
  print('********************* target *********************')
  print(t)
  
  
  ### abundance
  prio = get(paste0("prioritisation_abundance_", t))
  
  #---- abundance prioritisation
  #convert raster to polygon  
  r_abundance = raster::rasterToPolygons(prio, fun=function(x){x==1}, n=8, na.rm=TRUE, digits=12, dissolve=FALSE)
  r2_abundance = sf::st_as_sf(r_abundance)
  
  #calculate Polsby-Popper Compactness
  comp = redistmetrics::comp_polsby(r2_abundance$layer, r2_abundance)
  assign(paste0("compactness_abundance_", t), comp)
  
  
  ### range
  prio = get(paste0("prioritisation_range_", t))
  
  #---- range prioritisation raster
  #convert raster to polygon  
  r_range = raster::rasterToPolygons(prio, fun=function(x){x==1}, n=8, na.rm=TRUE, digits=12, dissolve=FALSE)
  r2_range = sf::st_as_sf(r_range)
  
  #calculate Polsby-Popper Compactness
  comp = redistmetrics::comp_polsby(r2_range$solution_2, r2_range)
  assign(paste0("compactness_range_", t), comp)

  
}



compactness_abundance = c(compactness_abundance_0.1, compactness_abundance_0.2, compactness_abundance_0.3, compactness_abundance_0.4,
                          compactness_abundance_0.5, compactness_abundance_0.6, compactness_abundance_0.7,
                          compactness_abundance_0.8, compactness_abundance_0.9)


compactness_range = c(compactness_range_0.1, compactness_range_0.2, compactness_range_0.3, compactness_range_0.4,
                      compactness_range_0.5, compactness_range_0.6, compactness_range_0.7,
                      compactness_range_0.8, compactness_range_0.9)



png(here::here("outputs/prioritisation/plot_reserve_compactness.png"), width = 960, height = 960)
#grey backgroundp
r(bg="white")
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
u <- par("usr") # The coordinates of the plot area
rect(-0.874, -0.97, 0.875, 100, col="grey90", border=NA)
par(mar = c(8,8,3,6), new = TRUE)
plot(targets, compactness_abundance,
     type = "p",
     las = 2,
     col = "yellow", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 100), ylim = c(min(compactness_range, compactness_abundance), max(compactness_range, compactness_abundance)))

axis(2, cex.axis=2.3, at = seq(0, 0.05, 0.005), las = 1, pos = 0)
title(xlab = "% Protected abundance or range", cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.3, las=1, at = seq(0,100,10), pos = 0)
title(ylab = "Compactness index", cex.lab = 3, line = 6)
points(targets, compactness_range, col = "brown", pch = 16, cex = 3)
legend(50, 0.0415, legend=c("Prioritization on abundance", "Prioritization on area"),
       col=c("yellow", "brown"), pch = c(16, 16), cex=2)
dev.off()
