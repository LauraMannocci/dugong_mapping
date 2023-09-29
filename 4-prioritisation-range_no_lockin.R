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
pred_region_mpa[pred_region_mpa > 0] <- 1
raster::cellStats(pred_region_mpa, "sum") #nb of cells in mpa 236



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




######################################## PRIORITISATION ON RANGE ######################################## 


#convert predicted abundance to range
range <- pred_region
range[range > 0] <- 1


###--------------------------------- minimum set objective, no penalty, lock in **** varying target ***** ----------------------------

set.seed(1)

for (t in seq(0.1, 0.9, 0.1)){
  
  print('********************* target *********************')
  print(t)
  
  r <- design_reserves_minset_no_penalty_randomize(rast_xy, range, t) #output is a portofolio of 10 solutions
  
  #extract first solution
  assign(paste0("prioritisation_range_", t), r[[1]]) 
  
  #map all 10 solutions
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_randomize_range.png"))), width = 800, height = 480)
  raster::plot(r, main = paste("protecting", t*100, "% of range"), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
  #map solution 1 of 10 solutions
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




###--------------------------------- multimap 30 50 80 % ----------------------------


map_reserves_minset_no_penalty_target_osm_range_new_multi(maplatlonproj, prioritisation_range_0.3, prioritisation_range_0.5, prioritisation_range_0.8, mpa) #run outside function
  

###--------------------------------- multimap 10-90 % ----------------------------


png(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_range_multi.png"))
par(mar = c(0,0,1.5,0), mfrow = c(3,3))
#10%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "10% range")
raster::plot(prioritisation_range_0.1, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#20%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "20% range")
raster::plot(prioritisation_range_0.2, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#30%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "30% range")
raster::plot(prioritisation_range_0.3, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#40%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "40% range")
raster::plot(prioritisation_range_0.4, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#50%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "50% range")
raster::plot(prioritisation_range_0.5, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#60%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "60% range")
raster::plot(prioritisation_range_0.6, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#70%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "70% range")
raster::plot(prioritisation_range_0.7, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#80%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "80% range")
raster::plot(prioritisation_range_0.8, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
#90%
raster::plot(new_caledonia_proj, border = "transparent", col = "grey", xlim = c(275000, 380000), ylim = c(255000, 330000), main = "90% range")
raster::plot(prioritisation_range_0.9, legend = FALSE, add = TRUE, col = c("grey92", "brown"))
dev.off()




###--------------------------------- plot reserve surface vs target ----------------------------


reserve_surf_range_no_lockin <- c(reserve_surf_range_0.1, reserve_surf_range_0.2, reserve_surf_range_0.3, reserve_surf_range_0.4, reserve_surf_range_0.5,
                                  reserve_surf_range_0.6, reserve_surf_range_0.7, reserve_surf_range_0.8, reserve_surf_range_0.9)

percent_protected_range_no_lockin <- c(percent_protected_range_0.1, percent_protected_range_0.2, percent_protected_range_0.3, percent_protected_range_0.4, percent_protected_range_0.5,
                                      percent_protected_range_0.6, percent_protected_range_0.7, percent_protected_range_0.8, percent_protected_range_0.9)


targets <- seq(10, 90, 10)

png(here::here("outputs/prioritisation/plot_reserve_surf_abundance_range_vs_target_no_lockin.png"), width = 960, height = 960)
par(mar = c(8,8,3,6))
plot(reserve_surf_range_no_lockin, percent_protected_range_no_lockin*100,
     type = "p",
     las = 2,
     col = "brown", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 800), ylim = c(5, 95))
axis(2, cex.axis=2.2, at = seq(10,90,10), labels= paste0(targets, "%"), las = 1)
title(xlab = expression(paste("Reserve surface (", "km"^"2", ")")), cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.2, las=1)
title(ylab = "% Abundance protected", cex.lab = 3, line = 5)
points(reserve_surf_abundance_no_lockin, targets, col = "dark blue", pch = 16, cex = 3)
legend(1, 95, legend=c("Prior. on abundance", "Prior. on range"),
       col=c("dark blue", "brown"), pch = 16, cex=2)
box()
dev.off()


mean(unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin)
unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin




###--------------------------------- plot reserve surface vs target no penalty ----------------------------


png(here::here("outputs/prioritisation/plot_reserve_surf_abundance_range_vs_target_no_lockin_no_penalty.png"), width = 960, height = 960)
#grey backgroundp
r(bg="white")
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
u <- par("usr") # The coordinates of the plot area
rect(-0.935, -0.97, 0.9, 1.1, col="grey90", border=NA)
par(mar = c(8,8,3,6), new = TRUE)
plot(reserve_surf_range_no_lockin, percent_protected_range_no_lockin*100,
     type = "p",
     las = 2,
     col = "brown", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 800), ylim = c(0, 100))

axis(2, cex.axis=2.2, at = seq(0,100,10), labels= paste0( seq(0, 100, 10), "%"), las = 1)
title(xlab = expression(paste("Reserve surface (", "km"^"2", ")")), cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.2, las=1)
title(ylab = "% Abundance protected", cex.lab = 3, line = 6)
points(reserve_surf_abundance_no_lockin_no_penalty, targets, col = "yellow", pch = 16, cex = 3)
#add abundance protected in existing reserve surface  
points(70, 9.4, col = "orange", pch = 17, cex = 3)
legend(1, 98, legend=c("Prior. on abundance", "Prior. on range"),
       col=c("yellow", "brown"), pch = 16, cex=2)
dev.off()


mean(unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin_no_penalty)
unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin_no_penalty





###--------------------------------- plot nb reserve cells vs target no penalty ----------------------------


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
title(ylab = "% Abundance protected", cex.lab = 3, line = 6)
points(reserve_surf_abundance_no_lockin_no_penalty/0.25, targets, col = "yellow", pch = 16, cex = 3)
#add abundance protected in existing reserve surface  
points(236, 9.4, col = "orange", pch = 17, cex = 3)
legend(1, 98, legend=c("Prior. on abundance", "Prior. on range"),
       col=c("yellow", "brown"), pch = 16, cex=2)
dev.off()


mean(unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin_no_penalty)
unique(reserve_surf_range_no_lockin)/reserve_surf_abundance_no_lockin_no_penalty


mean(percent_protected_range_no_lockin[41:50])




###--------------------------------- plot % area protected vs target no penalty ----------------------------

#number of cells in study area
nbcells = raster::freq(rast_xy, value=1) # 3330

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
title(xlab = "% Protected cells", cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.3, las=1, at = seq(0,100,10), pos = 0)
title(ylab = "% Protected individuals", cex.lab = 3, line = 6)
points(reserve_percent_abundance_no_lockin_no_penalty, targets, col = "yellow", pch = 16, cex = 3)
#add abundance protected in existing reserve surface  
points(100*(236/nbcells), 9.4, col = "orange", pch = 17, cex = 3)
legend(50, 16, legend=c("Prioritization on abundance", "Prioritization on range", "Existing no-take MPAs"),
       col=c("yellow", "brown", "orange"), pch = c(16, 16, 17), cex=2)
dev.off()

targets/reserve_percent_abundance_no_lockin_no_penalty

########### spatial aggregation index (for categorical data only)

# abundance prioritisation
landscapemetrics::lsm_l_ai(prioritisation_abundance_0.5) #85.8
landscapemetrics::lsm_c_ai(prioritisation_abundance_0.5) #0: 89.5 / 1: 35.9 

# range prioritisation
landscapemetrics::lsm_l_ai(prioritisation_range_0.5) #49.2
landscapemetrics::lsm_c_ai(prioritisation_range_0.5) #0: 49.2 / 1:46.2


