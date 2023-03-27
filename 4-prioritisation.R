# tutoriel https://cran.r-project.org/web/packages/prioritizr/vignettes/prioritizr.html
# https://cran.r-project.org/web/packages/prioritizr/vignettes/saltspring.html

#gurobi installation
#manual download following instructions https://www.gurobi.com/academia/academic-program-and-licenses/ THEN
#install.packages('C:/gurobi911/win64/R/gurobi_9.1-1.zip', repos=NULL)
#install.packages('slam')


#load all functions
devtools::load_all()





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



######################################## load rasters ######################################## 

#read NC land
shp = rgdal::readOGR(here::here("data", "raw_data", "nc_land", "NOUVELLE_CALEDONIE.shp"))
raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia

# regional prediction raster interp surveyed
pred_region = raster::raster(here::here("data", "processed_data", "predictions", "predictions_region_interp_surv.grd"))

# distance to land
dist_land = raster::raster(here::here("data", "processed_data", "predictors", "dist_land_no_mask.grd"))

# mpa (no take only poe / ile verte)
mpa = raster::raster(here::here("data", "processed_data", "predictors", "mpa_type_no_take_only_poe_ile_verte.grd"))
mpa[mpa == -1] <- 0


#proportion of densities covered by no take mpas 
pred_region_mpa = raster::mask(pred_region, mpa, maskvalue = 0)
raster::cellStats(pred_region_mpa, 'sum') #61 indiv
raster::cellStats(pred_region_mpa, 'sum') / raster::cellStats(pred_region, 'sum') #6%


######################################## preprocessing ######################################## 

#set cost to 1 in study area
rast_xy[] <- 1 

#mask with land
rast_xy <- mask_with_land(rast_xy, dist_land)





######################################## prioritisation ######################################## 


### minimum set objective, no penalty ***** varying target ***** 

for (t in seq(0.1, 0.9, 0.1)){

  r <- design_reserves_minset_no_penalty(rast_xy, pred_region, t)
  
  #save map
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, ".png"))), width = 800, height = 480)
  raster::plot(r, main = paste("protecting", t*100, "% of density hotspots"), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
}



### minimum set objective, with penalty ***** target 0.5 ***** 

# Loop on BLM values (ratio perimeter / surface controlling compatcness)

# round 1 -----------------------
blmval = c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)
t <- 0.5

for (p in blmval){
  
  print("-----------------blm----------------")
  print(p)
  r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)

  #reformat p to avoid scientifc writing in objects
  p2 = format(p, scientific = FALSE)
  
  #calculate vector of patch surfaces
  assign(paste0("patch_surf_", p2), calc_reserve_patch_surfaces(r))
  
  #calculate mean of patch surfaces
  assign(paste0("mean_surf_", p2), mean(get(paste0("patch_surf_", p2))))
  
  #save map
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p2, "_target_", t*100, ".png"))), width = 800, height = 480)
  raster::plot(r, main = paste("protecting", t*100, "% of density hotspots - blm", p2), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
}

#blm fails at 1000, removing that value
blmval = blmval[1:9]
  
#vector of patch surfaces
mean_surface = c(mean_surf_0, mean_surf_0.00001, mean_surf_0.0001, mean_surf_0.001, mean_surf_0.01, mean_surf_0.1,
              mean_surf_1, mean_surf_10, mean_surf_100)



# plot BLM vs mean pach surface
png(here::here(paste0("outputs/prioritisation/plot_blm_vs_patch_surf_round1.png")), width = 960, height = 960)
par(mar = c(8,8,3,6))
plot(mean_surface,
     las = 2,
     col = "darkorange", pch = 16, cex = 2,
     axes = FALSE, xlab = "", ylab = "", ylim=c(0,max(mean_surface)))
axis(1, cex.axis=2, at = seq(1,9,1), labels= blmval, las =2)
title(ylab = expression(paste("Mean reserve patch surface (", "km"^"2", ")")), cex.lab = 2, line = 5.5)
axis(2, cex.axis=2, las=1)
title(xlab = "BLM values", cex.lab = 2, line = 5)
dev.off()

# Domisch et al 2018 appproach : identified the elbow, that is, the point from where an increase
# in the compactness (higher BLM values) has no major effect on the
# connectivity in spatial plans anymore. -> here between blm = 0.1 and 0.01




# round 2 -----------------------
blmval = seq(0.01, 0.1, 0.01)
t <- 0.5

for (p in blmval){
  
  print("-----------------blm----------------")
  print(p)
  r <- design_reserves_minset_with_penalty(rast_xy, pred_region, t, p)
  
  #reformat p to avoid scientifc writing in objects
  p2 = format(p, scientific = FALSE)
  
  #calculate vector of patch surfaces
  assign(paste0("patch_surf_", p2), calc_reserve_patch_surfaces(r))
  
  #calculate mean of patch surfaces
  assign(paste0("mean_surf_", p2), mean(get(paste0("patch_surf_", p2))))
  
  #save map
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p2, "_target_", t*100, ".png"))), width = 800, height = 480)
  raster::plot(r, main = paste("protecting", t*100, "% of density hotspots - blm", p2), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
}

#vector of patch surfaces
mean_surface = c(mean_surf_0.01, mean_surf_0.02, mean_surf_0.03, mean_surf_0.04, mean_surf_0.05, mean_surf_0.06,
                 mean_surf_0.07, mean_surf_0.08, mean_surf_0.09, mean_surf_0.1)



# plot BLM vs mean pach surface
png(here::here(paste0("outputs/prioritisation/plot_blm_vs_patch_surf_round2.png")), width = 960, height = 960)
par(mar = c(8,8,3,6))
plot(mean_surface,
     las = 2,
     col = "darkorange", pch = 16, cex = 2,
     axes = FALSE, xlab = "", ylab = "", ylim=c(0,max(mean_surface)))
axis(1, cex.axis=2, at = seq(1,10,1), labels= blmval, las =2)
title(ylab = expression(paste("Mean reserve patch surface (", "km"^"2", ")")), cex.lab = 2, line = 5.5)
axis(2, cex.axis=2, las=1)
title(xlab = "BLM values", cex.lab = 2, line = 5)
dev.off()

# Domisch et al 2018 appproach : identified the elbow, that is, the point from where an increase
# in the compactness (higher BLM values) has no major effect on the
# connectivity in spatial plans anymore. -> here blm = 0.03 corresponding to mean reserve patch 42 km2



### minimum set objective, with penalty and locked in constraint on mpas ***** target 0.5 ***** blm 0.03***** 

t <- 0.5
p <- 0.03

r <- design_reserves_minset_with_penalty_lockin(rast_xy, pred_region, t, p, mpa)

#save map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, "_lockin.png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of density hotspots - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#save map osm
map_reserves_minset_with_penalty_target_lockin_osm(maplatlon_proj, r, p, t)





### minimum set objective, with penalty and locked in constraint on mpas ***** target 0.8 ***** blm 0.03***** 

t <- 0.8
p <- 0.03

r <- design_reserves_minset_with_penalty_lockin(rast_xy, pred_region, t, p, mpa)

#save map
png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, "_lockin.png"))), width = 800, height = 480)
raster::plot(r, main = paste("protecting", t*100, "% of density hotspots - blm", p), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
dev.off()

#save map osm
map_reserves_minset_with_penalty_target_lockin_osm(maplatlon_proj, r, p, t)

#overlay choosen reserve prioritisation with predicted abundance
map_abundance_predictions_region_interp_surv_choosen_prioritisation(maplatlon_proj, pred_region, r)




#cost not al to zeros ?
