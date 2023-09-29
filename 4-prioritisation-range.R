
######################################## PRIORITISATION ON RANGE ######################################## 


#convert predicted abundance to range
range <- pred_region
range[range > 0] <- 1


###--------------------------------- minimum set objective, no penalty, lock in **** varying target ***** ----------------------------

for (t in seq(0.1, 0.9, 0.1)){
  
  print('********************* target *********************')
  print(t)
  
  r <- design_reserves_minset_no_penalty_lockin_randomize(rast_xy, range, t, mpa) #output is a portofolio of 10 solutions
  
  #map all 10 solutions
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_lockin_randomize_range.png"))), width = 800, height = 480)
  raster::plot(r, main = paste("protecting", t*100, "% of range"), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
  #map solution 1 of 10 solutions
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_lockin_randomize_range_sol1.png"))), width = 800, height = 480)
  raster::plot(r[[1]], main = paste("protecting", t*100, "% of range"), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
  #map reserves with map osm overlaid with range (solution 1 of 10 solutions)
  map_reserves_minset_no_penalty_target_lockin_osm_range(maplatlon_proj, range, r[[1]], mpas_notake_shp, t)
  
  # write raster stack (10 solutions and solution 1 of 10)
  raster::writeRaster(r, here::here("data", "processed_data", "prioritisation", paste0("prioritisation_range_", t*100)), overwrite=TRUE)
  raster::writeRaster(r[[1]], here::here("data", "processed_data", "prioritisation", paste0("prioritisation_range_sol1_", t*100)), overwrite=TRUE)
  
  #mask prediction with reserve to get percentage of abundance protected
  pred_region_mask <- raster::mask(pred_region, r, maskvalue = 1, inverse = T)
  
  #get percentage of protected abundnace per solution
  
  percent_protected <- NA
  
  for (i in 1:10){
    
     percent_protected[i] <- sum(raster::values(pred_region_mask[[i]]), na.rm = T) /  sum(raster::values(pred_region), na.rm = T)
     
  }
 
  print('********************* percent of abundance protected *********************')
  print(percent_protected)
  assign(paste0("percent_protected_range_", t), percent_protected)

  #get surface in prioritization per solution
  
  surf <- NA  
  
  for (i in 1:10){
    
    surf[i] <- raster::cellStats(r[[i]], "sum") * 0.25
  
  }
  
  print('********************* surf *********************')
  print(surf)
  assign(paste0("reserve_surf_range_", t), surf) #compute the mean as the surface of all solutions is the same
  
}



###--------------------------------- plot reserve surface vs target ----------------------------


reserve_surf_range <- c(reserve_surf_range_0.1, reserve_surf_range_0.2, reserve_surf_range_0.3, reserve_surf_range_0.4, reserve_surf_range_0.5,
                        reserve_surf_range_0.6, reserve_surf_range_0.7, reserve_surf_range_0.8, reserve_surf_range_0.9)

percent_protected_range <- c(percent_protected_range_0.1, percent_protected_range_0.2, percent_protected_range_0.3, percent_protected_range_0.4, percent_protected_range_0.5,
                             percent_protected_range_0.6, percent_protected_range_0.7, percent_protected_range_0.8, percent_protected_range_0.9)


targets <- seq(10, 90, 10)

png(here::here("outputs/prioritisation/plot_reserve_surf_abundance_range_vs_target.png"), width = 960, height = 960)
par(mar = c(8,8,3,6))
plot(reserve_surf_range, percent_protected_range*100,
     type = "p",
     las = 2,
     col = "brown", pch = 16, cex = 3, lwd =2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 800), ylim = c(5, 95))
axis(2, cex.axis=2.2, at = seq(10,90,10), labels= paste0(targets, "%"), las = 1)
title(xlab = expression(paste("Reserve surface (", "km"^"2", ")")), cex.lab = 3, line = 4.5)
axis(1, cex.axis=2.2, las=1)
title(ylab = "% Abundance protected", cex.lab = 3, line = 5)
points(reserve_surf_abundance, targets, col = "dark blue", pch = 16, cex = 3)
legend(1, 95, legend=c("Prior. on abundance", "Prior. on range"),
       col=c("dark blue", "brown"), pch = 16, cex=3)
box()
dev.off()


mean(reserve_surf_range/reserve_surf)




######################################## PRIORITISATION ON RANDOM RASTER ######################################## 

set.seed(1)
plot(range)
samp <- sampleRandom(range, 119, xy = TRUE, sp = TRUE, na.rm = TRUE) #119 individus
points(samp)

random <- raster::rasterize(samp, range)$layer



###--------------------------------- minimum set objective, no penalty, lock in **** varying target ***** ----------------------------

for (t in seq(0.1, 0.9, 0.1)){
  
  print('********************* target *********************')
  print(t)
  
  r <- design_reserves_minset_no_penalty_lockin_randomize(rast_xy, random, t, mpa) #output is a portofolio of 10 solutions
  
  #map
  png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_lockin_randomize_random.png"))), width = 800, height = 480)
  raster::plot(r, main = paste("protecting", t*100, "% of range"), col = c("light grey", "#39568CFF"), legend=FALSE)
  raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
  dev.off()
  
  #mask prediction with reserve to get percentage of abundance protected
  pred_region_mask <- raster::mask(pred_region, r, maskvalue = 1, inverse = T)
  
  #get percentage of protected abundnace per solution
  
  percent_protected <- NA
  
  for (i in 1:10){
    
    percent_protected[i] <- sum(raster::values(pred_region_mask[[i]]), na.rm = T) /  sum(raster::values(pred_region), na.rm = T)
    
  }
  
  print('********************* percent of abundance protected *********************')
  print(percent_protected)
  assign(paste0("percent_protected_random_", t), percent_protected)
  
  #get surface in prioritization per solution
  
  surf <- NA  
  
  for (i in 1:10){
    
    surf[i] <- raster::cellStats(r[[i]], "sum") * 0.25
    
  }
  
  print('********************* surf *********************')
  print(surf)
  assign(paste0("reserve_surf_random_", t), surf) #compute the mean as the surface of all solutions is the same
  
}


###--------------------------------- plot reserve surface vs target ----------------------------


reserve_surf_random <- c(reserve_surf_random_0.1, reserve_surf_random_0.2, reserve_surf_random_0.3, reserve_surf_random_0.4, reserve_surf_random_0.5,
                        reserve_surf_random_0.6, reserve_surf_random_0.7, reserve_surf_random_0.8, reserve_surf_random_0.9)

percent_protected_random <- c(percent_protected_random_0.1, percent_protected_random_0.2, percent_protected_random_0.3, percent_protected_random_0.4, percent_protected_random_0.5,
                             percent_protected_random_0.6, percent_protected_random_0.7, percent_protected_random_0.8, percent_protected_random_0.9)


targets <- seq(10, 90, 10)
plot(reserve_surf_random, percent_protected_random*100, xlim = c(0,800), ylim = c(0,100), col = "red")
points(reserve_surf_abundance, targets)

mean(reserve_surf_random/reserve_surf)





####### create disagregated raster

disagreg <- rast_xy
line1 <- rep(0.00001, raster::ncol(disagreg))
line2 <- c(rep(c(0.00001,1), (raster::ncol(disagreg)/2)), 0.00001)
disagreg[] <- rep(c(line1, line2), floor(raster::nrow(disagreg)/2))
disagreg <- raster::mask(disagreg, range)
raster::plot(disagreg)

r <- design_reserves_minset_no_penalty_lockin_randomize(rast_xy, disagreg, 0.6, mpa) #output is a portofolio of 10 solutions
raster::plot(r, main = paste("protecting", t*100, "% of range"), col = c("light grey", "#39568CFF"), legend=FALSE)
raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")



########### spatial aggregation index (for categorical data only)

# observed presence absence converted from observed abundance
pres_abs <- abundance_dugong
pres_abs[pres_abs >= 1] <- 1
pres_abs[pres_abs < 1] <- 0
landscapemetrics::lsm_l_ai(pres_abs) #59.7
landscapemetrics::lsm_c_ai(pres_abs) #0: 60.5 / 1: 5 -> donc faible aggregation des presences

# predicted presence absence converted from observed abundance
pres_abs <- pred_region
pres_abs[pres_abs >= 1] <- 1
pres_abs[pres_abs < 1] <- 0
landscapemetrics::lsm_l_ai(pres_abs) #92.7
landscapemetrics::lsm_c_ai(pres_abs) #0: 92.9 / 1:0

#range (uniform)
landscapemetrics::lsm_l_ai(range) #93.1
landscapemetrics::lsm_c_ai(range) #93.1

#random
landscapemetrics::lsm_l_ai(random) #2.78
landscapemetrics::lsm_c_ai(random) #2.78

#disagreggated
landscapemetrics::lsm_l_ai(disagreg) #62.2
landscapemetrics::lsm_c_ai(disagreg) #0: 62.2 / 1: 0
