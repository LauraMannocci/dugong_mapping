

#' Design reserve based on predicted densities with minimum set objective and given target with no penalty
#'
#' @param region 
#' @param pred 
#' @param target 
#'
#' @return
#' @export
#'

design_reserves_minset_no_penalty <- function(region, pred, target){
  
  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred)
  
  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
  
  # print number of planning units
  # prioritizr::number_of_planning_units(p)
  
  # print number of features
  # prioritizr::number_of_features(p)
  
  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)
  
  # Calculate how well feature representation targets are met by a solution to a conservation planning problem().
  print("--------------feature representation----------------")
  print(prioritizr::eval_target_coverage_summary(p, s))

  return(s)
}






#' Design reserve based on predicted densities with minimum set objective and given target with locked in constraint on mpas
#'
#' @param region 
#' @param pred 
#' @param target 
#' @param mpas 
#'
#' @return
#' @export
#'

design_reserves_minset_no_penalty_lockin <- function(region, pred, target, mpas){
  
  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred)
  
  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>%  # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_locked_in_constraints(mpas) #lock in constraint to ensure that reserves are selected
  
  # print number of planning units
  # prioritizr::number_of_planning_units(p)
  
  # print number of features
  # prioritizr::number_of_features(p)
  
  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)
  
  # Calculate how well feature representation targets are met by a solution to a conservation planning problem().
  print("--------------feature representation----------------")
  print(prioritizr::eval_target_coverage_summary(p, s))
  
  return(s)
}






#' Design reserve based on predicted densities with minimum set objective and given target with penalty
#'
#' @param region 
#' @param pred 
#' @param target 
#' @param penalty 
#'
#' @return
#' @export
#'

design_reserves_minset_with_penalty <- function(region, pred, target, penalty){

  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred)
  
  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>%  # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_boundary_penalties(penalty = penalty) # boundary penalty (to favor solutions that have planning units clumped together into contiguous areas)

  # print number of planning units
  # prioritizr::number_of_planning_units(p)
  
  # print number of features
  # prioritizr::number_of_features(p)
  
  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)
  
  # Calculate how well feature representation targets are met by a solution to a conservation planning problem().
  print("--------------feature representation----------------")
  print(prioritizr::eval_target_coverage_summary(p, s))

  return(s)
}






#' Design reserve based on predicted densities with minimum set objective and given target with locked in constraint on mpas 
#' with randomisation of uniform feature raster (corresponding to species range) - this returns a portofolio of solutions
#'
#' @param region 
#' @param pred 
#' @param target 
#' @param mpas 

design_reserves_minset_no_penalty_lockin_randomize <- function(region, pred, target, mpas){

  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred)
  
  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>%  # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_locked_in_constraints(mpas) %>% #lock in constraint to ensure that reserves are selected
    prioritizr::add_shuffle_portfolio(11, remove_duplicates = TRUE)  #randomly reordering the data prior to solving the problem (returns of portofolio of 10 solutions)
  
  # print number of planning units
  # prioritizr::number_of_planning_units(p)
  
  # print number of features
  # prioritizr::number_of_features(p)
  
  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)
  
  #remove first solution (geographically biased) from the list of solutions
  s = s[2:11]
  
  #convert to stack
  stack = raster::stack(s)
  
  return(stack)
}





#' Design reserve based on predicted densities with minimum set objective and given target with locked in constraint on mpas 
#' with randomisation of uniform feature raster (corresponding to species range) - this returns a portofolio of solutions
#'
#' @param region 
#' @param pred 
#' @param target 
#' @param mpas 

design_reserves_minset_no_penalty_randomize <- function(region, pred, target){
  
  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred)
  
  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>%  # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_shuffle_portfolio(11, remove_duplicates = TRUE)  #randomly reordering the data prior to solving the problem (returns of portofolio of 10 solutions)
  
  # print number of planning units
  # prioritizr::number_of_planning_units(p)
  
  # print number of features
  # prioritizr::number_of_features(p)
  
  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)
  
  #remove first solution (geographically biased) from the list of solutions
  s = s[2:11]
  
  #convert to stack
  stack = raster::stack(s)
  
  return(stack)
}





#' Design reserve based on predicted densities with minimum set objective and given target with locked in constraint on mpas 
#' with randomisation of uniform feature raster (corresponding to species range) - this returns a portofolio of solutions
#'
#' @param region 
#' @param pred 
#' @param target 
#' @param mpas 

design_reserves_minset_with_penalty_randomize <- function(region, pred, target, penalty){
  
  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred)
  
  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>%  # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_shuffle_portfolio(11, remove_duplicates = TRUE) %>%  #randomly reordering the data prior to solving the problem (returns of portofolio of 10 solutions)
    prioritizr::add_boundary_penalties(penalty = penalty) # boundary penalty (to favor solutions that have planning units clumped together into contiguous areas)
   
  
  # print number of planning units
  # prioritizr::number_of_planning_units(p)
  
  # print number of features
  # prioritizr::number_of_features(p)
  
  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)
  
  #remove first solution (geographically biased) from the list of solutions
  s = s[2:11]
  
  #convert to stack
  stack = raster::stack(s)
  
  return(stack)
}




#' Calculate reserve patch surfaces in km2
#'
#' @param reserve
#'
#' @return
#' @export
#'

calc_reserve_patch_surfaces <- function(reserves){
  
  #replace 0s (no reserve) with NAs
  reserves[reserves==0] <- NA
  
  #landscapemetrics::check_landscape(reserves)
  
  #calculate patch surface
  s = landscapemetrics::lsm_p_area(reserves) #in Hectares
  surfaces_km2 = s$value * 0.01 #in km2
  
  return(surfaces_km2)
}





#' Design reserve based on predicted densities with minimum set objective and given target with penalty and locked in constraint on mpas
#'
#' @param region 
#' @param pred 
#' @param target 
#' @param penalty 
#' @param mpas 
#'
#' @return
#' @export
#'

design_reserves_minset_with_penalty_lockin <- function(region, pred, target, penalty, mpas){
  
  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred)
  
  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>%  # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_boundary_penalties(penalty = penalty) %>% # boundary penalty (to favor solutions that have planning units clumped together into contiguous areas)
    prioritizr::add_locked_in_constraints(mpas) #lock in constraint to ensure that reserves are selected
  
  # print number of planning units
  # prioritizr::number_of_planning_units(p)
  
  # print number of features
  # prioritizr::number_of_features(p)
  
  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)
  
  # Calculate how well feature representation targets are met by a solution to a conservation planning problem().
  print("--------------feature representation----------------")
  print(prioritizr::eval_target_coverage_summary(p, s))
  
  return(s)
}







#' Map reserves with osm map
#'
#' @param pred 
#' @param maplatlonproj
#' @param pred_se 
#'
#' @return
#' @export
#'

map_reserves_minset_with_penalty_target_lockin_osm <- function(maplatlonproj, r, p, t){
  
  #converting to data frame
  r2 = r %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon_proj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = subset(r2, r2$layer == 1), ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::guides(fill="none") +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "orange")
  
  
  ggplot2::ggsave(here::here(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p, "_target_", t*100, "_lockin_osm.png")), map, width = 7, height = 5)
  
  
}





#' Map reserves with osm map and overlay of abundance predictions 
#' 
#' @param pred 
#' @param r 
#' @param maplatlonproj
#' @param mpa 
#' @param p 
#' @param t 
#'
#' @return
#' @export
#'

map_reserves_minset_with_penalty_target_lockin_osm_abundance <- function(maplatlonproj, pred, r, mpa, p, t){
  
  ####predictions
  
  #conversting predictions to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  #convert reserve prioritization and mpa to polygon
  r[r==0] <- NA
  pol = raster::rasterToPolygons(r)
  
  #project mpas
  mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.9) +
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), col = "cyan", alpha = 0) + 
    #axes set manually
    ggplot2::scale_y_continuous(breaks = c(332000,309000,288000,266000), labels = c("-21.2", "-21.4", "-21.6", "-21.8"), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = c(295000,330000,360000), labels = c("165.0", "165.3", "165.6"), expand = c(0, 0)) +
    #mpa polygon in orange
    ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "light grey", high = "red", na.value = NA,
                                 name = "Individuals")
  
  ggplot2::ggsave(here::here(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p,"_target_", t*100, "_lockin_osm_abundance.png")), map, width = 7, height = 5)
  
  
}





#' Map reserves with osm map and overlay of abundance predictions 
#' 
#' @param pred 
#' @param r 
#' @param maplatlonproj
#' @param mpa 
#' @param p 
#' @param t 
#'
#' @return
#' @export
#'

map_reserves_minset_with_penalty_target_osm_abundance <- function(maplatlonproj, pred, r, mpa, p, t){
  
  ####predictions
  
  #conversting predictions to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  #convert reserve prioritization and mpa to polygon
  r[r==0] <- NA
  pol = raster::rasterToPolygons(r)
  
  #project mpas
  mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.9) +
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), col = "cyan", alpha = 0) + 
    #axes set manually
    ggplot2::scale_y_continuous(breaks = c(332000,309000,288000,266000), labels = c("-21.2", "-21.4", "-21.6", "-21.8"), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = c(295000,330000,360000), labels = c("165.0", "165.3", "165.6"), expand = c(0, 0)) +
    #mpa polygon in orange
    ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "light grey", high = "red", na.value = NA,
                                 name = "Individuals")
  
  ggplot2::ggsave(here::here(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p,"_target_", t*100, "_osm_abundance.png")), map, width = 7, height = 5)
  
  
}





#' Map reserves with osm map and overlay of abundance predictions multimap
#'
#' @param maplatlonproj 
#' @param t 
#' @param r30 
#' @param r50 
#' @param r70 
#'
#' @return
#' @export
#'
#' @examples
#' 
map_reserves_minset_no_penalty_target_osm_abundance_new_multi <- function(maplatlonproj, r30, r50, r70, t){
  
  
  # 30 % ----------------------------
  
  ####predictions
  
  #convert reserve prioritization and mpa to polygon
  r30[r30==0] <- NA
  pol30 = raster::rasterToPolygons(r30)
  
  #project mpas
  # mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map30 = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol30, ggplot2::aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.8) + 
    #mpa polygon in orange
    # ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    # ggplot2::ggtitle(" ") +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                   legend.position = "none")
  
  # 50 % ----------------------------
  
  ####predictions
  
  #convert reserve prioritization and mpa to polygon
  r50[r50==0] <- NA
  pol50 = raster::rasterToPolygons(r50)
  
  #project mpas
  # mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map50 = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol50, ggplot2::aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.8) + 
    #mpa polygon in orange
    # ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    # ggplot2::ggtitle(" ") +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                   legend.position = "none")
  
  # 70 % ----------------------------
  
  ####predictions
  
  #convert reserve prioritization and mpa to polygon
  r70[r70==0] <- NA
  pol70 = raster::rasterToPolygons(r70)
  
  #project mpas
  # mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map70 = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol70, ggplot2::aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.8) + 
    #mpa polygon in orange
    # ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    # ggplot2::ggtitle(" ") +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                   legend.position = "none")
  
  # multiplot
  plot = cowplot::ggdraw() +
    cowplot::draw_plot(map70, 0, 0, 0.98, 0.32) +
    cowplot::draw_plot(map50, 0, 0.33, 0.98, 0.32) +
    cowplot::draw_plot(map30, 0, 0.66, 0.98, 0.32) +
    cowplot::draw_text("a", x = 0.05, y = 0.96, size = 15) +
    cowplot::draw_text("b", x = 0.05, y = 0.63, size = 15) +
    cowplot::draw_text("c", x = 0.05, y = 0.3, size = 15)
  
  cowplot::save_plot(here::here("outputs/prioritisation/map_reserves_minset_no_penalty_target_osm_abundance_new_multi.png"), plot, base_width = 5, base_height = 10)

  
  
}





#' Map reserves with osm map and overlay of abundance predictions 
#'
#' @param maplatlonproj 
#' @param r 
#' @param mpa 
#' @param t 
#'
#' @return
#' @export
#'
#' @examples
#' 
map_reserves_minset_no_penalty_target_osm_abundance_new <- function(maplatlonproj, r, mpa, t){
  
  ####predictions
  
  #conversting predictions to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  #convert reserve prioritization and mpa to polygon
  r[r==0] <- NA
  pol = raster::rasterToPolygons(r)
  
  #project mpas
  # mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.8) + 
    #axes set manually
    ggplot2::scale_y_continuous(breaks = c(332000,309000,288000,266000), labels = c("-21.2", "-21.4", "-21.6", "-21.8"), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = c(295000,330000,360000), labels = c("165.0", "165.3", "165.6"), expand = c(0, 0)) +
    #mpa polygon in orange
    # ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5))
  
  ggplot2::ggsave(here::here(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_osm_abundance_new.png")), map, width = 7, height = 5)
  
  
}

#' Map reserves with osm map and overlay of abundance predictions 
#' 
#' @param pred 
#' @param r 
#' @param maplatlonproj
#' @param mpa 
#' @param p 
#' @param t 
#'
#' @return
#' @export
#'

map_reserves_minset_no_penalty_target_osm_abundance <- function(maplatlonproj, pred, r, mpa, t){
  
  ####predictions
  
  #conversting predictions to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  #convert reserve prioritization and mpa to polygon
  r[r==0] <- NA
  pol = raster::rasterToPolygons(r)
  
  #project mpas
  mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.9) +
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), col = "cyan", alpha = 0) + 
    #axes set manually
    ggplot2::scale_y_continuous(breaks = c(332000,309000,288000,266000), labels = c("-21.2", "-21.4", "-21.6", "-21.8"), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = c(295000,330000,360000), labels = c("165.0", "165.3", "165.6"), expand = c(0, 0)) +
    #mpa polygon in orange
    ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "light grey", high = "red", na.value = NA,
                                 name = "Individuals")
  
  ggplot2::ggsave(here::here(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_osm_abundance.png")), map, width = 7, height = 5)
  
  
}



#' Map reserves with osm map and overlay of range  
#' 
#' @param pred 
#' @param r 
#' @param maplatlonproj
#' @param mpa 
#' @param p 
#' @param t 
#'
#' @return
#' @export
#'

map_reserves_minset_no_penalty_target_lockin_osm_range <- function(maplatlonproj, range, r, mpa, t){

  ####predictions
  
  #conversting predictions to data frame
  range2 = range %>% 
    raster::as.data.frame(xy = T)
  
  #convert reserve prioritization and mpa to polygon
  r[r==0] <- NA
  pol = raster::rasterToPolygons(r)
  
  #project mpas
  mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = range2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.9) +
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), col = "cyan", alpha = 0) + 
    #axes set manually
    ggplot2::scale_y_continuous(breaks = c(332000,309000,288000,266000), labels = c("-21.2", "-21.4", "-21.6", "-21.8"), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = c(295000,330000,360000), labels = c("165.0", "165.3", "165.6"), expand = c(0, 0)) +
    #mpa polygon in orange
    ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "none") +
    ggplot2::scale_fill_gradient(low = "light grey", high = "light grey", na.value = NA,
                                 name = "Individuals")
  
  ggplot2::ggsave(here::here(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_lockin_osm_range.png")), map, width = 7, height = 5)
  
  
}



#' Map reserves with osm map and overlay of range  
#' 
#' @param pred 
#' @param r 
#' @param maplatlonproj
#' @param mpa 
#' @param p 
#' @param t 
#'
#' @return
#' @export
#'

map_reserves_minset_no_penalty_target_osm_range <- function(maplatlonproj, range, r, mpa, t){
  
  ####predictions
  
  #conversting predictions to data frame
  range2 = range %>% 
    raster::as.data.frame(xy = T)
  
  #convert reserve prioritization and mpa to polygon
  r[r==0] <- NA
  pol = raster::rasterToPolygons(r)
  
  #project mpas
  mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = range2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.9) +
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), col = "cyan", alpha = 0) + 
    #axes set manually
    ggplot2::scale_y_continuous(breaks = c(332000,309000,288000,266000), labels = c("-21.2", "-21.4", "-21.6", "-21.8"), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = c(295000,330000,360000), labels = c("165.0", "165.3", "165.6"), expand = c(0, 0)) +
    #mpa polygon in orange
    ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "none") +
    ggplot2::scale_fill_gradient(low = "light grey", high = "light grey", na.value = NA,
                                 name = "Individuals")
  
  ggplot2::ggsave(here::here(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_osm_range.png")), map, width = 7, height = 5)
  
  
}



#' Map reserves with osm map and overlay of range  
#' 
#' @param pred 
#' @param r 
#' @param maplatlonproj
#' @param mpa 
#' @param p 
#' @param t 
#'
#' @return
#' @export
#'

map_reserves_minset_no_penalty_target_osm_range_new <- function(maplatlonproj, r, mpa, t){
  
  ####predictions
  
  #convert reserve prioritization and mpa to polygon
  r[r==0] <- NA
  pol = raster::rasterToPolygons(r)
  
  #project mpas
  # mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), fill = "brown", alpha = 0.8) + 
    #axes set manually
    ggplot2::scale_y_continuous(breaks = c(332000,309000,288000,266000), labels = c("-21.2", "-21.4", "-21.6", "-21.8"), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = c(295000,330000,360000), labels = c("165.0", "165.3", "165.6"), expand = c(0, 0)) +
    #mpa polygon in orange
    # ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "none") 
  
  ggplot2::ggsave(here::here(paste0("outputs/prioritisation/map_reserves_minset_no_penalty_target_", t*100, "_osm_range_new.png")), map, width = 7, height = 5)
  
  
}



#' Map reserves with osm map and overlay of range mutimap
#' 
#' @param maplatlonproj
#' @param r30 
#' @param r50 
#' @param r70 
#' @param mpa 
#'
#' @return
#' @export
#'

map_reserves_minset_no_penalty_target_osm_range_new_multi <- function(maplatlonproj, r30, r50, r70, mpa){
  
  # 30 % ----------------------------
  
  ####predictions
  
  #convert reserve prioritization and mpa to polygon
  r30[r30==0] <- NA
  pol30 = raster::rasterToPolygons(r30)
  
  #project mpas
  # mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map30 = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol30, ggplot2::aes(x = long, y = lat, group = group), fill = "brown", alpha = 0.8) + 
    #mpa polygon in orange
    # ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    # ggplot2::ggtitle(" ") +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                   legend.position = "none")
  
  # 50 % ----------------------------
  
  ####predictions
  
  #convert reserve prioritization and mpa to polygon
  r50[r50==0] <- NA
  pol50 = raster::rasterToPolygons(r50)
  
  #project mpas
  # mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map50 = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol50, ggplot2::aes(x = long, y = lat, group = group), fill = "brown", alpha = 0.8) + 
    #mpa polygon in orange
    # ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    # ggplot2::ggtitle(" ") +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                   legend.position = "none")
  
  # 70 % ----------------------------
  
  ####predictions
  
  #convert reserve prioritization and mpa to polygon
  r70[r70==0] <- NA
  pol70 = raster::rasterToPolygons(r70)
  
  #project mpas
  # mpa_proj = sp::spTransform(mpa, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  map70 = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    #overlay prioritisation in cyan
    ggplot2::geom_polygon(data = pol70, ggplot2::aes(x = long, y = lat, group = group), fill = "brown", alpha = 0.8) + 
    #mpa polygon in orange
    # ggplot2::geom_polygon(data = mpa_proj, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + 
    #ggplot2::theme_minimal() +
    # ggplot2::ggtitle(" ") +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                   legend.position = "none")

  # multiplot
  plot = cowplot::ggdraw() +
    cowplot::draw_plot(map70, 0, 0, 0.98, 0.32) +
    cowplot::draw_plot(map50, 0, 0.33, 0.98, 0.32) +
    cowplot::draw_plot(map30, 0, 0.66, 0.98, 0.32) +
    cowplot::draw_text("d", x = 0.05, y = 0.96, size = 15) +
    cowplot::draw_text("e", x = 0.05, y = 0.63, size = 15) +
    cowplot::draw_text("f", x = 0.05, y = 0.3, size = 15)
  
  cowplot::save_plot(here::here("outputs/prioritisation/map_reserves_minset_no_penalty_target_osm_range_new_multi.png"), plot, base_width = 5, base_height = 10)
  
  
  
  
}





#' Calibrate blm value for given target (minimum set objective, with penalty, lock in) round 1 (BLM from 0 to 100)
#'
#' @param t 
#'
#' @return
#' @export
#'

calibrate_blm_round1 <- function(pred, t){
  
  # BLM Calibration
  # Domisch et al 2018 appproach : identified the elbow, that is, the point from where an increase
  # in the compactness (higher BLM values) has no major effect on the
  # connectivity in spatial plans anymore. -> here between blm = 0.1 and 0.01
  
  ########### round 1 
  blmval = c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100)
  
  for (p in blmval){
    
    print("-----------------blm calibration round 1----------------")
    print(p)
    r <- design_reserves_minset_with_penalty(rast_xy, pred, t, p)
    
    #reformat p to avoid scientifc writing in objects
    p2 = format(p, scientific = FALSE)
    
    #calculate surface of reserve ie cost (nb of cells selected times surface of one cell ie 0.25 km2)
    assign(paste0("reserve_surf_", p2), raster::cellStats(r, "sum") * 0.25)
    
    #save map
    # png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p2, "_lockin_target_", t*100, ".png"))), width = 800, height = 480)
    # raster::plot(r, main = paste("protecting", t*100, "% of density hotspots - blm", p2), col = c("light grey", "#39568CFF"), legend=FALSE)
    # raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
    # dev.off()
    
  }
  
  #vector of patch surfaces
  reserve_surface = c(reserve_surf_0, reserve_surf_0.00001, reserve_surf_0.0001, reserve_surf_0.001, reserve_surf_0.01, reserve_surf_0.1,
                      reserve_surf_1, reserve_surf_10, reserve_surf_100)
  
  # plot BLM vs reserve surface
  png(here::here(paste0("outputs/prioritisation/plot_blm_vs_reserve_surf_round1_target_", t*100, ".png")), width = 960, height = 960)
  par(mar = c(8,8,3,6))
  plot(reserve_surface,
       las = 2,
       col = "darkorange", pch = 16, cex = 2,
       axes = FALSE, xlab = "", ylab = "", ylim=c(0,max(reserve_surface)))
  axis(1, cex.axis=2, at = seq(1,9,1), labels= blmval, las =2)
  title(ylab = expression(paste("Reserve surface (", "km"^"2", ")")), cex.lab = 2, line = 5.5)
  axis(2, cex.axis=2, las=1)
  title(xlab = "Boundary length modifier", cex.lab = 2, line = 5)
  dev.off()
  
}



#' Calibrate blm value for given target (minimum set objective, with penalty, lock in) round 2 
#'
#' @param t 
#'
#' @return
#' @export
#'

calibrate_blm_round2 <- function(pred, t, blmval){
  
  # BLM Calibration
  # Domisch et al 2018 appproach : identified the elbow, that is, the point from where an increase
  # in the compactness (higher BLM values) has no major effect on the
  # connectivity in spatial plans anymore. -> here between blm = 0.1 and 0.01
  
  ###########  round 2 
  
  for (p in blmval){
    
    print("-----------------blm calibration round 2----------------")
    print(p)
    r <- design_reserves_minset_with_penalty(rast_xy, pred, t, p)
    
    #reformat p to avoid scientifc writing in objects
    p2 = format(p, scientific = FALSE)
    
    #calculate surface of reserve ie cost (nb of cells selected times surface of one cell ie 0.25 km2)
    assign(paste0("reserve_surf_", which(blmval == p)), raster::cellStats(r, "sum") * 0.25)
    
    #save map
    # png(here::here(paste0(paste0("outputs/prioritisation/map_reserves_minset_with_penalty_", p2, "_lockin_target_", t*100, ".png"))), width = 800, height = 480)
    # raster::plot(r, main = paste("protecting", t*100, "% of density hotspots - blm", p2), col = c("light grey", "#39568CFF"), legend=FALSE)
    # raster::plot(surv_block, add = TRUE, border = "black", col = "transparent")
    # dev.off()
    
  }
  
  #vector of patch surfaces
  reserve_surface = c(reserve_surf_1, reserve_surf_2, reserve_surf_3, reserve_surf_4, reserve_surf_5, reserve_surf_6,
                      reserve_surf_7, reserve_surf_8, reserve_surf_9, reserve_surf_10)
  
  
  # plot BLM vs reserve surface
  png(here::here(paste0("outputs/prioritisation/plot_blm_vs_reserve_surf_round2_target_", t*100, ".png")), width = 960, height = 960)
  par(mar = c(8,8,3,6))
  plot(reserve_surface,
       las = 2,
       col = "darkorange", pch = 16, cex = 2,
       axes = FALSE, xlab = "", ylab = "", ylim=c(0,max(reserve_surface)))
  axis(1, cex.axis=2, at = seq(1,10,1), labels= blmval, las =2)
  title(ylab = expression(paste("Reserve surface (", "km"^"2", ")")), cex.lab = 2, line = 5.5)
  axis(2, cex.axis=2, las=1)
  title(xlab = "Boundary length modifier", cex.lab = 2, line = 5)
  dev.off()
  
  
}







#' calculate reserve patch surface in km2
#'
#' @param reserves 
#'
#' @return
#' @export
#'
#' @examples
calc_reserve_patch_surfaces <- function(reserves){
  
  #project
  r = reserves
  
  #replace 0s (no reserve) with NAs
  r[r==0] <- NA
  
  #calculate patch surface
  s = landscapemetrics::lsm_p_area(r) #area in Hectares
  s_km2 = s$value /100 #convertion to km 
  
  return(s_km2)
}



