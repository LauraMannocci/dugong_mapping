

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
  
  #replace nas with very small value to avoid error
  pred[is.na(pred)] <- 0.00001
  
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
  
  #replace nas with very small value to avoid error
  pred[is.na(pred)] <- 0.00001
  
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
  
  #replace nas with very small value to avoid error
  pred[is.na(pred)] <- 0.00001
  
  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred)
  
  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>%  # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_boundary_penalties(penalty = penalty) %>% # boundary penalty (to favor solutions that have planning units clumped together into contiguous areas)
    prioritizr::add_locked_in_constraints(mpas) #lock out constraint to ensure that fish reserves are not selected
  
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





#' Map abundance predictions in region in univariate interpolation situations constrained to surveyed block
#' with chsen prioritisation overlaid
#' 
#' @param pred 
#' @param r 
#' @param maplatlonproj
#'
#' @return
#' @export
#'

map_abundance_predictions_region_interp_surv_choosen_prioritisation <- function(maplatlonproj, pred, r){
  
  ####predictions
  
  #conversting predictions to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  #converting reserves to polygon
  r[r==0] <- NA
  pol = raster::rasterToPolygons(r, dissolve=TRUE)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #overlay prioritisation
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), col = "orange", alpha = 0) + #mpa polygon in yellow
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.box.spacing = grid::unit(0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "Predicted dugongs")
  
  ggplot2::ggsave(here::here("outputs/prioritisation/map_abundance_predictions_region_interp_surv_choosen_prioritisation.png"), map, width = 7, height = 5)
  
  
}

