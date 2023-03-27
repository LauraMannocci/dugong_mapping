
#' fit full abudance GAM with area surveyed as offset and 
#' automated term selection thanks to select = T (https://stats.stackexchange.com/questions/405129/model-selection-for-gam-in-r)
#' unlimited k
#'
#' @param distrib 
#'
#' @return
#' @export
#'

fit_full_abundance_model <- function(distrib, data_model){
  
  if (distrib == "tw"){
    mod <- gam(abundance ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass) +
                offset(log(area_surveyed_m2)), 
                family = tw(), data = data_model, method="REML", select = TRUE)}
  
  if (distrib == "nb"){
    mod <- gam(abundance ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                 s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                 s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass) +
                 offset(log(area_surveyed_m2)), 
               family = nb(), data = data_model, method="REML", select = TRUE)}
  
  if (distrib == "zip"){
    mod <- gam(abundance_round ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                 s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                 s(turbidity) + s(travel_time) + s(dist_reef) + s(depth) + s(dist_seagrass) +
                 offset(log(area_surveyed_m2)), 
               family = ziP(), data = data_model, method="REML", select = TRUE)}
  
  #summary
  sink(here::here(paste0("outputs/models/summary_full_", distrib, ".txt")))
  print(summary(mod))
  sink()
  
  #terms plot
  png(here::here(paste0("outputs/models/terms_plot_full_", distrib, ".png")))
  plot(mod, pages = 1) 
  dev.off()
  
  #gam check
  png(here::here(paste0("outputs/models/gam_check_full_", distrib, ".png")))
  qq.gam(mod, type="deviance")
  dev.off()
  
  #diagnostics plot
  png(here::here(paste0("outputs/models/diagnostics_plot_full_", distrib, ".png")))
  par(mfrow=c(2,2))
  gam.check(mod) 
  dev.off()
  
  #autocorrelation plot
  png(here::here(paste0("outputs/models/autocorrelogram_plot_full_", distrib, ".png")))
  acf(mod$residuals)
  dev.off()
  
  return(mod)
}





#' fit full abudance GAM with area surveyed as offset and 
#' automated term selection thanks to select = T (https://stats.stackexchange.com/questions/405129/model-selection-for-gam-in-r)
#' with k limited to 5
#'
#' @param distrib 
#'
#' @return
#' @export
#'

fit_full_abundance_model_k5 <- function(distrib, data_model){
  
  if (distrib == "tw"){
    mod <- gam(abundance ~ s(rubble, k = 5) + s(coral_algae, k = 5) + s(microalgal_mats, k = 5) + s(seagrass, k = 5) + s(sand, k = 5) + s(rock, k = 5) + s(back_reef_slope, k = 5) + 
                 s(deep_lagoon, k = 5) + s(inner_reef_flat, k = 5) + s(outer_reef_flat, k = 5) + s(plateau, k = 5) + s(reef_crest, k = 5) + s(reef_slope, k = 5) + s(shallow_lagoon, k = 5) + 
                 s(turbidity, k = 5) + s(travel_time, k = 5) + s(dist_reef, k = 5) + s(depth, k = 5) + s(dist_seagrass, k = 5) +
                 offset(log(area_surveyed_m2)), 
               family = tw(), data = data_model, method="REML", select = TRUE)}
  
  if (distrib == "nb"){
    mod <- gam(abundance ~ s(rubble, k = 5) + s(coral_algae, k = 5) + s(microalgal_mats, k = 5) + s(seagrass, k = 5) + s(sand, k = 5) + s(rock, k = 5) + s(back_reef_slope, k = 5) + 
                 s(deep_lagoon, k = 5) + s(inner_reef_flat, k = 5) + s(outer_reef_flat, k = 5) + s(plateau, k = 5) + s(reef_crest, k = 5) + s(reef_slope, k = 5) + s(shallow_lagoon, k = 5) + 
                 s(turbidity, k = 5) + s(travel_time, k = 5) + s(dist_reef, k = 5) + s(depth, k = 5) + s(dist_seagrass, k = 5) +
                 offset(log(area_surveyed_m2)), 
               family = nb(), data = data_model, method="REML", select = TRUE)}
  
  if (distrib == "zip"){
    mod <- gam(abundance_round ~ s(rubble, k = 5) + s(coral_algae, k = 5) + s(microalgal_mats, k = 5) + s(seagrass, k = 5) + s(sand, k = 5) + s(rock, k = 5) + s(back_reef_slope, k = 5) + 
                 s(deep_lagoon, k = 5) + s(inner_reef_flat, k = 5) + s(outer_reef_flat, k = 5) + s(plateau, k = 5) + s(reef_crest, k = 5) + s(reef_slope, k = 5) + s(shallow_lagoon, k = 5) + 
                 s(turbidity, k = 5) + s(travel_time, k = 5) + s(dist_reef, k = 5) + s(depth, k = 5) + s(dist_seagrass, k = 5) +
                 offset(log(area_surveyed_m2)), 
               family = ziP(), data = data_model, method="REML", select = TRUE)}
  
  #summary
  sink(here::here(paste0("outputs/models/summary_full_", distrib, "_k5.txt")))
  print(summary(mod))
  sink()
  
  #terms plot
  png(here::here(paste0("outputs/models/terms_plot_full_", distrib, "_k5.png")))
  plot(mod, pages = 1) 
  dev.off()
  
  #diagnostics plot
  png(here::here(paste0("outputs/models/diagnostics_plot_full_", distrib, "_k5.png")))
  par(mfrow=c(2,2))
  gam.check(mod) 
  dev.off()
  
  #autocorrelation plot
  png(here::here(paste0("outputs/models/autocorrelogram_plot_full_", distrib, "_k5.png")))
  acf(mod$residuals)
  dev.off()
  
  return(mod)
}





#' Map abundance predictions, st errors and coef variations in region
#'
#' @param pred 
#' @param maplatlonproj
#' @param pred_se 
#'
#' @return
#' @export
#'

map_abundance_predictions_region <- function(maplatlonproj, pred, pred_se){
  
  ####predictions
  
  #conversting to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_mean_region.png"), map, width = 7, height = 5)
  
  ####se
  
  #conversting to data frame
  pred_se2 = pred_se %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_se2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_se_region.png"), map, width = 7, height = 5)
  
  
  ####cv
  
  pred_cv = 100* (pred_se / pred)
  
  #conversting to data frame
  pred_cv = pred_cv %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_cv, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "%")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_cv_region.png"), map, width = 7, height = 5)
  
  
  
}








#' Map abundance predictions, st errors and coef variations in surveyed area
#'
#' @param pred 
#' @param maplatlonproj
#'
#' @return
#' @export
#'

map_abundance_predictions_surveyed <- function(maplatlonproj, pred, pred_se){
  
  
  ####predictions
  
  #conversting to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_mean_surveyed.png"), map, width = 7, height = 5)
  
  
  
  ####se
  
  #conversting to data frame
  pred_se2 = pred_se %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_se2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_se_surveyed.png"), map, width = 7, height = 5)
  
  
  
  ####cv
  
  pred_cv = 100* (pred_se / pred)
  
  #conversting to data frame
  pred_cv = pred_cv %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_cv, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "%")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_cv_surveyed.png"), map, width = 7, height = 5)
  
}






#' Map abundance predictions, st errors and coef variations in region in univariate interpolation situations 
#'
#' @param pred 
#' @param maplatlonproj
#' @param pred_se 
#'
#' @return
#' @export
#'

map_abundance_predictions_region_interp <- function(maplatlonproj, pred, pred_se){
  
  ####predictions
  
  #conversting to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_mean_region_interp.png"), map, width = 7, height = 5)
  
  ####se
  
  #conversting to data frame
  pred_se2 = pred_se %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_se2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_se_region_interp.png"), map, width = 7, height = 5)
  
  
  ####cv
  
  pred_cv = 100* (pred_se / pred)
  
  #conversting to data frame
  pred_cv = pred_cv %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_cv, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "%")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_cv_region_interp.png"), map, width = 7, height = 5)
  
  
  
}





#' Map abundance predictions, st errors and coef variations in region in univariate interpolation situations constrained to surveyed block
#'
#' @param pred 
#' @param maplatlonproj
#' @param pred_se 
#'
#' @return
#' @export
#'

map_abundance_predictions_region_interp_surv <- function(maplatlonproj, pred, pred_se){
  
  ####predictions
  
  #conversting to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_mean_region_interp_surv.png"), map, width = 7, height = 5)
  
  ####se
  
  #conversting to data frame
  pred_se2 = pred_se %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_se2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_se_region_interp_surv.png"), map, width = 7, height = 5)
  
  
  ####cv
  
  pred_cv = 100* (pred_se / pred)
  
  #conversting to data frame
  pred_cv = pred_cv %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_cv, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "%")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_cv_region_interp_surv.png"), map, width = 7, height = 5)
  
  
  
}


#' Get polygon of surveyed area
#'
#' @param r 
#'
#' @return
#' @export
#'

get_polygon_surveyed_area <- function(r){
  
  #get extent
  e <- raster::extent(r)
  
  # make all raster values the same. 
  r <- x > -Inf
  
  # convert raster to polygons (you need to have package 'rgeos' installed for this to work)
  p <- raster::rasterToPolygons(r, dissolve=TRUE)
  
  # create buffer around polygons
  b = raster::buffer(p, width = 400, dissolve = TRUE) #width in meters
  
  # ideaklly buffer should not be on land side - some test below
  # ref to look up https://andrewpwheeler.com/2015/10/08/one-sided-line-buffers-in-r-using-rgeos/
  # t = sf::st_buffer(sf::st_as_sf(p), dist = -50, singleSide = TRUE)
  
  # look at the results
  raster::plot(r)
  raster::plot(p, lwd=5, border='red', add=TRUE)
  raster::plot(b, border = "green", add = TRUE)
  
  return(b)
  
}






#' read and project surveyed bloc (created in qgis)
#'
#' @return
#' @export
#'

read_project_surveyed_block <- function(){
  
  s = sf::read_sf(here::here("data", "raw_data", "surveyed_bloc", "surveyed_bloc.shp"))
  
  s = sf::st_transform(s, 3163) #NC projection
  
  return(s)
  
}








