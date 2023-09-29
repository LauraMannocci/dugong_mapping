
#' fit full abudance GAM with area surveyed as offset and 
#' unlimited k
#'
#' @param distrib 
#'
#' @return
#' @export
#'

fit_full_abundance_model_old <- function(distrib, data_model){
  
  if (distrib == "tw"){
    mod <- gam(abundance ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                s(turbidity)  + s(dist_reef) + s(depth) + s(dist_seagrass) +
                s(dist_pass) + s(dist_barrier_reef) + s(dist_intermediate_reef) +
                offset(log(area_surveyed_m2)), 
                family = tw(), data = data_model, method="REML")}
  
  if (distrib == "nb"){
    mod <- gam(abundance ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                 s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                 s(turbidity)  + s(dist_reef) + s(depth) + s(dist_seagrass) +
                 s(dist_pass) + s(dist_barrier_reef) + s(dist_intermediate_reef) +
                 offset(log(area_surveyed_m2)), 
               family = nb(), data = data_model, method="REML")}
  
  if (distrib == "zip"){
    mod <- gam(abundance_round ~ s(rubble) + s(coral_algae) + s(microalgal_mats) + s(seagrass) + s(sand) + s(rock) + s(back_reef_slope) + 
                 s(deep_lagoon) + s(inner_reef_flat) + s(outer_reef_flat) + s(plateau) + s(reef_crest) + s(reef_slope) + s(shallow_lagoon) + 
                 s(turbidity) + s(dist_reef) + s(depth) + s(dist_seagrass) +
                 s(dist_pass) + s(dist_barrier_reef) + s(dist_intermediate_reef) +
                 offset(log(area_surveyed_m2)), 
               family = ziP(), data = data_model, method="REML")}
  
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
#' unlimited k
#'
#' @param distrib 
#'
#' @return
#' @export
#'

fit_full_abundance_model <- function(distrib, data_model){
  
  if (distrib == "tw"){
    mod <- gam(abundance ~ s(dist_seagrass) + 
                 s(dist_pass) + s(dist_all_reef) + 
                 s(deep_lagoon_coverage) + s(reefflat_coverage) + s(depth) + 
                 s(seagrass_coverage) + 
                 offset(log(area_surveyed_m2)), 
               family = tw(), data = data_model, method="REML")}
  
  if (distrib == "nb"){
    mod <- gam(abundance ~ s(dist_seagrass) + 
                 s(dist_pass) + s(dist_all_reef) + 
                 s(deep_lagoon_coverage) + s(reefflat_coverage) + s(depth) + 
                 s(seagrass_coverage) + 
                 offset(log(area_surveyed_m2)), 
               family = nb(), data = data_model, method="REML")}
  
  if (distrib == "zip"){
    mod <- gam(abundance_round ~ s(dist_seagrass) + 
                 s(dist_pass) + s(dist_all_reef) + 
                 s(deep_lagoon_coverage) + s(reefflat_coverage) + s(depth) + 
                 s(seagrass_coverage) + 
                 offset(log(area_surveyed_m2)), 
               family = ziP(), data = data_model, method="REML")}
  
  #summary
  sink(here::here(paste0("outputs/models/summary_full_", distrib, ".txt")))
  print(summary(mod))
  sink()
  
  #terms plot
  png(here::here(paste0("outputs/models/terms_plot_full_", distrib, ".png")))
  plot(mod, pages = 1, scale = 0) 
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



#' make customized term for the best model with distance to pass, seagrss and depth as predictors
#'
#' @param model 
#'
#' @return
#' @export
#'

make_customized_term_plot <- function(model){
  
  #Only distance to pass plot   
  dist_pass_plot <- gratia::draw(model, 
                                 select = "dist_pass", partial_match = T,
                                 ci_alpha = 0.5,
                                 ci_col = "lightblue",
                                 nrow = 1,
                                 residuals = F) & 
    ggplot2::theme_minimal() &
    ggplot2::theme(axis.text=ggplot2::element_text(size=14),
                   axis.title=ggplot2::element_text(size=18,face="bold")) &
    ggplot2::labs(y = "Log(Abundance)",x = "Distance to pass (m)",title = " ")
  
  #Only seagrass plot
  seagrass_plot <- gratia::draw(model, 
                                select = "seagrass_coverage", partial_match = T,
                                ci_alpha = 0.5,
                                ci_col = "lightblue",
                                nrow = 1,
                                residuals = F) & 
    ggplot2::theme_minimal() &
    ggplot2::theme(axis.text=ggplot2::element_text(size=14),
                   axis.title=ggplot2::element_text(size=18,face="bold")) &
    ggplot2::labs(y = "Log(Abundance)",x = "Seagrass coverage (%)",title = " ")
  
  #Only depth plot
  depth_plot <- gratia::draw(model, 
                             select = "depth", partial_match = T,
                             ci_alpha = 0.5,
                             ci_col = "lightblue",
                             nrow = 1,
                             residuals = F) & 
    ggplot2::theme_minimal() &
    ggplot2::theme(axis.text=ggplot2::element_text(size=14),
                   axis.title=ggplot2::element_text(size=18,face="bold")) &
    ggplot2::labs(y = "Log(Abundance)",x = "Depth (m)",title = " ")
  
  
  #Only depth plot zoomed
  depth_plot2 <- gratia::draw(model, 
                              select = "depth", partial_match = T,
                              ci_alpha = 0.5,
                              ci_col = "lightblue",
                              nrow = 1,
                              residuals = F) & 
    ggplot2::theme_minimal() &
    ggplot2::xlim(0, 50) &
    ggplot2::ylim(-5, 5) &
    ggplot2::theme(axis.text=ggplot2::element_text(size=14),
                   axis.title=ggplot2::element_text(size=18,face="bold")) &
    ggplot2::labs(y = "Log(Abundance)",x = "Depth (m) - zoomed panel",title = " ")
  
  #Draw all together
  plot = dist_pass_plot + seagrass_plot + depth_plot + depth_plot2 + patchwork::plot_layout(ncol = 2,nrow =2)
  
  
  ggplot2::ggsave(here::here("outputs/models/gam_check_reduced_nb_customized.png"), plot, width = 300, height = 200, unit = "mm", dpi = 600)
  
}






#' fit full abudance GAM with area surveyed as offset and 
#' with k limited to 5
#'
#' @param distrib 
#'
#' @return
#' @export
#'

fit_full_abundance_model_k5 <- function(distrib, data_model){
  
  
  if (distrib == "tw"){
    mod <- gam(abundance ~ s(dist_seagrass, k = 5) + 
                 s(dist_pass, k = 5) + s(dist_all_reef, k = 5) + 
                 s(deep_lagoon, k = 5) + s(reefflat, k = 5) + s(depth, k = 5) + 
                 s(seagrass, k = 5) + 
                 offset(log(area_surveyed_m2)), 
               family = tw(), data = data_model, method="REML")}
  
  if (distrib == "nb"){
    mod <- gam(abundance ~ s(dist_seagrass, k = 5) + 
                 s(dist_pass, k = 5) + s(dist_all_reef, k = 5) + 
                 s(deep_lagoon, k = 5) + s(reefflat, k = 5) + s(depth, k = 5) + 
                 s(seagrass, k = 5) + 
                 offset(log(area_surveyed_m2)), 
               family = nb(), data = data_model, method="REML")}
  
  if (distrib == "zip"){
    mod <- gam(abundance_round ~ s(dist_seagrass, k = 5) + 
                 s(dist_pass, k = 5) + s(dist_all_reef, k = 5) + 
                 s(deep_lagoon, k = 5) + s(reefflat, k = 5) + s(depth, k = 5) + 
                 s(seagrass, k = 5) + 
                 offset(log(area_surveyed_m2)), 
               family = ziP(), data = data_model, method="REML")}
  
  #summary
  sink(here::here(paste0("outputs/models/summary_full_", distrib, "_k5.txt")))
  print(summary(mod))
  sink()
  
  #terms plot
  png(here::here(paste0("outputs/models/terms_plot_full_", distrib, "_k5.png")))
  plot(mod, pages = 1, scale = 0) 
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
                                 name = "Individuals")
  
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




#' Map abundance predictions, st errors and coef variations in region in univariate interpolation situations with mpas
#'
#' @param pred 
#' @param maplatlonproj
#' @param pred_se 
#'
#' @return
#' @export
#'

map_abundance_predictions_region_interp_mpas <- function(maplatlonproj, pred, pred_se, pa){
  
  #prepare mpas data
  pa =  sp::spTransform(pa, "+init=epsg:3163") #NC projection
  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")
  
  
  ####predictions
  
  #conversting to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #mpas
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "ÃŽle Verte"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "Roche PercÃ©e et de la Baie des Tortues"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "PoÃ©"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id =="Ouano"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "Individuals")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_mean_region_interp_mpas.png"), map, width = 7, height = 5)
  
  ####se
  
  #conversting to data frame
  pred_se2 = pred_se %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_se2, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #mpas
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "ÃŽle Verte"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "Roche PercÃ©e et de la Baie des Tortues"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "PoÃ©"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id =="Ouano"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "ind / 0.25 km2")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_se_region_interp_mpas.png"), map, width = 7, height = 5)
  
  
  ####cv
  
  pred_cv = 100* (pred_se / pred)
  
  #conversting to data frame
  pred_cv = pred_cv %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred_cv, ggplot2::aes(x = x, y = y, fill = layer), alpha = 0.8) +
    #mpas
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "ÃŽle Verte"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "Roche PercÃ©e et de la Baie des Tortues"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "PoÃ©"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id =="Ouano"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient(low = "dark grey", high = "red", na.value = NA,
                                 name = "%")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_cv_region_interp_mpas.png"), map, width = 7, height = 5)
  
  
  
}


#' Map abundance predictions new
#'
#' @param pred 
#' @param maplatlonproj
#'
#' @return
#' @export
#'

map_abundance_predictions_region_interp_new <- function(maplatlonproj, pred){
  
  ####predictions
  
  #converting to data frame
  pred2 = pred %>% 
    raster::as.data.frame(xy = T)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_tile(data = pred2, ggplot2::aes(x = x, y = y, fill = layer)) +
    #axes set manually
    ggplot2::scale_y_continuous(breaks = c(332000,309000,288000,266000), labels = c("-21.2", "-21.4", "-21.6", "-21.8"), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = c(295000,330000,360000), labels = c("165.0", "165.3", "165.6"), expand = c(0, 0)) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    #log transformed color scale
    ggplot2::scale_fill_stepsn(colours = c("#feebe2",  "#980043"),
                               breaks = c(0.001,0.01,0.1,1,5), na.value = NA, 
                               name = "Abundance", trans = "log")
  
  ggplot2::ggsave(here::here("outputs/models/map_abundance_mean_region_interp_new.png"), map, width = 7, height = 5)
  
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
  
  ####mean
  
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




#' read surveyed block (created in qgis)
#'
#' @return
#' @export
#'

read_surveyed_block <- function(){
  
  s = sf::read_sf(here::here("data", "raw_data", "surveyed_bloc", "surveyed_bloc.shp"))
  
  return(s)
  
}



#' read and project surveyed block (created in qgis)
#'
#' @return
#' @export
#'

read_project_surveyed_block <- function(){
  
  s = sf::read_sf(here::here("data", "raw_data", "surveyed_bloc", "surveyed_bloc.shp"))
  
  s = sf::st_transform(s, 3163) #NC projection
  
  return(s)
  
}


#' read and project surveyed strata (created in qgis)
#'
#' @param name 
#'
#' @return
#' @export
#'

read_project_surveyed_strata <- function(name){
  
  s = sf::read_sf(here::here("data", "raw_data", "surveyed_strata", name))
  
  s = sf::st_transform(s, 3163) #NC projection
  
  return(s)
  
}






#' Histogram of predicted abundances
#'
#' @param name 
#'
#' @return
#' @export
#'

hist_predicted_abundance <- function(rast){
  
  png(here::here("outputs/models/hist_predicted_abundance.png"))
  hist(rast, main = "", xlab = "Individuals")
  dev.off()
  
}



