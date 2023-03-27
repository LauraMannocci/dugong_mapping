

#' Make open street map for study area (satellite view)
#'
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#'
#' @return
#' @export
#'

osm_map <- function(lat1, lon1, lat2, lon2){
  
  #Open OSM
  map = OpenStreetMap::openmap(c(lat2,lon1), c(lat1,lon2), zoom = NULL,
                               type = c("bing"), #for satellite view
                               mergeTiles = TRUE)
  maplatlon = OpenStreetMap::openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  return(maplatlon)
  
}



#' Make map of Allen coral habitat polygons overlaid on osm
#'
#' @param cor
#' @param maplatlon
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#'
#' @return
#' @export
#'

map_allen_coral_osm <- function(maplatlon, cor, lon1, lon2, lat1, lat2){
  
  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE
  
  # fortify
  cor3 = cor2 %>%
    ggplot2::fortify(region = "class")
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon)
  
  map = map +
    ggplot2::geom_polygon(data = cor3, ggplot2::aes(x = long, y = lat, group = group, fill = id), alpha = 0.8) +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 13),
                   legend.key.size = ggplot2::unit(1.15,"line"),
                   legend.position = "bottom",
                   axis.title.x =  ggplot2::element_blank(),
                   axis.title.y =  ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::labs(fill = "") +
    #colors
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor3$id)), c = 70, l = 50))) +
    ggplot2::labs(x = "Longitude", y = "Latitude")
  
  ggplot2::ggsave(here::here("outputs/predictors/map_allen_coral_osm.png"), map, width = 7, height = 5)
  
}



#' Make map of Allen coral geomorphology polygons overlaid on osm
#'
#' @param cor
#' @param maplatlon
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#'
#' @return
#' @export
#'

map_allen_geomorpho_osm <- function(maplatlon, cor, lon1, lon2, lat1, lat2){
  
  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE
  
  # fortify
  cor3 = cor2 %>%
    ggplot2::fortify(region = "class")
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon)
  
  map = map +
    ggplot2::geom_polygon(data = cor3, ggplot2::aes(x = long, y = lat, group = group, fill = id), alpha = 0.8) +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 9),
                   legend.key.size = ggplot2::unit(1.15,"line"),
                   legend.position = "bottom",
                   axis.title.x =  ggplot2::element_blank(),
                   axis.title.y =  ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::labs(fill = "") +
    #colors
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor3$id)), c = 70, l = 50))) +
    ggplot2::labs(x = "Longitude", y = "Latitude")
  
  ggplot2::ggsave(here::here("outputs/predictors/map_allen_geomorpho_osm.png"), map, width = 7, height = 5)
  
}


#' Make map of telemetry
#'
#' @param telem
#' @param maplatlon
#'
#' @return
#' @export
#'

map_telemetry <- function(maplatlon, telem){
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5, color = "white") +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank())
  
    ggplot2::ggsave(here::here("outputs/map_telemetry.png"), map, width = 7, height = 5)
  
}



#' Make map of telemetry with one color per date
#'
#' @param telem
#' @param maplatlon
#'
#' @return
#' @export
#'

map_telemetry_date <- function(maplatlon, telem){
  
  cols = c("2021-06-04" = "dark blue", "2021-06-05" = "light blue", "2021-06-16" = "orange", "2021-06-21" = "red", "2021-06-22" = "green", "2021-07-29" = "brown", "2021-08-04" = "purple")
    
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat, color = date), size = 0.01, alpha = 0.4) +
    ggplot2::scale_color_manual(values = cols) +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size=3))) + #increase dot size in legend
    ggplot2::theme(axis.title = ggplot2::element_blank())
  
  ggplot2::ggsave(here::here("outputs/map_telemetry_date.png"), map, width = 7, height = 5)
  
}



#' Make separate map of telemetry per date
#'
#' @param telem
#' @param maplatlon
#'
#' @return
#' @export
#'

map_telemetry_date_separate <- function(maplatlon, telem){
  
  
  cols = c("2021-06-04" = "dark blue", "2021-06-05" = "light blue", "2021-06-16" = "orange", "2021-06-21" = "red", "2021-06-22" = "green", "2021-07-29" = "brown", "2021-08-04" = "purple")
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat, color = date), size = 0.005, alpha = 0.5) +
    ggplot2::facet_wrap(~date, nrow = 2) + #facet per date
    ggplot2::scale_color_manual(values = cols) +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())
  
  ggplot2::ggsave(here::here("outputs/map_telemetry_date_separate.png"), map, width = 7, height = 5)
  
}




#' Make map of telemetry with individual species observations
#'
#' @param telem
#' @param maplatlon
#' @param telem_obs
#'
#' @return
#' @export
#'

map_indiv_species_telemetry <- function(maplatlon, telem, telem_obs){
  
  #select species observations and remove coral and plane
  telem_obs %>%
    dplyr::filter(!object %in% c("Coral", "Plane_shadow")) -> telem_obs
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5) + #telem only
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat, color = object), size = 0.1, alpha = 0.5) +
    ggplot2::guides(color = ggplot2::guide_legend("Species", override.aes = list(size=3))) + #change title and increase dot size in legend
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank())
  
  ggplot2::ggsave(here::here("outputs/map_indiv_species_telemetry.png"), map, width = 7, height = 5)
  
}







#' Make map of telemetry with individual dugong observations (1 dot = 1 obs centered on image so dots can be overlaid)
#'
#' @param telem
#' @param maplatlon
#' @param telem_obs
#'
#' @return
#' @export
#'

map_indiv_dugong_telemetry <- function(maplatlon, telem, telem_obs){
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5) + #telem only
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat, color = object), size = 0.2, alpha = 0.5) +
    ggplot2::ggtitle(paste0("Dugong_certain_probable - number = ", nrow(telem_obs))) +
    ggplot2::theme(legend.position="none") +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5))
  
  ggplot2::ggsave(here::here("outputs/map_indiv_dugong_telemetry.png"), map, width = 7, height = 5)
  
}












#' Make map of telemetry with individual dugong observations  (1 dot = 1 obs centered on images so dots can be overlaid)
#'
#' @param telem
#' @param maplatlon
#' @param telem_obs
#'
#' @return
#' @export
#'

map_indiv_dugong_stage_telemetry <- function(maplatlon, telem, telem_obs){
  
  labels <- c("adult" = paste0("adults number=", nrow(telem_obs[telem_obs$stage=="adult",])),
              "juvenile" = paste0("juveniles number=", nrow(telem_obs[telem_obs$stage=="juvenile",])))
                                       
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5) + #telem only
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat, color = object), size = 0.2, alpha = 0.5) +
    ggplot2::facet_wrap(~stage, nrow = 2, labeller = ggplot2::as_labeller(labels)) + #facet per stage
    ggplot2::theme(legend.position="none") +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5))
  
  ggplot2::ggsave(here::here("outputs/map_indiv_dugong_stage_telemetry.png"), map, width = 7, height = 5)
  
}



#' Make map of telemetry with individual dugong observations per image differenciating stage (adult vs juvenile)  
#'
#' @param telem
#' @param maplatlon
#' @param telem_obs
#'
#' @return
#' @export
#'

map_indiv_dugong_per_image_stage_telemetry <- function(maplatlon, telem, telem_obs){
  
  #calculate group size per image (uncorrected by avail bias because all rows summed)
  telem_obs %>% 
    dplyr::group_by(image_id, lat, lon, stage) %>% 
    dplyr::summarise(Dugongs = dplyr::n()) -> telem_obs
  
  labels <- c("adult" = "adults per image",
              "juvenile" = "juveniles per image")
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, color = "white", alpha = 0.01) + #telem only
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat, size = Dugongs), color = "pink", alpha = 0.7) +
    ggplot2::facet_wrap(~stage, nrow = 2, labeller = ggplot2::as_labeller(labels)) + #facet per stage
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::ylab("Latitude") +
    ggplot2::xlab("Longitude") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme_light()
  
  ggplot2::ggsave(here::here("outputs/map_indiv_dugong_per_image_stage_telemetry.png"), map, width = 7, height = 5)
  
}



#' Make map of telemetry with individual dugong observations per image 
#'
#' @param telem
#' @param maplatlon
#' @param telem_obs
#'
#' @return
#' @export
#'

map_indiv_dugong_per_image_telemetry <- function(maplatlon, telem, telem_obs){
  
  #calculate group size per image (uncorrected by avail bias because all rows summed)
  telem_obs %>% 
    dplyr::group_by(image_id, lat, lon, stage) %>% 
    dplyr::summarise(Dugongs = dplyr::n()) -> telem_obs
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, col = "white", alpha = 0.01) + #telem only
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat, size = Dugongs), color = "pink", alpha = 0.7) +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::ylab("Latitude") +
    ggplot2::xlab("Longitude") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme_light()
  
  ggplot2::ggsave(here::here("outputs/map_indiv_dugong_per_image_telemetry.png"), map, width = 7, height = 5)
  
}





#' Make map of telemetry with individual species observations with separate map per species
#'
#' @param telem
#' @param maplatlon
#' @param telem_obs
#'
#' @return
#' @export
#'

map_indiv_species_telemetry_separate <- function(maplatlon, telem, telem_obs){
  
  #select species observations and remove coral and plane
  telem_obs %>%
    dplyr::filter(!object %in% c("Coral", "Plane_shadow")) -> telem_obs
  
  #create dataset grouped by image and species to get number of images per species
  telem_obs %>%
    dplyr::group_by(image_id, object) %>%
    dplyr::summarise(n = dplyr::n()) -> telem_species_per_image
  
  #define species labels including for each species number of individuals and number of images
  species_labels <- c(
    "Turtle" = paste0("Turtle ind=", nrow(telem_obs[telem_obs$object=="Turtle",]),
                      " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Turtle"]))),
    "Dugong_certain_probable" = paste0("Dugong ind=", nrow(telem_obs[telem_obs$object=="Dugong_certain_probable",]),
                              " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Dugong_certain_probable"]))),
     "Shark" = paste0("Shark ind=", nrow(telem_obs[telem_obs$object=="Shark",]),
                     " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Shark"]))),
    "Round_ray" = paste0("Round_ray ind=", nrow(telem_obs[telem_obs$object=="Round_ray",]),
                         " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Round_ray"]))),
    "Eagle_ray" = paste0("Eagle_ray ind=", nrow(telem_obs[telem_obs$object=="Eagle_ray",]),
                         " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Eagle_ray"]))),
    "Humpback_whale" = paste0("Humpback_whale ind=", nrow(telem_obs[telem_obs$object=="Humpback_whale",]),
                              " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Humpback_whale"]))),
    "Dolphin" = paste0("Dolphin ind=", nrow(telem_obs[telem_obs$object=="Dolphin",]),
                       " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Dolphin"]))),
    "Sea_snake" = paste0("Sea_snake ind=", nrow(telem_obs[telem_obs$object=="Sea_snake",]),
                         " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Sea_snake"]))))
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat), size = 0.1, col = "red", alpha = 0.5) +
    ggplot2::facet_wrap(~object, nrow = 3, labeller = ggplot2::as_labeller(species_labels)) + #facet per species
    #ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5) + #telem only
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size=7)) #facet wrap title size

  ggplot2::ggsave(here::here("outputs/map_indiv_species_telemetry_separate.png"), map, width = 7, height = 5)

  
}





#' Make map of telemetry and transect lines
#'
#' @param telem
#' @param maplatlon
#' @param transects
#'
#' @return
#' @export
#'

map_telemetry_transects <- function(maplatlon, telem, transects){
  
  transects_fortify = ggplot2::fortify(transects)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_line(data = transects_fortify, ggplot2::aes(x = long, y = lat, group = id), col = "red", alpha = 0.5) + #transects in red
    ggplot2::geom_text(data = transects_fortify, ggplot2::aes(x = long, y = lat, label = id), hjust = 0, vjust = 0, size = 1, col = "red") + #add point transect numbers
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5, color = "white") +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  ggplot2::ggsave(here::here("outputs/map_telemetry_transects.png"), map, width = 7, height = 5)
  
}






#' Make map of selected transect lines
#'
#' @param telem
#' @param maplatlon
#' @param transects
#'
#' @return
#' @export
#'

map_transects <- function(maplatlon, transects){
  
  transects_fortify = ggplot2::fortify(transects)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_line(data = transects_fortify, ggplot2::aes(x = long, y = lat, group = id), col = "white") + 
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    #add scalebar
    ggsn::scalebar(data = NULL, dist = 5, transform = TRUE, model = "WGS84", dist_unit = "km", height = 0.3,
                   x.min = 164.2, x.max = 165, y.min = -21.84, y.max = -21.82,
                   st.dist = 1, st.color = "white", box.color = "white", border.size = 0.5, box.fill = c("white", "white"), st.size = 3.4) +
    #north arrow
    ggspatial::annotation_north_arrow(location = "bl", height = grid::unit(0.7, "cm"),  width = grid::unit(0.5, "cm"), 
                                      pad_y = grid::unit(1, "cm"), pad_x = grid::unit(1.2, "cm"), 
                                      style = ggspatial::north_arrow_orienteering(line_col = "white", text_col = "white")) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme_light()
  
  ggplot2::ggsave(here::here("outputs/map_transects.png"), map, width = 7, height = 5)
  
}




#' Make map of selected transect lines with mpas
#'
#' @param telem
#' @param maplatlon
#' @param transects
#'
#' @return
#' @export
#'

map_transects_mpas <- function(maplatlon, transects, pa){
  
  transects_fortify = ggplot2::fortify(transects)
  
  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")
    
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_line(data = transects_fortify, ggplot2::aes(x = long, y = lat, group = id), col = "white") + 
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "ÃŽle Verte"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "Roche PercÃ©e et de la Baie des Tortues"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "PoÃ©"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id =="Ouano"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "Parc de la Zone CÃ´tiÃ¨re Ouest"), ggplot2::aes(x = long, y = lat), col = "orange", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id =="Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    #add scalebar
    ggsn::scalebar(data = NULL, dist = 5, transform = TRUE, model = "WGS84", dist_unit = "km", height = 0.3,
                   x.min = 164.2, x.max = 165, y.min = -21.84, y.max = -21.82,
                   st.dist = 1, st.color = "white", box.color = "white", border.size = 0.5, box.fill = c("white", "white"), st.size = 3.4) +
    #north arrow
    ggspatial::annotation_north_arrow(location = "bl", height = grid::unit(0.7, "cm"),  width = grid::unit(0.5, "cm"), 
                                      pad_y = grid::unit(1, "cm"), pad_x = grid::unit(1.2, "cm"), 
                                      style = ggspatial::north_arrow_orienteering(line_col = "white", text_col = "white")) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme_light()
  
  ggplot2::ggsave(here::here("outputs/map_transects_mpas.png"), map, width = 7, height = 5)
  
}





#' Make New caledonia map with Poé location
#'
#' @return
#' @export
#'

map_new_caledonia_big_mpas <- function(mpas, transects){
  
  # Use raster::getData("ISO3") to see codes
  new_caledonia <- raster::getData("GADM", country = "NCL", level = 1)

  png(here::here("outputs", "map_new_caledonia_big_mpas.png"), width = 960, height = 960)
  plot(subset(mpas, mpas@data$NAME == "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems"), border = "dark blue", lwd = 2, 
       xlim = c(164.8, 166), ylim = c(-21.9,-21.3))
  plot(subset(mpas, mpas@data$NAME == "Parc de la Zone CÃ´tiÃ¨re Ouest"), add = T, border = "cyan", lwd = 2)
  plot(subset(mpas, mpas@data$NAME == "ÃŽle Verte"), add = T, border = "orange", lwd = 2)
  plot(subset(mpas, mpas@data$NAME == "PoÃ©"), add = T, border = "orange", lwd = 2)
  plot(subset(mpas, mpas@data$NAME == "Roche PercÃ©e et de la Baie des Tortues" ), add = T, border = "orange", lwd = 2)
  plot(subset(mpas, mpas@data$NAME == "Ouano" ), add = T, border = "orange", lwd = 2)
  plot(transects, add = T)
  plot(new_caledonia, col ="#999999", border = "black", lwd = 1, cex = 2, add = T ) 
  box()
  #add arrow (can't change size)
  #cartography::north(pos = "bottomleft", col = "black")
  #add scalebar
  raster::scalebar(30, type='bar', below = "Km", lonlat = TRUE, lwd = 30)
  #add text
  raster::text(165.3,-21.8, label = "Lagoons of New Caledonia", col = "dark blue", cex = 1.8, font = 1)
  raster::text(165.55,-21.9, label = "West coast parc", col = "cyan", cex = 1.8, font = 1)
  raster::text(165.2,-21.7, label = "No-take mpas", col = "orange", cex = 1.8, font = 1)
  dev.off()
  
}


#' Make map of selected transect lines with no take mpas
#'
#' @param telem
#' @param maplatlon
#' @param transects
#'
#' @return
#' @export
#'

map_transects_notake_mpas <- function(maplatlon, transects, pa){
  
  transects_fortify = ggplot2::fortify(transects)
  
  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_line(data = transects_fortify, ggplot2::aes(x = long, y = lat, group = id), col = "white") + 
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "ÃŽle Verte"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "Roche PercÃ©e et de la Baie des Tortues"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id == "PoÃ©"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    ggplot2::geom_polygon(data = subset(pa2, pa2$id =="Ouano"), ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    #add scalebar
    ggsn::scalebar(data = NULL, dist = 5, transform = TRUE, model = "WGS84", dist_unit = "km", height = 0.3,
                   x.min = 164.2, x.max = 165, y.min = -21.84, y.max = -21.82,
                   st.dist = 1, st.color = "white", box.color = "white", border.size = 0.5, box.fill = c("white", "white"), st.size = 3.4) +
    #north arrow
    ggspatial::annotation_north_arrow(location = "bl", height = grid::unit(0.7, "cm"),  width = grid::unit(0.5, "cm"), 
                                      pad_y = grid::unit(1, "cm"), pad_x = grid::unit(1.2, "cm"), 
                                      style = ggspatial::north_arrow_orienteering(line_col = "white", text_col = "white")) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme_light()
  
  ggplot2::ggsave(here::here("outputs/map_transects_notake_mpas.png"), map, width = 7, height = 5)
  
}
