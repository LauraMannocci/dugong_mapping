

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



#' Make map of Allen coral geomorphology polygons overlaid on osm
#'
#' @param cor
#' @param maplatlon
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#' @param offset_lon
#' @param offset_lat
#' @param pa
#' @param dist
#'
#' @return
#' @export
#'

map_allen_coral_osm <- function(maplatlon, pa, cor, lon1, lon2, lat1, lat2, dist, offset_lon, offset_lat){
  
  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE
  
  #crop polygons to osm extent
  cor3 = raster::crop(cor2, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1],
                                           maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))
  
  # fortify
  cor4 = cor3 %>%
    ggplot2::fortify(region = "class")
  
  #add mpa
  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon)
  
  map = map +
    ggplot2::geom_polygon(data = cor4, ggplot2::aes(x = long, y = lat, group = group, fill = id), alpha = 0.8) +
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
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor4$id)), c = 70, l = 50))) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    #add mpa
    ggplot2::geom_polygon(data = pa2, ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0.1)
  
  ggplot2::ggsave(here::here("outputs/map_allen_coral_osm.png"), map, width = 7, height = 5)
  
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
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat, color = date), size = 0.01, alpha = 0.5) +
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
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="white") +
    ggplot2::facet_wrap(~date, nrow = 2) + #facet per date
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
    "Dugong_certain" = paste0("Dugong_certain ind=", nrow(telem_obs[telem_obs$object=="Dugong_certain",]),
                              " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Dugong_certain"]))),
    "Dugong_probable" = paste0("Dugong_probable ind=", nrow(telem_obs[telem_obs$object=="Dugong_probable",]),
                               " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Dugong_probable"]))),
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


