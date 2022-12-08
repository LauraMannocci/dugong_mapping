
#' Make open street map for study area projected
#'
#' @return
#' @export
#'

osm_mapproj <- function(map){
  
  # Reproject OSM
  mapproj = OpenStreetMap::openproj(map, projection = "+init=epsg:3163")# NC projection
  
  return(mapproj)
  
}



#' Make grid from projected study area raster 
#'
#' @param rproj
#'
#' @return
#' @export
#'

make_grid <- function(rproj){

  # convert raster to spatial polygon
  p = raster::rasterToPolygons(rproj)
  
  # convert to sf object and change names
  polygon = sf::st_as_sf(p)
  
  #add id
  polygon$id = 1:nrow(polygon)
  
  return(polygon)
  
}



#' restrict telemetry to dates
#'
#' @param telem
#' @param dates
#'
#' @return
#' @export
#'

restrict_telem_dates <- function(telem, dates){
  
  telem %>%
    dplyr::filter(date %in% dates) %>%
    droplevels() -> telem_dates
  
  return(telem_dates)
}





#' convert telemetry points to lines
#'
#' @param telem
#'
#' @return
#' @export
#'

convert_telemetry_points_to_lines <- function(telem){
  
  library(sp)
  #set coords
  coordinates(telem) = ~ lon + lat
  
  # list of Lines per date, each with one Line in a list
  # ***** does not work in function
  listlines = lapply(split(telem, telem$date), function(x) Lines(list(Line(coordinates(x))), x$date[1L]))
  
  return(listlines)
}




#' Sum length of tracks (m) in grid cells per date
#'
#' @param listlines
#' @param polygon
#' @param telem
#'
#' @return
#' @export
#'


sum_length_per_grid_per_date <- function(polygon, listlines, dates){
  
  # convert to spatial lines
  splines = sp::SpatialLines(listlines, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  # convert to SpatialLinesDataFrame
  splinesdata = sp::SpatialLinesDataFrame(sl = splines, data = data.frame(date = dates), match.ID = FALSE)
  
  # project spatial lines
  lproj = sp::spTransform(splinesdata, CRS("+init=epsg:3163")) #NC projection
  
  # convert to sf object
  lines = sf::st_as_sf(lproj)
  
  #convert polygon to sf
  polygon %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:nrow(.)) -> polygon2
  
  #intersect polygon with lines and sum line length per date
  sf::st_intersection(polygon2, lines) %>%
    #dplyr::group_by(id, date) %>%
    dplyr::mutate(length = sf::st_length(.)) %>%
    sf::st_drop_geometry() -> intersection
  
  #join polygon with lines
  polygon2 %>%
    dplyr::left_join(intersection, by = "id") %>% 
    dplyr::filter(is.na(date) == FALSE) -> polygon3
  
  return(polygon3)
  
}




#' Map track length per grid cell per date 
#'
#' @param maplatlonproj
#' @param polytracks
#' @param size 
#'
#' @return
#' @export
#'

map_tracklen_per_grid_per_date <- function(maplatlonproj, polytracks, size){
  
  # convert back to spatial object for plotting
  polytracks2 = sf::as_Spatial(polytracks)
  
  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      date = polytracks2$date,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])
  
  # sum length per grid cell
  effort2 = effort %>%
    dplyr::filter(length > 0)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
      ggplot2::geom_point(data = effort2, ggplot2::aes(x = lon, y = lat, color = length), shape = 15, size = size, alpha = 0.85) +
      ggplot2::facet_wrap(~date, nrow = 2, ncol = 4) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                    name = "length(m)")
    
  ggplot2::ggsave(here::here("outputs/map_tracklen_per_grid_per_date.png"), map, width = 7, height = 5)
  
}



#' Map track length per grid cell 
#'
#' @param maplatlonproj
#' @param polytracks
#' @param size 
#'
#' @return
#' @export
#'

map_tracklen_per_grid <- function(maplatlonproj, polytracks, size){
  
  # convert back to spatial object for plotting
  polytracks2 = sf::as_Spatial(polytracks)
  
  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])
  
  # sum length per grid cell and select polygons with effort > 0
  effort2 = effort %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_length = sum(length)) %>% #IMPORTANT to sum lenght per cell across all flights
    dplyr::filter(tot_length > 0)
  
  # map
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_point(data = effort2, ggplot2::aes(x = lon, y = lat, color = tot_length), shape = 15, size = size) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                  name = "length(m)")
  
  ggplot2::ggsave(here::here("outputs/map_tracklen_per_grid.png"), map, width = 7, height = 5)
  
}






#' Make dataframe length of tracks per grid cell per date
#'
#' @param polytracks
#'
#' @return
#' @export
#'

make_df_tracklen_per_grid_per_date <- function(polytracks){
  
  # convert back to spatial object for plotting
  polytracks2 = sf::as_Spatial(polytracks)
  
  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      date = polytracks2$date,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])
  
  # sum length per grid cell
  effort2 = effort %>%
    dplyr::filter(length > 0)
  
  return(effort2)
  
}



#' Count total number of observations in grid cells per date
#'
#' @param polygon
#' @param telem_obs
#'
#' @return
#' @export
#'
#'
count_obs_per_grid_per_date <- function(polygon, telem_obs){
  
  # keep megafauna observations
  telem_obs %>%
    dplyr::filter(object %in% c("Turtle", "Dugong_certain_probable", "Round_ray", "Eagle_ray", "Manta_ray", "Dolphin", "Shark")) -> telem_obs2
  
  # convert to sf object
  points = sf::st_as_sf(x = telem_obs2,
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  #project points
  points = sf::st_transform(points, CRS("+init=epsg:3163")) #NC projection
  
  # Intersection between polygon and points and count nb of points per date per object
  sf::st_intersection(x = polygon, y = points) %>%
    dplyr::group_by(id, date, object) %>%
    dplyr::count() %>%
    sf::st_drop_geometry() -> intersection
  
  #join polygon with points
  polygon %>%
    dplyr::left_join(intersection, by = "id") -> polygon2
  
  return(polygon2)
  
}




#' Map number of observations per grid cell for given species 
#'
#' @param maplatlonproj
#' @param polygon
#' @param species 
#' @param size 
#'
#' @return
#' @export
#'
#'
map_obs_per_grid_per_date_species <- function(maplatlonproj, polygon, species, size){
  
  # select polygons with counts > 0 for species
  polygon2 = polygon %>%
    dplyr::filter(object == species)
  
  # convert back to spatial object for plotting
  polygon3 = sf::as_Spatial(polygon2)
  
  # make dataframe for plotting
  counts = data.frame(id = polygon3$id,
                      count = polygon3$n,
                      date = polygon3$date,
                      lon = coordinates(polygon3)[,1],
                      lat = coordinates(polygon3)[,2])
  
  dates = unique(counts$date)
  
  #map1
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
      ggplot2::geom_point(data = subset(counts, counts$date %in% dates[1:3]), ggplot2::aes(x = lon, y = lat, color = count), shape = 15, size=size, alpha = 0.85) +
      ggplot2::facet_wrap(~date, nrow = 2, ncol = 2) +
      ggplot2::ggtitle(species) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                      name = "individuals")
      
  ggplot2::ggsave(here::here(paste0("outputs/map_obs_per_grid_per_date_", species, "1.png")), map, width = 8, height = 6)
  
  #map2
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = subset(counts, counts$date %in% dates[4:7]), ggplot2::aes(x = lon, y = lat, color = count), shape = 15, size=size, alpha = 0.85) +
    ggplot2::facet_wrap(~date, nrow = 2, ncol = 2) +
    ggplot2::ggtitle(species) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                  name = "individuals")
  
  ggplot2::ggsave(here::here(paste0("outputs/map_obs_per_grid_per_date_", species, "2.png")), map, width = 8, height = 6)
  
}




#' Map number of observations per grid cell for given species 
#'
#' @param maplatlonproj
#' @param polygon
#' @param species 
#' @param size 
#'
#' @return
#' @export
#'

map_obs_per_grid_species <- function(maplatlonproj, polygon, species, size){
  
  # select polygons with counts > 0 for species
  polygon2 = polygon %>%
    dplyr::filter(object == species)
  
  # convert back to spatial object for plotting
  polygon3 = sf::as_Spatial(polygon2)
  
  # make dataframe for plotting
  counts = data.frame(id = polygon3$id,
                      count = polygon3$n,
                      date = polygon3$date,
                      lon = coordinates(polygon3)[,1],
                      lat = coordinates(polygon3)[,2])
  
  #IMPORTANT to sum counts per cell across all flights
  counts2 = counts %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_count = sum(count)) %>%
    dplyr::filter(tot_count > 0)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_point(data = counts2, ggplot2::aes(x = lon, y = lat, color = tot_count), shape = 15, size = size, alpha = 0.85) +
    #ggplot2::theme_minimal() +
    ggplot2::ggtitle(species) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                  name = "individuals")
  
  ggplot2::ggsave(here::here(paste0("outputs/map_obs_per_grid_", species, ".png")), map, width = 7, height = 5)
  
}




#' Map density per grid cell for given species per date (nb of observations / (length of tracks in meters * footprint_width in meters))
#'
#' @param polyobs
#' @param polytracks
#' @param footprintwidth
#' @param species
#' @param maplatlonproj
#' @param size 
#'
#' @return
#' @export
#'


map_dens_per_grid_per_date_species <- function(maplatlonproj, polyobs, polytracks, footprintwidth, species, size){
  
  # select polygons with counts > 0 for species
  polyobs2 = polyobs %>%
    dplyr::filter(object == species)
  
  # convert back to spatial object for plotting
  polyobs3 = sf::as_Spatial(polyobs2)
  
  # make dataframe for plotting
  counts = data.frame(id = polyobs3$id,
                      count = polyobs3$n,
                      date = polyobs3$date,
                      lon = coordinates(polyobs3)[,1],
                      lat = coordinates(polyobs3)[,2])
  
  # convert back to spatial object for plotting (in meters)
  polytracks2 = sf::as_Spatial(polytracks)
  
  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      date = polytracks2$date,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])
  
  #select polygons with effort > 0
  effort2 = effort %>%
    dplyr::filter(length > 0)
  
  # merge effort and counts based on polygon id and date and calculate density
  
  result = counts %>%
    dplyr::right_join(effort2, by = c("id", "date")) %>%
    dplyr::mutate(density = count / (length*footprintwidth))  %>% #density in indiv/m2
    dplyr::mutate(density = density * 250000)  %>%  #density in indiv/250000m2 (1 ha = 10000m2 - 1m2 = 10-4 ha)
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    dplyr::select(-lat.y, -lon.y)
  
  dates = unique(result$date)
  
  #map1
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
      ggplot2::geom_point(data = subset(result, result$date %in% dates[1:3]), ggplot2::aes(x = lon, y = lat, color = density), shape = 15, size = size, alpha = 0.85) +
      ggplot2::facet_wrap(~date, nrow = 2, ncol = 2) +
      ggplot2::ggtitle(species) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                    name = "indiv/0.25km2") #ie 250000m2
    
  ggplot2::ggsave(here::here(paste0("outputs/map_dens_per_grid_per_date_", species, "1.png")), map, width = 7, height = 5)
  
  #map2
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = subset(result, result$date %in% dates[4:7]), ggplot2::aes(x = lon, y = lat, color = density), shape = 15, size = size, alpha = 0.85) +
    ggplot2::facet_wrap(~date, nrow = 2, ncol = 2) +
    ggplot2::ggtitle(species) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                  name = "indiv/0.25km2") #ie 250000m2
  
  ggplot2::ggsave(here::here(paste0("outputs/map_dens_per_grid_per_date_", species, "2.png")), map, width = 7, height = 5)
    
  
}



#' Map density per grid cell for given species in LOG scale with zeros (nb of observations / (length of tracks in meters * footprint_width in meters))
#'
#' @param polyobs
#' @param polytracks
#' @param footprintwidth
#' @param species
#' @param maplatlonproj
#' @param size 
#'
#' @return
#' @export
#'

map_dens_per_grid_species_log_with_zero <- function(maplatlonproj, polyobs, polytracks, footprintwidth, species, size){
  
  # select polygons with counts of species
  polyobs2 = polyobs %>%
    dplyr::filter(object == species)
  
  # convert back to spatial object for plotting
  polyobs3 = sf::as_Spatial(polyobs2)
  
  # make dataframe for plotting
  counts = data.frame(id = polyobs3$id,
                      count = polyobs3$n,
                      date = polyobs3$date,
                      lon = coordinates(polyobs3)[,1],
                      lat = coordinates(polyobs3)[,2])
  
  #IMPORTANT to sum counts per cell across all flights
  counts2 = counts %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_count = sum(count)) %>%
    dplyr::filter(tot_count > 0)
  
  # convert back to spatial object for plotting (in meters)
  polytracks2 = sf::as_Spatial(polytracks)
  
  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])
  
  # sum length per grid cell and select polygons with effort > 0
  effort2 = effort %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_length = sum(length)) %>% #IMPORTANT to sum lenght per cell across all flights
    dplyr::filter(tot_length > 0)
  
  # merge effort and counts based on polygon id and calculate density
  result = counts2 %>%
    dplyr::left_join(effort2, by = "id") %>%
    dplyr::mutate(density = tot_count / (tot_length*footprintwidth))  %>% #density in indiv/m2
    dplyr::mutate(density = density * 250000)  %>%  #density in indiv/250000m2 (1 ha = 10000m2 - 1m2 = 10-4 ha)
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    dplyr::select(-lat.y, -lon.y)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ##add cells were there were tracks with no observations (ie zeros)
    ggplot2::geom_point(data = effort2, ggplot2::aes(x = lon, y = lat), color = "white", shape = 15, size = size, alpha = 0.4) +
    ggplot2::geom_point(data = result, ggplot2::aes(x = lon, y = lat, color = log(density)), shape = 15, size = size, alpha = 1) +
    #ggplot2::theme_minimal() +
    ggplot2::ggtitle(species) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 11),
                   legend.title = ggplot2::element_text(hjust = 0.5, size = 13)) +
    ggplot2::scale_color_gradient(low = "bisque1", high = "red3", na.value = NA,
                                  name = "Log ind/0.25km2") #ie 250000m2
    
  ggplot2::ggsave(here::here(paste0("outputs/map_dens_per_grid_", species, "_log_with_zero.png")), map, width = 7, height = 5)
  
}






#' Map density per grid cell for given species with zeros (nb of observations / (length of tracks in meters * footprint_width in meters))
#'
#' @param polyobs
#' @param polytracks
#' @param footprintwidth
#' @param species
#' @param maplatlonproj
#' @param size 
#'
#' @return
#' @export
#'

map_dens_per_grid_species_with_zero <- function(maplatlonproj, polyobs, polytracks, footprintwidth, species, size){
  
  # select polygons with counts of species
  polyobs2 = polyobs %>%
    dplyr::filter(object == species)
  
  # convert back to spatial object for plotting
  polyobs3 = sf::as_Spatial(polyobs2)
  
  # make dataframe for plotting
  counts = data.frame(id = polyobs3$id,
                      count = polyobs3$n,
                      date = polyobs3$date,
                      lon = coordinates(polyobs3)[,1],
                      lat = coordinates(polyobs3)[,2])
  
  #IMPORTANT to sum counts per cell across all flights
  counts2 = counts %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_count = sum(count)) %>%
    dplyr::filter(tot_count > 0)
  
  # convert back to spatial object for plotting (in meters)
  polytracks2 = sf::as_Spatial(polytracks)
  
  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])
  
  # sum length per grid cell and select polygons with effort > 0
  effort2 = effort %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_length = sum(length)) %>% #IMPORTANT to sum lenght per cell across all flights
    dplyr::filter(tot_length > 0)
  
  # merge effort and counts based on polygon id and calculate density
  result = counts2 %>%
    dplyr::left_join(effort2, by = "id") %>%
    dplyr::mutate(density = tot_count / (tot_length*footprintwidth))  %>% #density in indiv/m2
    dplyr::mutate(density = density * 250000)  %>%  #density in indiv/50000m2 (1 ha = 10000m2 - 1m2 = 10-4 ha)
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    dplyr::select(-lat.y, -lon.y)
  
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ##add cells were there were tracks with no observations (ie zeros)
    ggplot2::geom_point(data = effort2, ggplot2::aes(x = lon, y = lat), color = "white", shape = 15, size = size, alpha = 0.4) +
    ggplot2::geom_point(data = result, ggplot2::aes(x = lon, y = lat, color = density), shape = 15, size = size, alpha = 1) +
    #ggplot2::theme_minimal() +
    ggplot2::ggtitle(species) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 11),
                   legend.title = ggplot2::element_text(hjust = 0.5, size = 13)) +
    ggplot2::scale_color_gradient(low = "bisque1", high = "red3", na.value = NA,
                                  name = "ind/0.25km2")  #ie 250000m2
  
  ggplot2::ggsave(here::here(paste0("outputs/map_dens_per_grid_", species, "_with_zero.png")), map, width = 7, height = 5)
  
}


