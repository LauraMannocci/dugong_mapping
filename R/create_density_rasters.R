


#' Make dataframe per grid cell (incl. empty cells) of observations, track length and densities per date and per species 
#'
#' @param polyobs
#' @param polytracks
#' @param footprintwidth
#'
#' @return
#' @export
#'

make_density_df_all_species <- function(polyobs, polytracks, footprintwidth){
  
  
  ### polyobs
  
  polyobs %>% 
    dplyr::rename(n = n_count_avail_corrected) -> polyobs
  
  #to avoid errors when replacing nas
  polyobs$object = as.character(polyobs$object)
  
  # replace NAs
  polyobs2 = polyobs %>%
    dplyr::mutate(date = as.character(date)) %>%
    tidyr::replace_na(list(n = 0, object = "unknown", date = "unknown"))
  
  # convert back to spatial object for plotting
  polyobs3 = sf::as_Spatial(polyobs2)
  
  #add coords
  polyobs3@data$lon = coordinates(polyobs3)[,1]
  polyobs3@data$lat = coordinates(polyobs3)[,2]
  
  #reformat
  polyobs3@data %>%
    #replace special character
    dplyr::mutate(date = stringr::str_replace_all(date, "-", "_")) %>%
    #reshape
    tidyr::pivot_wider(names_from = c(date, object), values_from = n, values_fill = 0, names_glue = "n_{object}_{date}") %>%
    #remove columns
    dplyr::select(c(-layer, -n_unknown_unknown)) %>%
    #calculate tot observation nb per species
    dplyr::mutate(n_Turtle = rowSums(dplyr::across(tidyselect::starts_with("n_Turtle")))) %>%
    dplyr::mutate(n_Dugong_certain_probable = rowSums(dplyr::across(tidyselect::starts_with("n_Dugong_certain_probable")))) %>%
    dplyr::mutate(n_Eagle_ray= rowSums(dplyr::across(tidyselect::starts_with("n_Eagle_ray")))) %>%
    dplyr::mutate(n_Round_ray = rowSums(dplyr::across(tidyselect::starts_with("n_Round_ray")))) %>%
    dplyr::mutate(n_Manta_ray = rowSums(dplyr::across(tidyselect::starts_with("n_Manta_ray")))) %>%
    dplyr::mutate(n_Dolphin = rowSums(dplyr::across(tidyselect::starts_with("n_Dolphin")))) %>%
    dplyr::mutate(n_Shark = rowSums(dplyr::across(tidyselect::starts_with("n_Shark")))) -> polyobs4
  
  
  
  ### polytracks
  
  # replace NAs
  polytracks2 = polytracks %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::select(-layer.y, -layer.x) %>%
    dplyr::mutate(length = as.numeric(length)) %>%
    tidyr::replace_na(list(length = 0, date = "unknown"))
  
  # convert back to spatial object for plotting
  polytracks3 = sf::as_Spatial(polytracks2)
  
  #add coords
  polytracks3@data$lon = coordinates(polytracks3)[,1]
  polytracks3@data$lat = coordinates(polytracks3)[,2]
  
  #reformat
  polytracks3@data %>%
    #replace special character
    dplyr::mutate(date = stringr::str_replace_all(date, "-", "_")) %>%
    #reshape
    tidyr::pivot_wider(names_from = date, values_from = length, values_fill = 0, names_glue = "length_{date}") %>%
    #calculate tot length
    dplyr::mutate(length = rowSums(dplyr::across(tidyselect::starts_with("length"))))  -> polytracks4
  
  
  #join polyobs and polytracks and compute density per date and per species
  polyobs4 %>%
    #join
    dplyr::right_join(polytracks4, by = c("id"))  %>%
    #compute desnity per date and per species
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_08_04"),
                                .fns = function(x){10000 * x/(length_2021_08_04*footprintwidth)}, #density converted from ind/m2 to in indiv / 10000m2 (=0.1 km2 or 100m x 100m)
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_06_21"),
                                .fns = function(x){10000 * x/(length_2021_06_21*footprintwidth)}, 
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_07_29"),
                                .fns = function(x){10000 * x/(length_2021_07_29*footprintwidth)}, 
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_06_16"),
                                .fns = function(x){10000 * x/(length_2021_06_16*footprintwidth)}, 
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_06_22"),
                                .fns = function(x){10000 * x/(length_2021_06_22*footprintwidth)},
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_06_04"),
                                .fns = function(x){10000 * x/(length_2021_06_04*footprintwidth)}, 
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_06_05"),
                                .fns = function(x){10000 * x/(length_2021_06_05*footprintwidth)}, 
                                .names = "density_{.col}")) %>%
    #compute density per species
    dplyr::mutate(density_Turtle = 10000 * n_Turtle / (length*footprintwidth)) %>%  #density converted from ind/m2 to in indiv / 10000m2 (=0.1 km2 or 100m x 100m)
    dplyr::mutate(density_Dugong_certain_probable = 10000 * n_Dugong_certain_probable / (length*footprintwidth)) %>%  
    dplyr::mutate(density_Round_ray = 10000 * n_Round_ray / (length*footprintwidth)) %>% 
    dplyr::mutate(density_Eagle_ray = 10000 * n_Eagle_ray / (length*footprintwidth)) %>%  
    dplyr::mutate(density_Manta_ray = 10000 * n_Manta_ray / (length*footprintwidth)) %>%
    dplyr::mutate(density_Dolphin = 10000 * n_Dolphin / (length*footprintwidth)) %>%  
    dplyr::mutate(density_Shark = 10000 * n_Shark / (length*footprintwidth)) %>%  
    
    #clean
    dplyr::select(-tidyselect::starts_with("density_length")) %>%
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    dplyr::select(-lat.y, -lon.y) -> result
  
  
  #clean column names
  names(result) <- gsub(x = names(result), pattern = "density_n", replacement = "density")
  
  
  # NB 0/0 returns nan (no observation and no effort)
  
  
  return(result)
  
}







#' Get raster stack of density for given species
#'
#' @param df 
#' @param species 
#'
#' @return
#' @export
#'

get_density_stack_species <- function(df, species){
  
  #select species columns
  d <- data.frame(matrix(ncol = length(names(df)), nrow = 1))
  names(d) <- names(df)
  
  d %>% 
    dplyr::select(tidyselect::starts_with(paste0("density_", species))) %>% 
    names() -> columns
  
  #make stack
  r <- raster::stack()
    
  for (i in 1:length(columns)){
    
    r = raster::stack(r, raster::rasterFromXYZ(df[,c("lon", "lat", columns[i])],  crs="+init=epsg:3163"))
    
  }
                              
  
  return(r)
  
}






#' Make dataframe per grid cell centers (including empty cell centers) of abundance and surveyed area
#'
#' @param polyobs
#' @param polytracks
#' @param footprintwidth
#'
#' @return
#' @export
#'

make_abundance_df <- function(polyobs, polytracks, footprintwidth){
  
  
  ### polyobs
  
  polyobs %>% 
    dplyr::rename(n = n_count_avail_corrected) -> polyobs
  
  #to avoid errors when replacing nas
  polyobs$object = as.character(polyobs$object)
  
  # replace NAs
  polyobs2 = polyobs %>%
    dplyr::mutate(date = as.character(date)) %>%
    tidyr::replace_na(list(n = 0, object = "unknown", date = "unknown"))
  
  # convert back to spatial object for plotting
  polyobs3 = sf::as_Spatial(polyobs2)
  
  #add coords
  polyobs3@data$lon = coordinates(polyobs3)[,1]
  polyobs3@data$lat = coordinates(polyobs3)[,2]
  
  #reformat
  polyobs3@data %>%
    #replace special character
    dplyr::mutate(date = stringr::str_replace_all(date, "-", "_")) %>%
    #reshape
    tidyr::pivot_wider(names_from = c(date, object), values_from = n, values_fill = 0, names_glue = "n_{object}_{date}") %>%
    #remove columns
    dplyr::select(c(-layer, -n_unknown_unknown)) %>%
    #calculate tot observation nb per species
    dplyr::mutate(n_Dugong_certain_probable = rowSums(dplyr::across(tidyselect::starts_with("n_Dugong_certain_probable")))) -> polyobs4
  
  
  
  ### polytracks
  
  # replace NAs
  polytracks2 = polytracks %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::select(-layer.y, -layer.x) %>%
    dplyr::mutate(length = as.numeric(length)) %>%
    tidyr::replace_na(list(length = 0, date = "unknown"))
  
  # convert back to spatial object for plotting
  polytracks3 = sf::as_Spatial(polytracks2)
  
  #add coords
  polytracks3@data$lon = coordinates(polytracks3)[,1]
  polytracks3@data$lat = coordinates(polytracks3)[,2]
  
  #reformat
  polytracks3@data %>%
    #replace special character
    dplyr::mutate(date = stringr::str_replace_all(date, "-", "_")) %>%
    #reshape
    tidyr::pivot_wider(names_from = date, values_from = length, values_fill = 0, names_glue = "length_{date}") %>%
    #calculate tot length
    dplyr::mutate(length = rowSums(dplyr::across(tidyselect::starts_with("length"))))  -> polytracks4
  
  
  ### join polyobs and polytracks and get abundance and surveyed area
  
  polyobs4 %>%
    #join
    dplyr::right_join(polytracks4, by = c("id"))  %>%
    #compute area surveyed in m2    
    dplyr::mutate(area_surveyed_m2 = length*footprintwidth )%>%  
    #clean
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    dplyr::select(c(lat, lon, n_Dugong_certain_probable, area_surveyed_m2)) -> result
  
  
  return(result)
  
}




