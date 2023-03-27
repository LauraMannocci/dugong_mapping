

#' Read and convert New Caledonia Allen coral shapefile benthic
#'
#' @param lon1
#' @param lon2
#' @param lat2
#' @param lat1
#'
#' @return
#' @export
#'

read_convert_coral_benthic <- function(lon1, lon2, lat2, lat1){
  
  # read shapefile
  data_allen = sf::st_read(dsn = "data/raw_data/allen/Benthic.gpkg", stringsAsFactors = FALSE)
  
  #crop
  data_allen_crop = sf::st_crop(data_allen, xmin = lon1, ymin = lat2, xmax = lon2, ymax = lat1)
  
  #convert sf to spatialPolygonsDataFrame
  sf_allen_coral_poly = sf:::as_Spatial(data_allen_crop)
  
  return(sf_allen_coral_poly)
  
}




#' Read and convert New Caledonia Allen coral shapefile geomorphology
#'
#' @param lon1
#' @param lon2
#' @param lat2
#' @param lat1
#'
#' @return
#' @export
#'

read_convert_coral_geomorpho <- function(lon1, lon2, lat2, lat1){
  
  # read shapefile
  data_allen = sf::st_read(dsn = "data/raw_data/allen/Geomorphic.gpkg", stringsAsFactors = FALSE)
  
  #crop
  data_allen_crop = sf::st_crop(data_allen, xmin = lon1, ymin = lat2, xmax = lon2, ymax = lat1)
  
  #convert sf to spatialPolygonsDataFrame
  sf_allen_coral_poly = sf:::as_Spatial(data_allen_crop)
  
  return(sf_allen_coral_poly)
  
}




#' Read New Caledonia Allen coral quaterly turbidity, produce average raster and resample
#'
#' @param lon1
#' @param lon2
#' @param lat2
#' @param lat1
#'
#' @return
#' @export
#'

read_average_resample_turbidity <- function(lon1, lon2, lat2, lat1, rast){
  
  # read quaterly rasters
  turb_q2 = raster::raster("data/raw_data/allen/Turbidity-Q2-2021/turbidity-quarterly_0.tif")
  turb_q3 = raster::raster("data/raw_data/allen/Turbidity-Q3-2021/turbidity-quarterly_0.tif")
  
  #crop
  turb_q2 = raster::crop(turb_q2, raster::extent(lon1, lon2, lat2, lat1))
  turb_q3 = raster::crop(turb_q3, raster::extent(lon1, lon2, lat2, lat1))
  
  #stack
  turb = raster::stack(turb_q2, turb_q3)
  
  #mean
  turb_mean = raster::mean(turb[[1:2]], na.rm = TRUE)
  
  #aggregate into coarser resolution
  turb_aggr = raster::aggregate(turb_mean, fact = 10, fun=mean)
  
  #project
  turb_proj = raster::projectRaster(turb_aggr, crs = "+init=epsg:3163") #project to lambert NC
  
  #resample
  turb_resamp = raster::resample(turb_proj, rast)
  
  #rename
  names(turb_resamp) = "turbidity"
  
  return(turb_resamp)
  
}





#' Make coverage raster for each habitat type
#'
#' @param cor 
#' @param r_xy 
#'
#' @return
#' @export
#'

make_raster_coral_benthic_type <- function(cor, r_xy, hab_type){
  
  #Transforming to sf object
  print("converting ...")
  cor_sf = sf::st_as_sf(cor)
  
  #Renaming names that made problems
  cor_sf$class[cor_sf$class == "Coral/Algae"] <- "Coral_algae"
  cor_sf$class[cor_sf$class == "Microalgal Mats"] <- "Microalgal_mats"
  
  #Filter specific habitat
  print("getting coverage ...")
  cor_sf_habitat = cor_sf %>% dplyr::filter(class == hab_type)
  
  #Loop to get coverage per polygon
  raster_loop <- foreach(i = 1:nrow(cor_sf_habitat)) %dopar% 
    
    #Calculate coverage for this value 
    exactextractr::coverage_fraction(r_xy, cor_sf_habitat[i,], crop = TRUE)[[1]]
  
  #merge raster lists into unique raster
  print("merging ...")
  r_merged <- do.call(raster::merge, raster_loop)
  
  #rename
  names(r_merged) <- tolower(hab_type)
  
  return(r_merged)
  
}




#' Make distance to seagrass raster
#'
#' @param coral
#' @param raster
#'
#' @return
#' @export
#'

make_dist_to_seagrass_raster <- function(cor, raster){
  
  # Extract polygon of seagrass from coral shapefil
  seagrass <- cor[cor@data$class ==  "Seagrass", ]
  
  #project
  seagrass = sp::spTransform(seagrass, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  # Calculate distance of raster cells to seagrass 
  cat('calculating distance \n')
  pts = as(raster, "SpatialPoints")
  #length(pts)
  dd = rgeos::gDistance(seagrass, pts, byid=TRUE)
  # This creates a matrix with a column for each feature in seagrass
  
  # To get the nearest distance (meters) to any feature, apply min over rows:
  raster[] = apply(dd, 1, min) #---------warning message
  # raster::plot(raster)
  
  # rename
  names(raster) = "dist_seagrass"
  
  return(raster)
  
}



#' Make coverage raster for each geomorphology type
#'
#' @param cor 
#' @param r_xy 
#' @param geom_type 
#'
#' @return
#' @export
#'

make_raster_coral_geomorpho_type <- function(cor, r_xy, geom_type){
  
  #Transforming to sf object
  print("converting ...")
  cor_sf = sf::st_as_sf(cor)
  
  #Renaming names that made problems
  cor_sf$class[cor_sf$class == "Inner Reef Flat"] <- "Inner_Reef_Flat"
  cor_sf$class[cor_sf$class == "Reef Slope"] <- "Reef_Slope"
  cor_sf$class[cor_sf$class == "Shallow Lagoon"] <- "Shallow_Lagoon"
  cor_sf$class[cor_sf$class == "Deep Lagoon"] <- "Deep_Lagoon"
  cor_sf$class[cor_sf$class == "Reef Slope"] <- "Reef_Slope"
  cor_sf$class[cor_sf$class == "Back Reef Slope"] <- "Back_Reef_Slope"
  cor_sf$class[cor_sf$class == "Outer Reef Flat"] <- "Outer_Reef_Flat"
  cor_sf$class[cor_sf$class == "Reef Crest"] <- "Reef_Crest"
  cor_sf$class[cor_sf$class == "Sheltered Reef Slope"] <- "Reef_Slope"
  cor_sf$class[cor_sf$class == "Terrestrial Reef Flat"] <- "Terrestrial_Reef_Flat"
  
  #Filter specific habitat
  print("getting coverage ...")
  cor_sf_habitat = cor_sf %>% dplyr::filter(class == geom_type)
  
  #Loop to get coverage per polygon
  raster_loop <- foreach(i = 1:nrow(cor_sf_habitat)) %dopar% 
    
    #Calculate coverage for this value 
    exactextractr::coverage_fraction(r_xy, cor_sf_habitat[i,], crop = TRUE)[[1]]
  
  #merge raster lists into unique raster
  print("merging ...")
  r_merged <- do.call(raster::merge, raster_loop)
  
  #rename
  names(r_merged) <- tolower(geom_type)
  
  return(r_merged)
  
}





#' assign zeros where habitat/geomorpho type is absent using habitat/geomorpho polygons
#'
#' @param cor 
#' @param r 
#' @param rastxy 
#'
#' @return
#' @export
#'

assign_zeros_where_absent_type <- function(cor, r, rastxy){
  
  #rasterize
  mask = raster::rasterize(r, rastxy)
  
  #convert non na value to 0
  mask[!is.na(mask[])] = 0 
  
  #mask
  rnew = raster::mask(cor, mask, updateNA = TRUE, updatevalue = 0)
  
  #assign 0s where appropriate
  rnew[is.na(rnew[])] = 9999
  rnew[rnew == 0] = NA
  rnew[rnew == 9999] = 0
  
  return(rnew)
  
}




#' assign zeros where habitat/geomorpho type is absent using coral cover raster as mask 
#'
#' @param cor 
#' @param mask 
#'
#' @return
#' @export
#'

assign_zeros_where_absent_type <- function(cor, mask){
  
  #convert 0s to NAs
  mask[mask == 0] <- NA
  
  #convert non na value to 0
  mask[!is.na(mask[])] = 0 
  
  #mask
  rnew = raster::mask(cor, mask, updateNA = TRUE, updatevalue = 0)
  
  #assign 0s where NAs
  rnew[is.na(rnew[])] = 0
  
  #mask a second time to replace 0s with NAs where land
  rnew = mask(rnew, mask)
  
  return(rnew)
  
}


  
#' Load original travel time raster (in seconds) from Florian Baletaud and resample
#'
#' @param rast 
#'
#' @return
#' @export
#'

load_travel_time_raster  <- function(rast){
  
  r = raster::raster(here::here("data", "raw_data", "travel_time", "TravelTime_coral_l3_Laura1km.tif"))
  
  #project
  rproj = raster::projectRaster(r, crs = "+init=epsg:3163") #project to lambert NC
  
  #resample
  rcrop = raster::resample(rproj, rast)
  
  #rename
  names(rcrop) = "travel_time"
  
  return(rcrop)
  
}



#' Map coral habitat
#'
#' @param dep

map_coral_habitat <- function(hab, hab_name){
  
  png(here::here(paste0("outputs/predictors/map_", hab_name, ".png")), width = 960, height = 480)
  raster::plot(hab)
  dev.off()
  
}  


#' Map travel time
#'
#' @param travel

map_travel_time <- function(travel){
  
  png(here::here("outputs", "predictors", "map_travel_time.png"), width = 960, height = 480)
  raster::plot(travel)
  dev.off()
  
}




#' Make population raster
#'
#' @param raster 
#'
#' @return
#' @export
#'

make_population_raster <- function(raster){
  
  # read population data
  pop = read.csv2(here::here("data", "raw_data", "population", "tribus_ville_JB.csv"))
  
  # calculate mean of population from 1989 - 2009
  pop$pop1989_2009 = rowMeans(pop[,4:7])
  pop2 = pop[,c("pop1989_2009", "longitude", "latitude")]
  
  # convert to spatial points
  sp::coordinates(pop2) = ~ longitude + latitude
  raster::projection(pop2) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  #project
  pop2 = sp::spTransform(pop2, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  # Inverse distance weighted” interpolation
  # From https://rspatial.org/raster/analysis/4-interpolation.html
  # First we fit a model for prediction. ~1 means “intercept only” (that is only ‘x’ and ‘y’ coordinates are used)
  gs = gstat::gstat(formula = pop1989_2009 ~ 1, locations = pop2, set = list(idp = 5)) #inverse distance power set to 5 (the lower the power the lower the influence of points far away)
  
  # Interpolate to raster
  idw = raster::interpolate(raster, gs)
  
  names(idw) <- "population"
    
  return(idw)
  
}


#' Map turbidity raster
#'
#' @param raster
#'
#' @return
#' @export
#'

map_turbidity <- function(raster){
  
  png(here::here("outputs", "predictors", "map_turbidity.png"), width = 960, height = 480)
  raster::plot(raster, main = "turbidity")
  dev.off()
  
}




#' Map population raster
#'
#' @param raster
#'
#' @return
#' @export
#'

map_population <- function(raster){
  
  png(here::here("outputs", "predictors", "map_population_log.png"), width = 960, height = 480)
  raster::plot(log(raster), main = "log population")
  dev.off()
  
  png(here::here("outputs", "predictors", "map_population.png"), width = 960, height = 480)
  raster::plot(raster, main = "population")
  dev.off()
  
}






#' Prepare population density raster (worldwide data, 2020 year and 5 x 5 km buffer)
#'
#' @param raster 
#' @param lon1 
#' @param lon2 
#' @param lat1 
#' @param lat2 
#'
#' @return
#' @export
#'

make_pop_density <- function(raster, lon1, lon2, lat1, lat2){
  
  # read raster of pop density for given years (number of persons per square kilometer)
  r = raster::raster(here::here("data", "raw_data", "population_density", "gpw_v4_population_density_rev11_2020_30_sec.tif"))
  
  # crop to NC
  ext <- raster::extent(lon1, lon2, lat2, lat1)
  r = raster::crop(r, ext)
  
  # define 25x25 moving window
  win = matrix(1,5,5)
  
  # focal mean over a 5x5 km moving window
  r = raster::focal(r, win,  fun=function(x){mean(x, na.rm=T)})
  
  # replacing NA's by 0s
  r[is.na(r[])] = 0
  
  # names
  names(r) <- "pop_dens"
  
  #project
  r = raster::projectRaster(r, crs = "+init=epsg:3163") #project to lambert NC
  
  # resample to raster
  r = raster::resample(r, raster)
  
  return(r)
  
}






#' Map population density 
#'
#' @param raster
#'
#' @return
#' @export
#'

map_pop_density <- function(raster){

  png(here::here("outputs", "predictors", "map_pop_density.png"), width = 960, height = 480)
  raster::plot(raster, main = "population density (persons per square kilometer)")
  dev.off()
  
}



#' Read New Caledonia coral shapefile (milenium coral project)
#'
#' @return
#' @export
#'

read_original_coralnc_millenium <- function(){
  # read shapefile (lambert nc projection)
  shp = rgdal::readOGR(here::here("data", "raw_data", "geomorpho_nc", "ae182c30-5b06-420a-bce6-52712658dff02020413-1-1lk3qk7.n062k.shp"))
  return(shp)
}




#' Make distance to land raster
#'
#' @param coral
#' @param raster
#'
#' @return
#' @export
#'

make_dist_to_land_raster <- function(coral, raster){
  
  # Extract polygon of main land from coral shapefil
  coralland <- coral[coral@data$l3_attrib ==  "Main Land", ]
   
  # Calculate distance of raster cells to main land 
  cat('calculating distance \n')
  pts = as(raster, "SpatialPoints")
  length(pts)
  dd = rgeos::gDistance(coralland, pts, byid=TRUE)
  # This creates a matrix with a column for each feature in coralland
  
  # To get the nearest distance (meters) to any feature, apply min over rows:
  raster[] = apply(dd, 1, min) #---------warning message
  # raster::plot(raster)
  
  # rename
  names(raster) = "dist_land"
  
  return(raster)
  
}


#' Map distance to land raster
#'
#' @param raster
#'
#' @return
#' @export
#'

map_dist_to_land <- function(raster){
  
  png(here::here("outputs", "predictors", "map_dist_to_land.png"), width = 960, height = 480)
  raster::plot(raster, main = "distance (m)")
  dev.off()
  
}





#' Make distance to reef raster
#'
#' @param coral
#' @param raster
#'
#' @return
#' @export
#'

make_dist_to_reef_raster <- function(coral, raster){
  
  # Extract polygon of reef from coral shapefile
  coralreef <- coral[coral@data$reef ==  1, ]
  
  cat('calculating distance\n')
  pts = as(raster, "SpatialPoints")
  length(pts)
  dd = rgeos::gDistance(coralreef, pts, byid=TRUE)
  # This creates a matrix with a column for each feature in coralreef
  
  # To get the nearest distance (meters) to any feature, apply min over rows:
  raster[] = apply(dd, 1, min) #---------warning message
  # raster::plot(raster)
  
  #rename
  names(raster) <- "dist_reef"
  
  return(raster)
  
}




#' Map distance to reef raster
#'
#' @param raster
#'
#' @return
#' @export
#'

map_dist_to_reef <- function(raster){

  png(here::here("outputs", "predictors", "map_dist_to_reef.png"), width = 960, height = 480)
  raster::plot(raster, main = "distance (m)")
  dev.off()
  
}


#' Map distance to seagrass raster
#'
#' @param raster
#'
#' @return
#' @export
#'

map_dist_to_seagrass <- function(raster){
  
  png(here::here("outputs", "predictors", "map_dist_to_seagrass.png"), width = 960, height = 480)
  raster::plot(raster, main = "distance (m)")
  dev.off()
  
}



#' Make MPA type raster
#'
#' @param raster
#'
#' @return
#' @export
#'

make_mpa_type_raster <- function(raster){
  
  # Read 3 shapefiles of new caledonia mpas
  shp0 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_0", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp1 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_1", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp2 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_2", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  
  # Merge
  shp = rbind(shp0, shp1, shp2)
  
  # Extract polygon of marine mpas
  shpmarine = shp[shp@data$MARINE !=  0, ]
  
  # Remove remaining non marine parks
  shpmarine = shpmarine[!shpmarine@data$NAME %in%  c("L'Etang de Koumac State Forest Reserve",
                                                     "Special Reserve Cap N'Dua", "Ouen Toro"), ]
  
  # Regroup reserve designation factor
  shpmarine$reserve_status = "partial reserve"
  shpmarine$reserve_status[shpmarine$NO_TK_AREA != 0.000] = "no-take reserve"
  shpmarine$reserve_status[shpmarine$NO_TK_AREA == 3236.000] = "partial reserve"
  shpmarine$reserve_status[shpmarine$NAME == "Atolls d'Entrecasteaux"] = "no-take reserve"
  shpmarine$reserve_status = as.factor(shpmarine$reserve_status)
  shpmarine$NAME = as.factor(shpmarine$NAME)
  
  # check plot
  print(sp::spplot(shpmarine,  c("reserve_status"), col.regions= sp::bpy.colors(length(levels(shpmarine$reserve_status)))))
  print(sp::spplot(shpmarine,  c("NAME"), col.regions= sp::bpy.colors(length(levels(shpmarine$NAME)))))
  
  #project
  shpmarine2 = sp::spTransform(shpmarine, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  # rasterize
  # For polygons, values are transferred if the polygon covers the center of a raster cell.
  r = raster::rasterize(shpmarine2, raster, field = "reserve_status", fun = "first", background = -1)
  
  # convert raster values to factors (error when raster:: removed)
  rr <- ratify(r)
  rat <- levels(rr)[[1]]
  rat$mpa_type <-  c("no reserve", levels(shpmarine$reserve_status))
  levels(rr) <- rat
  
  # names
  names(rr) = "mpa_type"
  
  return(rr)
  
}



#' Make MPA type raster no-take only
#'
#' @param raster
#'
#' @return
#' @export
#'

make_mpa_type_raster_no_take_only <- function(raster){
  
  # Read 3 shapefiles of new caledonia mpas
  shp0 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_0", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp1 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_1", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp2 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_2", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  
  # Merge
  shp = rbind(shp0, shp1, shp2)
  
  # Extract polygon of marine mpas
  shpmarine = shp[shp@data$MARINE !=  0, ]
  
  # Remove remaining non marine parks + all partial reserves
  shpmarine = shpmarine[!shpmarine@data$NAME %in%  c("L'Etang de Koumac State Forest Reserve",
                                                     "Special Reserve Cap N'Dua", "Ouen Toro",
                                                     #below are all partial mpas (nb)
                                                     "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems",
                                                     "Natural Park of the Coral Sea", #parial reserve with a no-take area of 3236 corresponding to entrecastaux atolls
                                                     "ÃŽle LeprÃ©dour",
                                                     "La Dieppoisse Special Marine Reserve",
                                                     "Parc du lagon sud: Ilot Amedee et Grand Recif Abore",
                                                     "Parc du Grand Lagon Sud",
                                                     "Parc de la Zone CÃ´tiÃ¨re Ouest",
                                                     "Cap N'dua",
                                                     "Parc de la CÃ´te OubliÃ©e",
                                                     "Parc Municipal du Ouen Toro - Albert EtuvÃ© et Lucien Audet"), ]
  
  # Regroup reserve designation factor
  shpmarine$reserve_status = "no-take reserve"
  shpmarine$reserve_status = as.factor(shpmarine$reserve_status)
  shpmarine$NAME = as.factor(shpmarine$NAME)
  
  # check plot
  print(sp::spplot(shpmarine,  c("reserve_status"), col.regions= sp::bpy.colors(length(levels(shpmarine$reserve_status)))))
  print(sp::spplot(shpmarine,  c("NAME"), col.regions= sp::bpy.colors(length(levels(shpmarine$NAME)))))
  
  #project
  shpmarine2 = sp::spTransform(shpmarine, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  # rasterize
  # For polygons, values are transferred if the polygon covers the center of a raster cell.
  r = raster::rasterize(shpmarine2, raster, field = "reserve_status", fun = "first", background = -1)
  
  # convert raster values to factors (error when raster:: removed)
  rr <- ratify(r)
  rat <- levels(rr)[[1]]
  rat$mpa_type <-  c("no reserve", levels(shpmarine$reserve_status))
  levels(rr) <- rat
  
  # names
  names(rr) = "mpa_type_no_take_only"

  return(rr)
  
}



#' Make MPA type raster no-take only poe / ile verte (in surveyed block)
#'
#' @param raster
#'
#' @return
#' @export
#'

make_mpa_type_raster_no_take_only_poe_ile_verte <- function(raster){
  
  # Read 3 shapefiles of new caledonia mpas
  shp0 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_0", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp1 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_1", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp2 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_2", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  
  # Merge
  shp = rbind(shp0, shp1, shp2)
  
  # Extract polygon of marine mpas
  shpmarine = shp[shp@data$MARINE !=  0, ]
  
  # Remove remaining non marine parks + all partial reserves
  shpmarine = shpmarine[shpmarine@data$NAME %in%  c("PoÃ©", "ÃŽle Verte"), ]
  
  # Regroup reserve designation factor
  shpmarine$reserve_status = "no-take reserve"
  shpmarine$reserve_status = as.factor(shpmarine$reserve_status)
  shpmarine$NAME = as.factor(shpmarine$NAME)
  
  # check plot
  print(sp::spplot(shpmarine,  c("reserve_status"), col.regions= sp::bpy.colors(length(levels(shpmarine$reserve_status)))))
  print(sp::spplot(shpmarine,  c("NAME"), col.regions= sp::bpy.colors(length(levels(shpmarine$NAME)))))
  
  #project
  shpmarine2 = sp::spTransform(shpmarine, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  # rasterize
  # For polygons, values are transferred if the polygon covers the center of a raster cell.
  r = raster::rasterize(shpmarine2, raster, field = "reserve_status", fun = "first", background = -1)
  
  # convert raster values to factors (error when raster:: removed)
  rr <- ratify(r)
  rat <- levels(rr)[[1]]
  rat$mpa_type <-  c("no reserve", levels(shpmarine$reserve_status))
  levels(rr) <- rat
  
  # names
  names(rr) = "mpa_type_no_take_only"
  
  return(rr)
  
}



#' Map mpa type
#'
#' @param raster
#'
#' @return
#' @export
#'

map_mpa_type <- function(raster){
  
  #to overlay coastline see https://stackoverflow.com/questions/17582532/r-overlay-plot-on-levelplot
  png(here::here("outputs", "predictors", "map_mpa_type.png"), width = 960, height = 480)
  myPal <- RColorBrewer::brewer.pal('Set3', n=3)
  myTheme <- rasterVis::rasterTheme(region = myPal)
  p=print(rasterVis::levelplot(raster, par.settings = myTheme))
  dev.off()
  
}


#' Map mpa type no take only
#'
#' @param raster
#'
#' @return
#' @export
#'

map_mpa_type_no_take_only <- function(raster){
  
  png(here::here("outputs", "predictors", "map_mpa_type_no_take_only.png"), width = 960, height = 480)
  myPal <- RColorBrewer::brewer.pal('Set3', n=3)
  myTheme <- rasterVis::rasterTheme(region = myPal)
  print(rasterVis::levelplot(raster, par.settings = myTheme))
  dev.off()
  
}





#' Make MPA presence raster
#'
#' @param raster
#'
#' @return
#' @export
#'

make_mpa_pres_raster <- function(raster){
  
  # Read 3 shapefiles of new caledonia mpas
  shp0 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_0", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp1 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_1", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp2 <- rgdal::readOGR(dsn = here::here("data", "raw_data", "mpas", "shp_2", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  
  # Merge
  shp = rbind(shp0, shp1, shp2)
  
  # Extract polygon of marine mpas
  shpmarine = shp[shp@data$MARINE !=  0, ]
  
  # Remove remaining non marine parks
  shpmarine = shpmarine[!shpmarine@data$NAME %in%  c("L'Etang de Koumac State Forest Reserve", "Special Reserve Cap N'Dua", "Ouen Toro"), ]
  
  #project
  shpmarine2 = sp::spTransform(shpmarine, sp::CRS("+init=epsg:3163")) #project to lambert NC
  
  # rasterize
  # For polygons, values are transferred if the polygon covers the center of a raster cell.
  r = raster::rasterize(shpmarine2, raster)
  
  r[!is.na(r[])] = 1 #mpa presence
  r[is.na(r[])] = 0 #mpa absence
  
  # convert raster values to factors (error when raster:: removed)
  rr <- ratify(r)
  rat <- levels(rr)[[1]]
  rat$mpa_pres <-  c("absence", "presence")
  levels(rr) <- rat
  
  # names
  names(rr) = "mpa_pres"
  
  return(rr)
  
}




#' Map mpa pres
#'
#' @param raster
#'
#' @return
#' @export
#'

map_mpa_pres <- function(raster){
  
  png(here::here("outputs", "predictors", "map_mpa_pres.png"), width = 960, height = 480)
  raster::plot(raster)
  dev.off()
  
}




#' read depth from Jean roger (IRD) and resample to study area
#'
#' @param rast 
#'
#' @return
#' @export

read_resample_depth <- function(rastlatlon, rastxy){
  
  depth = raster::raster(here::here("data", "raw_data", "Bathy", "MNT-nettoyé_v3_FINAL.tif"))
  
  #crop
  depthcrop = raster::crop(depth, rastlatlon)
  
  #project
  depthproj = raster::projectRaster(depthcrop, crs = "+init=epsg:3163") #project to lambert NC
  
  # resample depth raster with study area raster
  depthnew = raster::resample(depthproj, rastxy)
  
  names(depthnew) = "depth"
  
  return(depthnew)
}

 


#' Map depth
#'
#' @param dep

map_depth <- function(dep){
  
  png(here::here("outputs", "predictors", "map_depth.png"), width = 960, height = 480)
  raster::plot(dep)
  dev.off()
  
}  
  


#' Extend rasters based on defined study area raster
#'
#' @param r 
#' @param rast 
#'
#' @return
#' @export
#'

extend_raster <- function(r, rast){
  
  rnew = raster::extend(r, rast, value = NA)
  
  return(rnew)
  
}



 

#' Mask based on dist to mainland raster
#'
#' @param r 
#' @param mask 
#'
#' @return
#' @export
#'

mask_with_land <- function(r, mask){
    
  rnew = raster::mask(r, mask, maskvalue = 0)
  
  return(rnew)

}


