# for Raph


# regional abundance prediction raster
pred_region = raster::raster(here::here("data", "processed_data", "predictions", "predictions_region_interp.grd"))

#shapefile mpas no take
mpas <- read_mpanc()
mpas_notake_shp <- extract_notake_mpas(mpas)

#read NC land
nc = rgdal::readOGR(here::here("data", "raw_data", "nc_land", "NOUVELLE_CALEDONIE.shp"))
raster::crs(nc) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia

# rasters prioritisation abundance (targets 10 to 90%)
prioritisation_abundance_10 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_10.grd"))
prioritisation_abundance_20 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_20.grd"))
prioritisation_abundance_30 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_30.grd"))
prioritisation_abundance_40 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_40.grd"))
prioritisation_abundance_50 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_50.grd"))
prioritisation_abundance_60 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_60.grd"))
prioritisation_abundance_70 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_70.grd"))
prioritisation_abundance_80 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_80.grd"))
prioritisation_abundance_90 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_abondance_no_lockin_90.grd"))


prioritisation_range_10 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_10.grd"))
prioritisation_range_20 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_20.grd"))
prioritisation_range_30 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_30.grd"))
prioritisation_range_40 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_40.grd"))
prioritisation_range_50 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_50.grd"))
prioritisation_range_60 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_60.grd"))
prioritisation_range_70 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_70.grd"))
prioritisation_range_80 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_80.grd"))
prioritisation_range_90 = raster::raster(here::here("data", "processed_data", "prioritisation", "prioritisation_range_no_lockin_sol1_90.grd"))






save(pred_region, nc, mpas, 
     prioritisation_abundance_10, prioritisation_abundance_20, prioritisation_abundance_30, prioritisation_abundance_40, prioritisation_abundance_50,
     prioritisation_abundance_60, prioritisation_abundance_70, prioritisation_abundance_80, prioritisation_abundance_90,
     prioritisation_range_10, prioritisation_range_20, prioritisation_range_30, prioritisation_range_40, prioritisation_range_50,
     prioritisation_range_60, prioritisation_range_70, prioritisation_range_80, prioritisation_range_90,
     file = here::here("for_raph.RData"))
