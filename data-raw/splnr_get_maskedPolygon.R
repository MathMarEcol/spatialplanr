# splnr_get_MaskedPolygon <- function(df, res, mask, inverse){
#
#   longlat <- "EPSG:4326"
#
#   # Creating a empty raster
#   rs <- raster::raster(ncol = 360*(1/res), nrow = 180*(1/res))
#   rs[] <- 1:raster::ncell(rs)
#   raster::crs(rs) <- longlat
#
#   # Fasterize the land object
#   df_rs <- fasterize::fasterize(df, rs)
#
#   if(is.na(mask)) {
#     masked_df <- df_rs
#   } else{
#     mask_sp <- as(mask, "Spatial")
#     masked_df <- terra::mask(df_rs, mask_sp, inverse = inverse)
#   }
#
#   # Remove land pixels that are still present / delete certain aggrupation of pixels.
#   masked_clump <- raster::clump(masked_df, directions = 8)
#   df_clump <- raster::freq(masked_clump) %>%
#     as.data.frame()
#
#   str(which(df_clump$count <= 9)) # which rows of the data.frame are only represented by clumps under 9 pixels?
#   str(df_clump$value[which(df_clump$count <= 9)]) # which values do these correspond to?
#   excludeID <- df_clump$value[which(df_clump$count <= 9)] # put these into a vector of clump ID's to be removed
#
#   df_rs2 <- masked_clump # make a new raster to be sieved
#
#   df_rs2[df_rs2[,,1] %in% excludeID] <- NA # assign NA to all clumps whose IDs are found in excludeID
#
#   # Convert from Raster to Polygon
#   df_pol <- as(df_rs2,  "SpatialPolygonsDataFrame")
#   df_pol$layer <- seq(1, length(df_pol))
#   df_pol <- sp::spTransform(df_pol, longlat)
#
#   # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
#   df_pol_sf <- sf::st_as_sf(df_pol) %>%
#     dplyr::select(layer) %>%
#     dplyr::summarise(total_layer = sum(layer, do_union = TRUE))
# }
