
# longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#

#
# Formerly fix_species_type
#' A function to fix incorrect column types from fishbase and sealifebase
#'
#' A function to fix incorrect column types from fishbase and sealifebase.
#' This may be able to be removed. These errors should be fixed in a future FB version.
#'
#' @param df The species dataframe
#' @param datab Which server to use "fishbase" (default) or "sealifebase"
#'
#' @return
#' @export
#'
#' @examples
 SpatPlan_fix_FBtype<- function(df, datab = "fishbase"){

  if(datab == "fishbase"){ # Need to convert type of different columns depending on database
    nm <- c("SpecCode", "DepthRangeShallow", "CommonLength", "CommonLengthF", "LongevityWildRef", "MaxLengthRef", "DangerousRef")
  } else if(datab == "sealifebase"){
    nm <- c("SpecCode", "SpeciesRefNo", "GenCode", "DepthRangeRef", "LongevityWildRef", "Weight")
  }

  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(nm), as.numeric)) # Convert `nm` variables to numeric
}


#' Code to loop through and try and validate names with fishbase and sealifebase
#'
#' @param spp A dataframe with the species to be checked.
#' @param datab  Which server to use "fishbase" (default) or "sealifebase"
#'
#' @return
#' @export
#'
#' @examples
#' SpatPlan_validate_FBNAs(data.frame(Species = "Thunnus maccoyii"))
SpatPlan_validate_FBNAs <- function(spp, datab = "fishbase"){
  spp <- spp %>%
    dplyr::mutate(ValidSpecies = NA, valid = FALSE)

  # For some reason we are getting multiple names back for some species. Check which ones....
  for (a in 1:dim(spp)[1]){
    out <- rfishbase::validate_names(spp$Species[a], server = datab)
    if (length(out) == 1 & is.na(out)){ # Maintain original name
      spp$ValidSpecies[a] <- spp$Species[a]
    } else if (length(out) == 1){
      spp$ValidSpecies[a] <- out[1]
      spp$valid[a] <- TRUE
    } else if (length(out) > 1) {
      # First check if any match the original name
      # Sometimes a species comes back as valid, with an alternative.
      # Here we check if the name was in fact in the output
      out2 <- out[which(stringr::str_detect(out,spp$Species[a]))]
      spp$valid[a] <- TRUE
      if (length(out2) == 1){ # Name existed in the original
        spp$ValidSpecies[a] <- out2
      } else {
        spp$ValidSpecies[a] <- out[1] # Guess at the first one.
      }
    }
  }

  spp <- spp %>%
    dplyr::rename(OrigSpecies = Species)
}



#' Create ONE BIG polygon that we can use to populate with PUs
#'
#' @param df
#' @param res Resolution of the raster in degrees
#'
#' @return
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
SpatPlan_Create_SinglePolygon <- function (df, res){

  # Creating a empty raster
  rs <- raster::raster(ncol = 360*(1/res), nrow = 180*(1/res))
  rs[] <- 1:raster::ncell(rs)
  raster::crs(rs) <- sf::st_crs(df) # Make raster crs the same as the sf object.

  # Fasterize the land object
  df_rs <- fasterize::fasterize(df, rs)

  pol <- as(df_rs,  "SpatialPolygonsDataFrame")
  pol$layer <- seq(1, length(pol))

  # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
  pol_sf <- sf::st_as_sf(pol) %>%
    dplyr::select(layer) %>%
    dplyr::summarise(total_layer = sum(layer, do_union = TRUE))
}

#' Ensure all features are in the same order.
#'
#' @param df sf object to sort by Lon and Lat
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_ArrangeFeatures <- function(df){

  # Sort rows to ensure all features are in the same order.
  xy <- sf::st_coordinates(sf::st_centroid(df))
  df <- df[order(xy[,"X"], xy[,"Y"]),]

  df <- df %>%
    dplyr::mutate(cellID = dplyr::row_number())

}

# # Convert a world Robinson sf object to a Pacific-centred one
# #
# # Written by Jason D. Everett
# # UQ/CSIRO/UNSW
# # Last edited 8th Sept 2021
# SpatPlan_Convert2PacificRobinson <- function(df, buff = 0){
#   # Define a long & slim polygon that overlaps the meridian line & set its CRS to match
#   # that of world
#   # Adapted from here:
#   # https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2
#
#   rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#   longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#
#   polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
#                                        c(0, 90),
#                                        c(0, -90),
#                                        c(-0.0001, -90),
#                                        c(-0.0001, 90)))) %>%
#     st_sfc() %>%
#     st_set_crs(longlat)
#
#   # Modify world dataset to remove overlapping portions with world's polygons
#   df_robinson <- df %>%
#     st_make_valid() %>% # Just in case....
#     st_difference(polygon) %>%
#     st_transform(crs = rob_pacific) # Perform transformation on modified version of world dataset
#   rm(polygon)
#
#   # # notice that there is a line in the middle of Antarctica. This is because we have
#   # # split the map after reprojection. We need to fix this:
#   bbox <-  st_bbox(df_robinson)
#   bbox[c(1,3)] <- c(-1e-5, 1e-5)
#   polygon_rob <- st_as_sfc(bbox)
#
#   crosses <- df_robinson %>%
#     st_intersects(polygon_rob) %>%
#     sapply(length) %>%
#     as.logical %>%
#     which
#
#   # # Adding buffer 0
#   df_robinson[crosses,] %<>%
#     st_buffer(buff)
#
# }


#' Crop AquaMaps Data
#'
#' @param extent
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Crop_AQM <- function(dat, spp, extent){
  cropped <- dat %>%
    sf::st_crop(extent, crop = TRUE) %>%  # TODO replace ex_sf with a polygon to deal with EEZ or coastal areas
    stars:::slice.stars(along = "band", index = spp$SpeciesIDNum) %>% # indexes rows based on SpeciesIDNum
    stars::st_as_stars() %>% # loads it into memory
    stars::st_set_dimensions("band", values = spp$longnames) %>%
    sf::st_as_sf(na.rm = FALSE, as_points = FALSE, merge = FALSE) %>%
    sf::st_transform(cCRS) # Transform to robinson
}
