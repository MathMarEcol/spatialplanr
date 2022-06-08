utils::globalVariables("where")



#' Function for creating polygon
#'
#' @param x A named vector of lon/lat coordinates from which to make an `sf` polygon
#' @param cCRS The CRS to use for the polygon
#'
#' @return An `sf` object for the polygon
#' @export
#'
#' @examples
SpatPlan_Create_Polygon <- function(x, cCRS){
  x <- x %>%
    as.matrix() %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sf::st_transform(crs = cCRS)
}


#' A function to fix incorrect column types from fishbase and sealifebase
#'
#' A function to fix incorrect column types from fishbase and sealifebase.
#' This may be able to be removed. These errors should be fixed in a future FB version.
#'
#' @param df The species dataframe
#' @param datab Which server to use "fishbase" (default) or "sealifebase"
#'
#' @return The species dataframe with names corrected.
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
#' @return A dataframe with corrected species names
#' @export
#'
#' @examples
#' SpatPlan_validate_FBNAs(data.frame(Species = "Thunnus maccoyii"))
#' @importFrom rlang .data
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
      out2 <- out[which(stringr::str_detect(out, spp$Species[a]))]
      spp$valid[a] <- TRUE
      if (length(out2) == 1){ # Name existed in the original
        spp$ValidSpecies[a] <- out2
      } else {
        spp$ValidSpecies[a] <- out[1] # Guess at the first one.
      }
    }
  }

  spp <- spp %>%
    dplyr::rename(OrigSpecies = .data$Species)
}


#' Crop AquaMaps Data
#'
#' @param df The AquaMaps `stars` object from `SpatPlan_Get_AquaMaps`
#' @param spp A character vector of species
#' @param extent An `sf` object from which to extract the extent
#'
#' @return An `sf` object cropped to `extent`
#'
#' @examples
#' @importFrom rlang .data
SpatPlan_Crop_AQM <- function(df, spp, extent){

  cropped <- df %>%
    sf::st_crop(extent, crop = TRUE) %>%  # TODO replace ex_sf with a polygon to deal with EEZ or coastal areas
    stars:::slice.stars(along = "band", index = spp$SpeciesIDNum) %>% # indexes rows based on SpeciesIDNum
    stars::st_as_stars() %>% # loads it into memory
    stars::st_set_dimensions("band", values = spp$longnames) %>%
    sf::st_as_sf(na.rm = FALSE, as_points = FALSE, merge = FALSE)

  rs <- cropped %>%
    sf::st_drop_geometry() %>%
    is.na() %>%
    rowSums()

  nc <- ncol(cropped) - 1 # Number of cols not including geometry

  cropped <- cropped %>%
    dplyr::filter({rs == nc} == FALSE) # Remove Rows with all NAs (except geometry)

  # Removed this code so I could get rid of . for RMD checks
  # cropped <- df %>%
  #   sf::st_crop(extent, crop = TRUE) %>%  # TODO replace ex_sf with a polygon to deal with EEZ or coastal areas
  #   stars:::slice.stars(along = "band", index = spp$SpeciesIDNum) %>% # indexes rows based on SpeciesIDNum
  #   stars::st_as_stars() %>% # loads it into memory
  #   stars::st_set_dimensions("band", values = spp$longnames) %>%
  #   sf::st_as_sf(na.rm = FALSE, as_points = FALSE, merge = FALSE) %>%
  #   dplyr::filter(sf::st_drop_geometry(.) %>%
  #                   is.na(.) %>%
  #                   {rowSums(.) == ncol(.)} == FALSE) # Remove Rows with all NAs (except geometry)

  return(cropped)
}


#' Remove NAs from spatial data using nearest neighbour
#'
#' @param df An `sf` dataframe
#' @param vari Variable to remove NAs from
#'
#' @return An `sf` object with NAs replaced with the nearest neighbour
#' @export
#'
#' @examples
#' @importFrom rlang .data
#' @importFrom rlang :=
SpatPlan_Replace_NAs <- function(df, vari){
  if (sum(is.na(dplyr::pull(df, !!rlang::sym(vari)))) > 0){ # Check if there are NAs

    gp <- df %>%
      dplyr::mutate(isna = is.na(!!rlang::sym(vari)))
    gp <- split(gp, f = as.factor(gp$isna))

    # Switch to sf. Much faster 7/6/22
    # d <- nngeo::st_nn(gp$`TRUE`, gp$`FALSE`) %>% # Get nearest neighbour
    #   unlist()

    d <- sf::st_nearest_feature(gp$`TRUE`, gp$`FALSE`)

    gp$`TRUE` <- gp$`TRUE` %>%
      dplyr::mutate(!!rlang::sym(vari) := dplyr::pull(gp$`FALSE`, !!rlang::sym(vari))[d])

    df <- rbind(gp$`FALSE`, gp$`TRUE`) %>%
      dplyr::select(-.data$isna) %>%
      dplyr::arrange(.data$cellID)

  }
  return(df)
}



#' Substitute numbers for all_names in regionalisations
#'
#' @param dat `sf` data frame with one column of numeric/integer corresponding to `nam`
#' @param nam character vector of names corresponding to numeric column of dat
#'
#' @return An `sf` dataframe with numeric regionalisations substituted for category names
#' @export
#'
#' @examples
#' @importFrom rlang :=
SpatPlan_Match_Names <- function(dat, nam){
  col_name = stringr::str_subset(colnames(dat), "geometry", negate = TRUE)

  out <- dat %>%
    dplyr::mutate(!!col_name := nam[!!rlang::sym(col_name)]) # Apply categories to data
}



#' Scale spatial layers to between 0 and 1
#'
#' @param dat `sf` dataframe
#' @param col_name Name of the column to scale
#'
#' @return `sf` dataframe
#' @export
#'
#' @examples
#' @importFrom rlang :=
SpatPlan_Scale_01 <- function(dat, col_name){

  mx  <- max(dplyr::pull(dat, !!rlang::sym(col_name)), na.rm = TRUE) # Get max probability

  if (mx > 100){
    divi <- 1000
  } else if(mx > 10){
    divi <- 100
  } else if(mx > 1){
    divi <- 10
  } else if(mx < 1){
    divi <- 1 # Do nothing
  }

  dat <- dat %>%
    dplyr::mutate(!!col_name := !!rlang::sym(col_name)/divi)

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



# Create one polygon that we can use to populate with PUs
#
# SpatPlan_Create_SinglePolygon <- function (df, res){
#
#   # Creating a empty raster
#   rs <- raster::raster(ncol = 360*(1/res), nrow = 180*(1/res))
#   rs[] <- 1:raster::ncell(rs)
#   raster::crs(rs) <- sf::st_crs(df) # Make raster crs the same as the sf object.
#
#   # Fasterize the land object
#   df_rs <- fasterize::fasterize(df, rs)
#
#   pol <- stars::as(df_rs,  "SpatialPolygonsDataFrame")
#   pol$layer <- seq(1, length(pol))
#
#   # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
#   pol_sf <- sf::st_as_sf(pol) %>%
#     dplyr::select(.data$layer) %>%
#     dplyr::summarise(total_layer = sum(.data$layer, do_union = TRUE))
# }



# Ensure all features are in the same order.
#
# @param df sf object to sort by Lon and Lat
#
# @return
# @export
#
# @examples
# SpatPlan_ArrangeFeatures <- function(df){
#
#   # Sort rows to ensure all features are in the same order.
#   xy <- sf::st_coordinates(sf::st_centroid(df))
#   df <- df[order(xy[,"X"], xy[,"Y"]),]
#
#   df <- df %>%
#     dplyr::mutate(cellID = dplyr::row_number())
#
# }

