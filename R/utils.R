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
splnr_create_polygon <- function(x, cCRS){
  x <- x %>%
    as.matrix() %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sf::st_transform(crs = cCRS)
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
splnr_replace_NAs <- function(df, vari){
  if (sum(is.na(dplyr::pull(df, !!rlang::sym(vari)))) > 0){ # Check if there are NAs

    gp <- df %>%
      dplyr::mutate(isna = is.na(!!rlang::sym(vari)))
    gp <- split(gp, f = as.factor(gp$isna))

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
splnr_match_names <- function(dat, nam){
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
splnr_scale_01 <- function(dat, col_name){

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



#
#
#' Convert a world Robinson sf object to a Pacific-centred one
#'
#' Written by Jason D. Everett
#' UQ/CSIRO/UNSW
#' Last edited 8th Sept 2021
#'
#' @param df An sf dataframe
#' @param buff The buffer too apply to features that cross after merge
#'
#' @return An sf object in the Robinson projection
#' @export
#'
#' @examples
splnr_convert2PacificRobinson <- function(df, buff = 0){
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match
  # that of world
  # Adapted from here:
  # https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2

  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  polygon <- sf::st_polygon(x = list(rbind(c(-0.0001, 90),
                                       c(0, 90),
                                       c(0, -90),
                                       c(-0.0001, -90),
                                       c(-0.0001, 90)))) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(longlat)

  # Modify world dataset to remove overlapping portions with world's polygons
  df_robinson <- df %>%
    sf::st_make_valid() %>% # Just in case....
    sf::st_difference(polygon) %>%
    sf::st_transform(crs = rob_pacific) # Perform transformation on modified version of world dataset
  rm(polygon)

  # # notice that there is a line in the middle of Antarctica. This is because we have
  # # split the map after reprojection. We need to fix this:
  bbox <-  sf::st_bbox(df_robinson)
  bbox[c(1,3)] <- c(-1e-5, 1e-5)
  polygon_rob <- sf::st_as_sfc(bbox)

  crosses <- df_robinson %>%
    sf::st_intersects(polygon_rob) %>%
    sapply(length) %>%
    as.logical %>%
    which

  # # Adding buffer 0
  df_robinson <- df_robinson[crosses,] %>%
    sf::st_buffer(buff)

}



# Create one polygon that we can use to populate with PUs
#
# splnr_Create_SinglePolygon <- function (df, res){
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




#' Ensure all features are in the same order.
#'
#' @param df An sf object to sort by Lon and Lat
#'
#' @return A sorted sf object with the additionl cellID column
#' @export
#'
#' @examples
splnr_arrangeFeatures <- function(df){

  # Sort rows to ensure all features are in the same order.
  xy <- sf::st_coordinates(sf::st_centroid(df))
  df <- df[order(xy[,"X"], xy[,"Y"]),]

  df <- df %>%
    dplyr::mutate(cellID = dplyr::row_number())

}

