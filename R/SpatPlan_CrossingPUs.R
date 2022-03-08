

#' Cross raw features (as prast) with Planning Units
#'
#' @param file
#' @param PUs
#' @param binary
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Convert_Rast2PUs <- function(file, # filename
                                      PUs, # Planning Units
                                      binary = FALSE){

  if (stringr::str_detect(class(file)[1], "SpatRaster") == TRUE){ # Is it a file or a raster

    out <- file %>%
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=TRUE) %>% # Convert to polygon data #TODO This will be slow. Probably best to use
      sf::st_as_sf() %>%  # Convert to sf
      sf::st_transform(sf::st_crs(PUs))  # Transform to correct CRS

    nm = stringr::str_subset(colnames(out), "geometry", negate = TRUE) # Don't return "geometry"

  } else {
    nm <- fs::path_ext_remove(basename(file)) # Strip out the junk and get the name

    out <- terra::rast(file) %>% # Load the file
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=TRUE) %>% # Convert to polygon data #TODO This will be slow. Probably best to use
      sf::st_as_sf() %>%  # Convert to sf
      sf::st_transform(sf::st_crs(PUs))  # Transform to correct CRS
  }


  if (any(stringr::str_detect(colnames(out), "layer"))){
    out <- out %>%
      dplyr::rename(!!nm := layer)
  }

  if (binary == TRUE) {

    out <- out %>%
      sf::st_contains(PUs, ., sparse = FALSE) %>%
      rowSums() %>%
      as.logical() %>%
      as.numeric() %>%
      tibble::as_tibble_col(column_name = nm) %>%
      dplyr::bind_cols(PUs) %>% # Loses sf definition
      sf::st_sf() # Re-add here

  } else if (binary == FALSE){

    out2 <- out %>%
      sf::st_interpolate_aw(PUs, extensive = FALSE, keep_NA = TRUE) %>%  ## intersect with PUs
      tibble::as_tibble() %>%
      dplyr::select(-.data$geometry) %>%
      dplyr::bind_cols(PUs) %>% # Loses sf definition
      sf::st_sf() # Re-add here

  }

  return(out)
}





#' Cross raw features (as polygons) with Planning Units
#'
#' At the moment this requires a sf object with only the geometry and 1 data column.
#'
#' It also assumes the feature has to overlap with the centroid of the PU. This means
#' some small features won't be included, but also stops us over representing the area of these small features.
#'
#' @param file A filename to load or a dataframe to work with.
#' @param PUs The planning units to be crossed with
#' @param binary Is the feature binary (TRUE) or continuous (FALSE)
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Convert_Poly2PUs <- function(file, # filename
                                      PUs, # Planning Units
                                      binary = FALSE){

  if (is.data.frame(file) == TRUE){
    dat <- file

    if (sf::st_crs(dat) != sf::st_crs(PUs)){
      dat <- dat %>%
        sf::st_transform(sf::st_crs(PUs)) %>% # Transform to correct CRS
        sf::st_make_valid()
    }

    nm = stringr::str_subset(colnames(dat), "geometry", negate = TRUE) # Don't return "geometry"
  } else {
    # Some code to load the file
  }

  if (binary == TRUE) {

    logi_overlap <- sf::st_centroid(PUs) %>% #Second, get all the pu's with centroid inside the polygons
      sf::st_within(dat, sparse = FALSE) %>%
      rowSums() %>%
      as.logical() %>%
      as.numeric()

    out <- PUs %>%
      dplyr::mutate(!!nm := logi_overlap) # %>%
    # fCheckNAs(nm) %>%
    # fArrangeFeatures() %>%
    # dplyr::select(c(geometry, cellID, !!vari))
  } else if (binary == FALSE) {

    out <- PUs %>%
      dplyr::mutate(!!nm := exactextractr::exact_extract(dat, PUs, 'mean'))

  }

  return(out)

}
