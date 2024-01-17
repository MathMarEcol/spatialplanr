#' Function to compute distances to nearest coastline for each centroid of each planning unit in the 'sf' object provided.
#'
#' The code takes a sf object and return it updated with a new coastDistance column.
#' The output inherits the crs from this sf object so ensure it is in the correct projection for your needs
#'
#' Written by Kristine Buenafe
#' Written: March/April 2023
#' Modified by Kilian Barreiro
#' Updated: December 2023
#'
#' @param dat_sf An sf object.
#' @param custom_coast An sf coastline object (optional)
#'
#' @return An `sf` object with distances to the nearest coast
#' @export
#'
#' @examples
#' crs <- sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
#' bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 3, ymax = 3))
#' grid <- sf::st_make_grid(bbox, n = c(3, 3), what = "polygons")
#' grid <- sf::st_sf(geometry = grid) %>%
#'   sf::st_set_crs(crs)
#' splnr_get_distCoast(grid)
splnr_get_distCoast <- function(dat_sf, custom_coast = NULL) {

  # Class object check
  if (!inherits(dat_sf, "sf")) {
    stop("Input data should be an 'sf' spatial object.")
  }

  # CRS check
  if (is.null(sf::st_crs(dat_sf))) {
    stop("The sf spatial object must have a defined CRS.")
  }

  # Load coast
  if (is.null(custom_coast)) {
    coast <- rnaturalearth::ne_coastline(scale = 'medium') %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = sf::st_crs(dat_sf))
  } else {
    coast <- custom_coast %>%
      sf::st_transform(crs = sf::st_crs(dat_sf))
  }

  # Convert grid to points (centroids)
  grid_centroid <- sf::st_centroid(dat_sf)

  # Get distance matrix
  dist_mat <- sf::st_distance(grid_centroid, coast) %>%
    units::set_units("km") %>% # Convert to km
    units::drop_units() # Remove units (include in header below)

  # Find min distance for each row and convert to km.
  dat_sf$coastDistance_km <- do.call(pmin, as.data.frame(dist_mat))

  return(dat_sf)
}


