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
#'
#' @return An `sf` object with distances to the nearest coast
#' @export
#'
#' @examples
#' # Example 1
#' crs <- "ESRI:54009" #Mollweide projection
#' grid <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))))) %>%
#'   sf::st_set_crs(crs) %>%
#'   sf::st_cast("POLYGON")
#' splnr_get_distCoast(grid)
#'
#' # Example 2
#' crs_2 <- sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
#' bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 3, ymax = 3))
#' grid_2 <- sf::st_make_grid(bbox, n = c(3, 3), what = "polygons")
#' grid_2 <- sf::st_sf(geometry = grid_2) %>%
#'   sf::st_set_crs(crs_2)
#' splnr_get_distCoast(grid_2)

splnr_get_distCoast <- function(dat_sf) {

  # Class object check
  if (!inherits(dat_sf, "sf")) {
    stop("Input 'grid' should be an 'sf' spatial object.")
  }

  # CRS check
  if (is.null(st_crs(dat_sf))) {
    stop("The grid object must have a defined CRS.")
  }

  # Load coast
  coast <- rnaturalearth::ne_coastline(scale = 'large') %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = st_crs(dat_sf))

  # Convert grid to points (centroids)
  grid_centroid <- sf::st_centroid(dat_sf)

  # Find the nearest coast for all the grid cells centroids
  nearest <- sf::st_nearest_feature(grid_centroid, coast)

  # Assign distances to dat_sf directly
  dat_sf$coastDistance <- sf::st_distance(grid_centroid, coast[nearest, ])

  return(dat_sf)
}

