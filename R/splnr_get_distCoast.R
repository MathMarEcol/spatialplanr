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
#' @param res Allow user to choose resolution (`small`, `medium`, `large`) of `rnaturalearth` data used for coastline.
#'
#' @return An `sf` object with distances to the nearest coast
#' @export
#'
#' @examples
#' bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 3, ymax = 3))
#' grid <- sf::st_make_grid(bbox, n = c(3, 3), what = "polygons")
#' grid <- sf::st_sf(geometry = grid) %>%
#'   sf::st_set_crs("EPSG:4326")
#' splnr_get_distCoast(grid)
#'
#' cCRS <- "ESRI:54009"
#'
#' Bndry <- splnr_get_boundary(Limits = "Coral Sea",
#'                             Type = "Oceans",
#'                             cCRS = cCRS)
#'
#' landmass <- rnaturalearth::ne_countries(
#'   scale = "medium",
#'   returnclass = "sf"
#' ) %>%
#'   sf::st_transform(cCRS)
#'
# dat_sf <- spatialgridr::get_grid(boundary = Bndry,
#                                  projection_crs = cCRS,
#                                  option = "sf_hex",
#                                  resolution = 10000,
#                                  sf_method = "centroid") %>%
#   splnr_get_distCoast(res = "medium")

splnr_get_distCoast <- function(dat_sf, custom_coast = NULL, res = NULL) {

  assertthat::assert_that(
    inherits(dat_sf, "sf"),
    !is.null(sf::st_crs(dat_sf)),
    is.null(custom_coast) || inherits(custom_coast, "sf"),
    is.null(res) || res %in% c("small", "medium", "large")
  )

  # Load coast
  if (is.null(custom_coast)) {
    if (is.null(res)) {res <- "medium"}
    coast <- rnaturalearth::ne_coastline(scale = res) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = sf::st_crs(dat_sf))
  } else {
    coast <- custom_coast %>%
      sf::st_transform(crs = sf::st_crs(dat_sf))
  }

  # Convert grid to points (centroids)
  grid_centroid <- sf::st_centroid(sf::st_geometry(dat_sf))

  # Get distance matrix
  dist_mat <- sf::st_distance(grid_centroid, coast) %>%
    units::set_units("km") %>% # Convert to km
    units::drop_units() # Remove units (include in header below)

  # Find min distance for each row and convert to km.
  dat_sf$coastDistance_km <- do.call(pmin, as.data.frame(dist_mat))

  return(dat_sf)
}


