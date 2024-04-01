#' Function to create square or heaxagonal planning units for your area of interest.
#'
#' The code takes the bbox so the limits are the most important.
#' The output inherits the crs from this sf object so ensure it is in the correct projection for your needs
#' The code assumes that any Planning Unit whose centroids is over land will be removed. This approximates > 50% of the PU is landward.
#'
#' Some example options for hexagonal `PU_size`
#' PU_size <- 5240 # km2 (~1 deg at equator)
#' PU_size <- 2620 # km2 (~0.5 deg at equator)
#' PU_size <- 669.9 # km2 (~0.25 deg at equator)
#'
#' Written by Jason Everett (UQ/UNSW/CSIRO)
#' Written: 15 December 2020
#' Updated: 8th April 2022
#'
#' @param Bndry An sf polygon object which outlines the limits of the study area.
#' @param InnerB An sf multipolygon object which contains all the areas (ie land) that you wish to remove from the grid.
#' @param CellArea The area in km you wish your resultant Planning Units to be.
#' @param Shape Hexagon or Square
#' @param inverse To invert the `InnerB` and keep the areas within the InnerB
#'
#' @return An `sf` object of planning units
#' @export
#'
#' @examples
#' dat_bndry_moll <- dat_bndry %>% sf::st_transform("ESRI:54009")
#' landmass <- rnaturalearth::ne_countries(returnclass = "sf") %>% sf::st_transform("ESRI:54009")
#' PUs <- splnr_get_planningUnits(dat_bndry_moll, landmass, CellArea = 10000, Shape = "hexagon")
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = dat_bndry_moll, colour = "blue", fill = NA) +
#'   ggplot2::geom_sf(data = PUs, colour = "black", fill = NA)
splnr_get_planningUnits <- function(Bndry,
                                    InnerB,
                                    CellArea = 1000,
                                    Shape = "hexagon",
                                    inverse = FALSE) {

  assertthat::assert_that(CellArea > 0, msg = "CellArea must be greater than zero.")
  assertthat::assert_that(Shape %in% c("hexagon", "square", "Hexagon", "Square"),
                          msg = "Shape must be 'hexagon' or 'square'.")
  assertthat::assert_that(inherits(InnerB, c("sf", "sfc", "sfg")), msg = "InnerB must be an object of class 'sf', 'sfc', or 'sfg'.")

  if (Shape %in% c("hexagon", "Hexagon")) {
    sq <- FALSE
    diameter <- 2 * sqrt((CellArea * 1e6) / ((3 * sqrt(3) / 2))) * sqrt(3) / 2 # Diameter in m's
  }

  if (Shape %in% c("square", "Square")) {
    sq <- TRUE
    diameter <- sqrt(CellArea * 1e6) # Diameter in m's
  }

  # First create planning units for the whole region
  PUs <- sf::st_make_grid(Bndry,
    square = sq,
    cellsize = c(diameter, diameter),
    what = "polygons"
  ) %>%
    sf::st_sf()

  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- sf::st_centroid(PUs) %>%
    sf::st_intersects(Bndry) %>%
    lengths() > 0 # Get logical vector instead of sparse geometry binary

  PUs <- PUs[logi_Reg, ] # Get TRUE

  # Second, get all the pu's with < 50 % area on land (approximated from the centroid)
  logi_Ocean <- sf::st_centroid(PUs) %>%
    sf::st_intersects(InnerB) %>%
    lengths() > 0 # Get logical vector instead of sparse geometry binary

  if (inverse == FALSE) {
    PUs <- PUs[!logi_Ocean, ] # Get FALSE
  } else {
    PUs <- PUs[logi_Ocean == TRUE, ] # Get TRUE
  }

  PUs <- PUs %>%
    dplyr::mutate(cellID = dplyr::row_number()) # Add a cell ID reference

  return(PUs)
}
