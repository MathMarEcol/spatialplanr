#' Function to compute distances to nearest coastline for each centroid of each planning unit in the grid provided.
#'
#' The code takes a sf object and return it updated with a new coastDistance column.
#' The output inherits the crs from this sf object so ensure it is in the correct projection for your needs
#'
#' Written by Kristine Buenafe 
#' Written: March/April 2023
#' Modified by Kilian Barreiro
#' Updated: December 2023
#'
#' @param grid An sf polygon object which contains outlines the limits of the study area.
#'
#' @return An `sf` object with distances to the nearest coast
#' @export
#' 
#' @example 
#' cCRS <- "ESRI:54009"
#' grid <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))))) %>% 
#' st_set_crs(cCRS) %>%
#' st_cast("POLYGON") %>%
#' mutate(cellID = row_number())
#' updatedGrid <- splnr_get_nearestDist2Coast(grid)
splnr_get_nearestDist2Coast <- function(grid) {
  
  # Class object check
  if (!inherits(grid, "sf")) {
    stop("Input 'grid' should be an 'sf' spatial object.")
  }
  
  # CRS check 
  if (is.null(st_crs(grid))) {
    stop("The grid object must have a defined CRS.")
  }
  
  # Load coast
  coast <- rnaturalearth::ne_coastline(scale = 'large') %>% 
    sf::st_as_sf() %>%
    sf::st_transform(crs = st_crs(grid))
  
  # Convert grid to points (centroids)
  grid_centroid <- grid %>% 
    sf::st_transform(crs = st_crs(grid)) %>% # Convert to same CRS than provided grid
    sf::st_centroid()
  
  # Find the nearest coast for all the grid cells centroids
  nearest <- sf::st_nearest_feature(grid_centroid, coast)
  
  # Get the distance and populate an empty vector with it
  dists <- purrr::map_dbl(1:nrow(grid_centroid), function(i) {
    sf::st_distance(grid_centroid[i, ], coast[nearest[i], ])
  })
  
  # Add that in the grid_centroid df
  grid_centroid$coastDistance <- dists
  
  grid_out <- dplyr::left_join(grid %>% 
                                 dplyr::as_tibble(), 
                               grid_centroid %>%
                                 dplyr::as_tibble() %>% 
                                 dplyr::select(-geometry)) %>%  # join that with the grid cells
    
    dplyr::select(cellID, coastDistance, geometry)
  
  # saveRDS(grid_out, here::here(output_dir, "coast_distance.rds")) # save the coast distance df
  
  return(grid_out)
}
