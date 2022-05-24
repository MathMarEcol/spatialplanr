
#' Convert a region to Pacific-Centred Robinson Projection.
#'
#' @param df An 'sf' object in longlat space
#' @param buff The buffer to use for
#'
#' @return An `sf` object coverted to Pacific-Robinson projection
#'
#' @examples
SpatPlan_Convert_PacificRobinson <- function(df, buff = 0){

  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match
  # that of world

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
    sf::st_difference(polygon) %>%
    sf::st_transform(crs = rob_pacific) # Perform transformation on modified version of world dataset

  # notice that there is a line in the middle of Antarctica. This is because we have
  # split the map after reprojection. We need to fix this:
  bbox <-  sf::st_bbox(df_robinson)
  bbox[c(1,3)]  <-  c(-1e-5, 1e-5)
  polygon_rob <- sf::st_as_sfc(bbox)

  crosses <- df_robinson %>%
    sf::st_intersects(polygon_rob) %>%
    sapply(length) %>%
    as.logical %>%
    which

  # Adding buffer
  df_robinson[crosses,] %>%
    df_robinson[crosses,] %>%
    sf::st_buffer(buff)

  return(df_robinson)

}
