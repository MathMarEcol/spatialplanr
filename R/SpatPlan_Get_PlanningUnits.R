
#' Function to create square or heaxagonal planning units for your area of interest.
#'
#' The code takes the bbox so the limits are the most important.
#' The output inherits the crs from this sf object so ensure it is in the correct projection for your needs
#' The code assumes that any Planning Unit whose centroids is over land will be removed. This approximates > 50% of the PU is landward.
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
#' @return
#' @export
#'
#' @examples
SpatPlan_Get_PlanningUnits <- function(Bndry,
                                       InnerB,
                                       CellArea,
                                       Shape = "hexagon",
                                       inverse = FALSE){

  if(Shape %in% c("hexagon", "Hexagon")){
    sq <- FALSE
    diameter <- 2 * sqrt((CellArea*1e6)/((3*sqrt(3)/2))) * sqrt(3)/2 # Diameter in m's
  }

  if(Shape %in% c("square", "Square")){
    sq <- TRUE
    diameter <- sqrt(CellArea*1e6) # Diameter in m's
  }

  # First create planning units for the whole region
  PUs <- sf::st_make_grid(Bndry,
                      square = sq,
                      cellsize = c(diameter, diameter),
                      what = "polygons") %>%
    sf::st_sf()

  # Check cell size worked ok.
  print(paste0("Range of cellsize are ",
               round(as.numeric(range(units::set_units(sf::st_area(PUs), "km^2")))[1])," km2 to ",
               round(as.numeric(range(units::set_units(sf::st_area(PUs), "km^2")))[2])," km2")) # Check area

  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- sf::st_centroid(PUs) %>%
    sf::st_intersects(Bndry) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary

  PUs <- PUs[logi_Reg, ] # Get TRUE

  # Second, get all the pu's with < 50 % area on land (approximated from the centroid)
  logi_Ocean <- sf::st_centroid(PUs) %>%
    sf::st_intersects(InnerB) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary

  if(inverse == FALSE){
    PUs <- PUs[!logi_Ocean, ] # Get FALSE
  }
  else {
    PUs <- PUs[logi_Ocean==TRUE, ] # Get TRUE
  }

  PUs <- PUs %>%
    dplyr::mutate(cellID = dplyr::row_number()) # Add a cell ID reference

  return(PUs)
}
