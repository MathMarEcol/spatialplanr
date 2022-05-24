
#' Code to reduce AquaMaps to our requested species and area.
#'
#' @param PlanUnits Planning Units as an `sf` object
#' @param MinD The minimum depth for AquaMaps data
#' @param MaxD The maximum depth for AquaMaps data
#' @param Direc The directory where the MME data is being stored. If not specified, the default location is assumed.
#'
#' @return An `sf` object of AquaMaps data
#' @export
#'
#' @examples
#' @importFrom rlang .data
SpatPlan_Get_AquaMaps <- function(PlanUnits,
                                  MinD = 0,
                                  MaxD = 200, # Do epipelagic by default
                                  Direc = file.path("~", "SpatPlan_Data")){ # No longitundinal limits by default


  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  species <- readr::read_rds(file.path(Direc, "AquaMaps","AquaMaps_SpeciesInfoFB.rds"))  # The species info

  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  PUextent <- PlanUnits %>% # Get the extent for AquaMaps from the Bndry extent
    sf::st_transform(crs = longlat) %>% # Must be long/lat for AquaMaps
    sf::st_bbox() # get sf extent

  ex_sf <- PUextent + c(-1, -1, 1, 1) # Pad the limits by 1 degree

  # Filter the species list to only include those layers with data in the range of our PUs
  spp <- species %>%
    dplyr::filter(.data$MinLon <= ex_sf$xmax & .data$MaxLon >= ex_sf$xmin) %>%
    dplyr::filter(.data$MinLat <= ex_sf$ymax & .data$MaxLat >= ex_sf$ymin) %>%
    dplyr::filter(.data$DepthMin <= MaxD & .data$DepthMax >= MinD)

  # stars code to subset by data by our species list and crop area to the region of PlanUnits
  AquaMaps_sf <- stars::read_stars(file.path(Direc, "AquaMaps","AquaMaps.tif"), proxy = TRUE) # Load

  # Check that the species list and the tif species match up
  if (all.equal(stars::st_get_dimension_values(AquaMaps_sf, "band"), species$SpeciesID) != TRUE){
    stop("Species lists don't match up in SpatPlan_Get_AquaMaps.R")
  }

  AquaMaps_sf <- AquaMaps_sf %>%
    SpatPlan_Crop_AQM(spp, ex_sf) %>%
    sf::st_transform(sf::st_crs(PlanUnits)) %>%  # Transform to PU units
    sf::st_interpolate_aw(PlanUnits, extensive = FALSE) %>%
    dplyr::mutate(cellID = PlanUnits$cellID) %>% # We lose cellID in interpolate_aw
    dplyr::relocate(.data$cellID, tidyselect::everything()) # Move to the start

  # Get names of columns with data in them
  nm <- AquaMaps_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::select(which(!colSums(., na.rm = TRUE) %in% 0)) %>%
    names()

  # Remove zero columns now the cutoff has been applied
  AquaMaps_sf <- AquaMaps_sf %>%
    dplyr::select(tidyselect::all_of(nm))

  return(AquaMaps_sf)
}
