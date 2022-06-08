#' Get fisheries cost layer.
#'
#' @param PlanUnits Planning Units as an `sf` object
#' @param group The group of fish for the analysis. At the moment the only options are to split by "pelagic" or "all"
#' @param Direc The directory where the MME data is being stored. If not specified, the default location is assumed.
#'
#' @return A `sf` dataframe of fisheries cost for each planning unit.
#' @export
#'
#' @examples
#' @importFrom rlang .data
SpatPlan_Get_FishCost <- function(PlanUnits,
                                  group = "all",
                                  Direc = file.path("~", "SpatPlan_Data")){

  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  # TODO Add some filtering ability to get the cost only for specific groups
  # Calling the cost layer

  if(group == "all"){
    call_cost <- terra::rast(file.path(Direc, "Cost","Cost_Raster_Sum.grd")) %>%
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = FALSE) %>% # Convert to polygon data
      sf::st_as_sf() # Convert to sf
  } else if(group == "pelagic"){
    call_cost <- terra::rast(file.path(Direc, "Cost", "Cost_RasterStack_byFunctionalGroup.grd")) %>% # data by functional group
      terra::subset(c(14, 16, 19)) %>% # small, medium, and large pelagics
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = FALSE) %>% # Convert to polygon data
      sf::st_as_sf() # Convert to sf

    # Replace all NAs with the smallest value
    small_value <- list() #empty list
    for(i in 2:ncol(call_cost)-1) {
      small_value[i] <- (min(call_cost[[i]], na.rm = T))/2
    }
    small_value <- unlist(small_value)

    call_cost <- call_cost %>%
      dplyr::mutate(medium = ifelse(is.na(.data$MediumPelagics30_89Cm), small_value[1], .data$MediumPelagics30_89Cm),
                    small = ifelse(is.na(.data$SmallPelagics30Cm), small_value[2], .data$SmallPelagics30Cm),
                    large = ifelse(is.na(.data$LargePelagics90Cm), small_value[3], .data$LargePelagics90Cm)) %>%
      dplyr::select(.data$medium, .data$small, .data$large, .data$geometry) %>%
      dplyr::mutate(layer =.data$ medium + .data$small + .data$large) %>% # get the sum
      dplyr::select(.data$layer, .data$geometry) # only select sum and geometry
  }


  Cost <- call_cost %>%
    sf::st_transform(sf::st_crs(PlanUnits)) %>% # Transform to CRS of PlanUnits
    sf::st_interpolate_aw(PlanUnits, extensive = FALSE) %>% ## intersect with PlanUnits
    dplyr::rename(Cost = .data$layer)
}

