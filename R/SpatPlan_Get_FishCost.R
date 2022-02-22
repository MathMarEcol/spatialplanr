#' Get fisheries cost layer.
#'
#' @param PUs
#' @param cCRS
#' @param group
#' @param Direc
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Get_FishCost <- function(PUs,
                                  cCRS,
                                  group = "all",
                                  Direc = file.path("~", "SpatPlan_Data")){

  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  # TODO Add some filtering ability to get the cost only for specific groups
  # Calling the cost layer

  if(group == "all"){
    call_cost <- terra::rast(file.path(Direc, "Cost","Cost_Raster_Sum.grd")) %>%
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      sf::st_as_sf() # Convert to sf
  }

  else if(group == "pelagic"){
    call_cost <- terra::rast(file.path(Direc, "Cost", "Cost_RasterStack_byFunctionalGroup.grd")) %>% # data by functional group
      terra::subset(., c(14, 16, 19)) %>% # small, medium, and large pelagics
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      sf::st_as_sf() # Convert to sf

    # Replace all NAs with the smallest value
    small_value <- list() #empty list
    for(i in 2:ncol(call_cost)-1) {
      small_value[i] <- (min(call_cost[[i]], na.rm = T))/2
    }
    small_value <- unlist(small_value)

    call_cost <- call_cost %>%
      dplyr::mutate(medium = ifelse(is.na(.data$MediumPelagics30_89Cm), small_value[1], data$MediumPelagics30_89Cm),
                    small = ifelse(is.na(data$SmallPelagics30Cm), small_value[2], data$SmallPelagics30Cm),
                    large = ifelse(is.na(data$LargePelagics90Cm), small_value[3], data$LargePelagics90Cm)) %>%
      dplyr::select(medium, small, large, geometry) %>%
      dplyr::mutate(layer = medium + small + large) %>% # get the sum
      dplyr::select(layer, geometry) # only select sum and geometry
  }

  # If Pacific-centered:
  if(cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
    temp_cost <- call_cost %>%
      sf::st_make_valid() %>%
      sf::st_transform(longlat)

    temp_cost <- temp_cost %>%
      SpatPlan_Convert2PacificRobinson()

    Cost <- temp_cost %>%
      sf::st_interpolate_aw(PUs, extensive = FALSE) %>%
      dplyr::rename(Cost = layer)

    return(Cost)
  }

  else{
    Cost <- call_cost %>%
      sf::st_transform(cCRS) %>% # Transform to robinson
      sf::st_interpolate_aw(PUs, extensive = FALSE) %>% ## intersect with PUs
      dplyr::rename(Cost = layer)

    return(Cost)
  }
}
