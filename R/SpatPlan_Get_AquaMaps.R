
#' Code to reduce AquaMaps to our requested species and area.
#'
#' @param PlanUnits
#' @param cCRS
#' @param MinD
#' @param MaxD
#' @param CutOff
#' @param limits
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Get_AquaMaps <- function(PlanUnits,
                                  cCRS = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                                  MinD = 0,
                                  MaxD = 200, # Do epipelagic by default
                                  CutOff = 0.5,
                                  limits = NA,
                                  Direc = file.path("~", "SpatPlan_Data")){ # No longitundinal limits by default

  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  species <- readr::read_rds(file.path(Direc, "AquaMaps","AquaMaps_SpeciesInfoFB.rds")) %>%  # The species info
    dplyr::mutate(longnames = stringr::str_replace_all(longnames, " ", "."),
                  longnames = stringr::str_replace_all(longnames, "-", ".")) #TODO This can be removed then we have rerun AquaMaps

  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  temp <- PlanUnits %>% # Get the extent for AquaMaps from the Bndry extent
    sf::st_transform(crs = longlat) %>% # Must be long/lat for AquaMaps
    sf::st_bbox() # get sf extent

  # If Pacific-centered, change the way of species filtering species and defining extent
  if ((cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") && !is.na(extent)){
    ex_sf <- temp + c(1, -1, -1, 1) # Pad the limits by 1 degree
    # ex_sf <- temp + c(5, -5, -5, 5) # Pad the limits by 5 degrees
    # ex_sf <- temp + c(10, -10, -10, 10) # Pad the limits by 10 degrees

    # Filter species
    spp <- species %>%
      dplyr::filter((MaxLon >= ex_sf$xmax & MaxLon <= 180) & (MinLon <= ex_sf$xmin & MinLon >= -180)) %>%
      dplyr::filter(MinLat <= ex_sf$ymax & MaxLat >= ex_sf$ymin) %>%
      dplyr::filter(DepthMin <= MaxD & DepthMax >= MinD)

    # Create two extents from ex_sf
    ex_sf1 <- ex_sf
    ex_sf1[3] = limits[1] # change to western limit
    ex_sf1[1] = -180

    ex_sf2 <- ex_sf
    ex_sf2[1] = limits[2] # change to eastern limit
    ex_sf2[3] = 180
  } else {
    ex_sf <- temp + c(-1, -1, 1, 1) # Pad the limits by 1 degree
    # ex_sf <- ex_sf + c(-10, -10, 10, 10) # Pad the limits by 10 degrees
    print(ex_sf)

    # Filter the species list to only include those layers with data in the range of our PUs

    spp <- species %>%
      dplyr::filter(MinLon <= ex_sf$xmax & MaxLon >= ex_sf$xmin) %>%
      dplyr::filter(MinLat <= ex_sf$ymax & MaxLat >= ex_sf$ymin) %>%
      dplyr::filter(DepthMin <= MaxD & DepthMax >= MinD)

  }

  # stars code to subset by data by our species list and crop area to the region of PlanUnits
  AquaMaps_sf <- stars::read_stars(file.path(Direc, "AquaMaps","AquaMaps.tif"), proxy = TRUE) # Load

  # Check that the species list and the tif species match up
  if (all.equal(stars::st_get_dimension_values(AquaMaps_sf, "band"), species$SpeciesID) != TRUE){
    stop("Species lists don't match up in SpatPlan_Get_AquaMaps.R")
  }



  # If Pacific-centered, change the way of cropping.
  if ((cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") && !is.na(limits)){
    AquaMaps_temp1 <- AquaMaps_sf %>%
      SpatPlan_Crop_AQM(spp, ex_sf1) # Crops western half of the planning region
    AquaMaps_temp2 <- AquaMaps_sf %>%
      SpatPlan_Crop_AQM(spp, ex_sf2) # Crops eastern half of the planning region

    AquaMaps_sf <- dplyr::bind_rows(AquaMaps_temp1, AquaMaps_temp2)

    AquaMaps_sf <- AquaMaps_sf %>% # Intersects with cropped aquamaps data with planning region
      sf::st_interpolate_aw(PlanUnits, extensive = FALSE) %>%
      dplyr::mutate_at(vars(any_of(spp$longnames)),
                       ~ dplyr::case_when(. >= CutOff ~ 1,
                                          . <= CutOff ~ 0,
                                          is.na(.) ~ 0))
  }
  # Else, the default way of cropping
  else {
    AquaMaps_sf <- AquaMaps_sf %>%
      SpatPlan_Crop_AQM(spp, ex_sf) %>%
      sf::st_interpolate_aw(PlanUnits, extensive = FALSE) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::any_of(spp$longnames)),
                       ~ dplyr::case_when(. >= CutOff ~ 1,
                                          . <= CutOff ~ 0,
                                          is.na(.) ~ 0))
  }

  # Get names of columns with data in them
  nm <- AquaMaps_sf %>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry) %>%
    dplyr::select(which(!colSums(., na.rm=TRUE) %in% 0)) %>%
    names()

  # Remove zero columns now the cutoff has been applied
  AquaMaps_sf <- AquaMaps_sf %>%
    dplyr::select(all_of(nm))

  return(AquaMaps_sf)
}
