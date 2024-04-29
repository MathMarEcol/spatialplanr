#' The `get_gfwData` function recover the data of Global Fishing Watch and
#' returns it as a sf object.
#'
#'
#' The possibilities offered by this function are explained in `vignette("GlobalFishingWatch")`
#'
#' We have the same parameters than the `get_raster` function, plus `cCRS`
#' which is the crs for the sf_modification <br>
#' Different possible values can be combined and are : <br>
#' - `Time Range`, `Flag`, `Geartype`. <br>
#' __(A combination can be : c('Time Range','Geartype'), if you want to get__
#' __the sum of fishing hours per date and geartype, for example you want to__
#' __display the drifting longline fishing in a specific year)__ <br> <br>
#' __Notes :__ <br>
#' 1. For the moment we are limited to the EEZs of each region, but we can
#' potentially restrict the working area to specific MPAs. <br>
#' 2. Days indicated in the__ `start_date` __and__ `end_date` __variables are
#' included in the data recovery.
#'
#' The code takes several parameters described below and return an sf object
#' with gfw data aggregated or not (param compress)
#'
#' @param region Region studied (character) or a geojson shape to filter raster
#' @param start_date Start date (waited format : "%Y-%m-%d").
#' @param end_date End date (waited format : "%Y-%m-%d").
#' @param temp_res Temporal resolution ("daily","monthly","yearly").
#' @param spat_res Spatial resolution ("low" for 0.1 degree, "high" for 0.01 degree).
#' @param region_source source of the region ('eez','mpa', 'rfmo' or 'user_json')
#' @param key Token for GFW API (see details GlobalFishingWatch vignette).
#' @param cCRS The crs to which the sf will be returned (default = "EPSG:4326").
#' @param compress Binary operator to compress (aggregate) the data per coordinates (default = FALSE).
#'
#' @return An `sf` object with gfw data.
#' @export
#'
#' @examples
#' \dontrun{
#' gfw_data <- splnr_get_gfw('Australia', "2021-01-01", "2022-12-31", "yearly",
#'     cCRS = "ESRI:54009", compress = TRUE)
#'}
splnr_get_gfw <- function(region,
                          start_date,
                          end_date,
                          temp_res,
                          spat_res = "low",
                          region_source = "eez",
                          key = gfwr::gfw_auth(),
                          cCRS = "EPSG:4326",
                          compress = FALSE) {

  assertthat::assert_that(is.character(region),
                          inherits(start_date, "character") && !is.na(as.Date(start_date, "%Y-%m-%d")), #is.Date ?
                          inherits(end_date, "character") && !is.na(as.Date(end_date, "%Y-%m-%d")),
                          temp_res %in% c("daily", "monthly", "yearly"),
                          spat_res %in% c("low", "high"),
                          region_source %in% c('eez', 'mpa', 'rfmo', 'user_json'),
                          is.character(key),
                          is.character(cCRS),
                          is.logical(compress))

  if (region_source == "eez"){ # Only process eez. RFMO and geojson have bugs
    region_id <- gfwr::get_region_id(region_name = region, region_source = region_source, key = key)$id
  } else if (region_source == "rfmo"){
    region_id = region # gfwr retuns NULL for region ID due to a bug in as.numeric(ID)
  } else if (methods::is(region, "geojson")){
    region_id <- region # Use region as is
  }

  # Convert dates into Date objects
  start_date <- as.Date(start_date, format = "%Y-%m-%d")
  end_date <- as.Date(end_date, format = "%Y-%m-%d")

  # Function to obtain data for a specific date range
  get_data_for_range <- function(start_date, end_date, rid) {

    date_range <- paste(start_date, end_date, sep = ",")

    data <- gfwr::get_raster(
      spatial_resolution = spat_res,
      temporal_resolution = temp_res,
      group_by = 'flagAndGearType',
      date_range = date_range,
      region = rid,
      region_source = region_source,
      key = key)

    data <- data %>%
      dplyr::mutate(GFWregionID = rid,
                    GFWregion = region) %>%
      dplyr::rename(TimeRange = .data$`Time Range`,
                    VesselID = .data$`Vessel IDs`,
                    ApparentFishingHrs = .data$`Apparent Fishing Hours`)

    return(data)
  }

  # Create expanded dataframe with all combinations
  eg <- tidyr::expand_grid(
    Date = seq(start_date, end_date, by = "366 days"),
    Region = region_id
  )

  data_df <- purrr::map2(eg$Date, eg$Region, ~ get_data_for_range(.x, min(.x + 365, end_date), .y)) %>%
    vctrs::list_drop_empty() %>%
    dplyr::bind_rows()

  if(rlang::is_empty(data_df)){
    stop(paste0("No data found at all for the requested area of ", region, " between ", start_date, " and ", end_date))
  }


  if (isTRUE(compress)){

    data_df <- data_df %>%
      # dplyr::select("Lon", "Lat", "ApparentFishingHrs", "GFWregionID") %>%
      dplyr::group_by(.data$Lon, .data$Lat) %>%
      dplyr::summarise("ApparentFishingHrs" = sum(.data$ApparentFishingHrs, na.rm = TRUE),
                       GFWregionID = dplyr::first(.data$GFWregionID)) %>%
      dplyr::ungroup()

    data_sf <- data_df %>%
      terra::rast(type = "xyz", crs = "EPSG:4326") %>% # Convert to polygons for easier use
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = TRUE, round = FALSE) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(GFWregionID = as.factor(.data$GFWregionID),
                    GFWregion = region)

    if (dim(data_df)[1] != dim(data_sf)[1]){
      stop("Data dimensions of data_df and data_sf do not match after conversion to polygon")
    }

  } else if (isFALSE(compress)){

    # Combine data frames in the list into one data frame

    # Separate the "Time Range" column based on the specified temp_res
    if (temp_res == "yearly") {
      data_sf <- data_df %>%
        dplyr::mutate(Year = .data$TimeRange) %>%
        sf::st_as_sf(coords = c("Lon", "Lat"), crs ="EPSG:4326")
    } else {
      # Otherwise, separate the "Time Range" column according to the specified temp_res
      if (temp_res == "monthly") {
        data_sf <- data_df %>%
          tidyr::separate("TimeRange", into = c("Year", "Month"), sep = "-", remove = FALSE) %>%
          sf::st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4326")
      } else if (temp_res == "daily") {
        data_sf <- data_df %>%
          tidyr::separate("TimeRange", into = c("Year", "Month", "Day"), sep = "-", remove = FALSE) %>%
          sf::st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4326")
      }
    }
  }
  # But you may wish to return the data in a different CRS. For this you need transform
  if (isFALSE(cCRS == "EPSG:4326")){
    data_sf <- data_sf %>%
      sf::st_transform(crs = cCRS)
  }
  return(data_sf)
}

