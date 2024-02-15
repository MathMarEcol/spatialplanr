#' Function to recover data from Global Fishing Watch.
#'
#' The code takes several parameters described below and return an sf object with gfw data aggregated or not (param compress)
#'
#' Written by Kilian Barreiro
#' Written: February 2024
#'
#' @param region Region studied (character).
#' @param start_date Start date (waited format : "%Y-%m-%d").
#' @param end_date End date (waited format : "%Y-%m-%d").
#' @param temp_res Temporal resolution ("daily","monthly","yearly").
#' @param spat_res Spatial resolution ("low" for 0.1 degree, "high" for 0.01 degree).
#' @param key Token for GFW API (see details GlobalFishingWatch_Examples vignette).
#' @param cCRS The csr to which the sf will be returned (default = "EPSG:4326").
#' @param compress Binary operator to compress (aggregate) the data per coordinates (default = FALSE).
#'
#'
#' @return An `sf` object with gfw data.
#' @export
#'
#' @examples
#'
#' Region <- "Coral Sea" # "Australia"
#' Type <- "Oceans" # "EEZ"
#'
#' Shape <- "Hexagon" # "Shape of PUs
#' PU_size <- 10000 # km2
#'
#' cCRS <- "ESRI:54009"
#'
#' Bndry <- spatialplanr::splnr_get_boundary(Region, Type, cCRS)
#'
#' landmass <- rnaturalearth::ne_countries(
#'  scale = "medium",
#'  returnclass = "sf"
#' ) %>%
#' sf::st_transform(cCRS)
#'
#' PUs <- spatialplanr::splnr_get_planningUnits(Bndry, landmass, PU_size, Shape)
#'
#' gfw_data <- gfwr::splnr_get_gfw('Australia', "2022-01-01", "2022-12-31", "yearly", cCRS = cCRS, compress = TRUE)
#'
splnr_get_gfw <- function(region, start_date, end_date, temp_res,
                          spat_res = "low",
                          key = gfwr::gfw_auth(),
                          cCRS = "EPSG:4326",
                          compress = FALSE) {

  region_id <- gfwr::get_region_id(region_name = region, region_source = 'eez', key = key)$id[1]

  # Convert dates into Date objects
  start_date <- as.Date(start_date, format = "%Y-%m-%d")
  end_date <- as.Date(end_date, format = "%Y-%m-%d")

  # Function to obtain data for a specific date range
  get_data_for_range <- function(start_date, end_date) {
    date_range <- paste(start_date, end_date, sep = ",")

    data <- gfwr::get_raster(
      spatial_resolution = spat_res,
      temporal_resolution = temp_res,
      group_by = 'flagAndGearType',
      date_range = date_range,
      region = region_id,
      region_source = 'eez',
      key = key
    )

    return(data)
  }

  # Check whether the date range is less than or equal to 366 days
  if (as.numeric(difftime(end_date, start_date, units = "days")) <= 366) {
    # If yes, obtain data for the entire date range
    data_df <- get_data_for_range(start_date, end_date)

  } else {
    # If not, divide the date range into 366-day chunks and obtain the data for each chunk.
    date_chunks <- seq(start_date, end_date, by = "366 days")
    data_df <- purrr::map_dfr(date_chunks, ~ get_data_for_range(.x, min(.x + 365, end_date)))
  }

  if (isTRUE(compress)){
    # GFW data will always be "EPSG:4326". No need to have CRS as an option here

    data_df <- data_df %>%
      dplyr::select("Lon", "Lat", "Apparent Fishing Hours") %>%
      dplyr::group_by(Lon, Lat) %>%
      dplyr::summarise("Apparent Fishing Hours" = sum(`Apparent Fishing Hours`, na.rm = TRUE)) %>%
      dplyr::ungroup()

    data_sf <- data_df %>%
      terra::rast(type = "xyz", crs = "EPSG:4326") %>% # Convert to polygons for easier use
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = TRUE, round = FALSE) %>%
      sf::st_as_sf()

    if (dim(data_df)[1] != dim(data_sf)[1])
    {stop("Data dimensions of data_df and data_sf do not match after conversion to polygon")}

  } else if (isFALSE(compress)){

    # Combine data frames in the list into one data frame
    data_df <- bind_rows(data_df)

    # Separate the "Time Range" column based on the specified temp_res
    if (temp_res == "yearly") {
      data_sf <- data_df %>%
        dplyr::mutate(Year = `Time Range`) %>%
        sf::st_as_sf(coords = c("Lon", "Lat"), crs ="EPSG:4326")
    } else {
      # Sinon, séparer la colonne "Time Range" selon le temp_res spécifié
      if (temp_res == "monthly") {
        data_sf <- data_df %>%
          tidyr::separate("Time Range", into = c("Year", "Month"), sep = "-", remove = FALSE) %>%
          sf::st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4326")
      } else if (temp_res == "daily") {
        data_sf <- data_df %>%
          tidyr::separate("Time Range", into = c("Year", "Month", "Day"), sep = "-", remove = FALSE) %>%
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
