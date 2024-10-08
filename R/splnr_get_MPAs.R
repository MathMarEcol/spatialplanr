#' Get marine parks from the WDPA.
#'
#' This code is a wrapper for the wonderful `wdpar` package written by Jeffrey O. Hanson. This data is then interfaced with the planning units.
#' An `sf` object is returned with the PU area covered by the selected marine protected areas.
#'
#' @param PlanUnits Planning Units as an `sf` object
#' @param Countries A character vector of the countries for which to extract MPAs. To get all MPAs, use `"global"` here.
#' @param Status The status field in the WDPA provides information on whether a protected area has been established, designated, or proposed at the time the data was submitted.
#' @param Desig The designation type is the category or type of protected area as legally/officially designated or proposed.
#' @param Category Stores the IUCN Protected Area Management Categories (recorded in field IUCN_CAT) for each of the protected areas where these categories are reported
#' @param ... Other arguments passed to `wdpa_fetch()`
#'
#' @return A `sf` object with the MPAs intersected with the planning units
#' @export
#'
#' @examples
#' dat <- splnr_get_MPAs(PlanUnits = dat_PUs, Countries = "Australia")
#'
#' aust <- rnaturalearth::ne_countries(country = "Australia", returnclass = "sf")
#'
#' gg <- ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = dat, ggplot2::aes(fill = wdpa)) +
#'   ggplot2::geom_sf(data = aust, fill = "grey50")
splnr_get_MPAs <- function(PlanUnits,
                           Countries,
                           Status = c("Designated", "Established", "Inscribed"),
                           Desig = c("National", "Regional", "International", "Not Applicable"),
                           Category = c("Ia", "Ib", "II", "III", "IV"),
                           ...) {

  assertthat::assert_that(
    inherits(PlanUnits, "sf"),
    is.character(Countries),
    all(Status %in% c("Designated", "Established", "Inscribed", "Proposed", "Adopted")),
    all(Desig %in% c("National", "Regional", "International", "Not Applicable")),
    all(Category %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Applicable", "Not Assigned"))
  )


  options(chromote.timeout = 120)

  wdpa_data <- Countries %>%
    purrr::map(wdpar::wdpa_fetch,
           wait = TRUE,
           download_dir = rappdirs::user_data_dir("wdpar"),
           ...) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(.data$MARINE > 0) %>%
    dplyr::filter(.data$IUCN_CAT %in% Category) %>% # filter category
    dplyr::filter(.data$DESIG_TYPE %in% Desig) %>% # filter designation
    dplyr::filter(.data$STATUS %in% Status) %>% # filter status
    wdpar::wdpa_clean(retain_status = NULL, erase_overlaps = FALSE) %>% # clean protected area data
    wdpar::wdpa_dissolve() %>% # Dissolve data to remove overlapping areas.
    dplyr::select("geometry") %>%
    dplyr::mutate(wdpa = 1)

  wdpa_data <- spatialgridr::get_data_in_grid(spatial_grid = PlanUnits,
                                              dat = wdpa_data,
                                              cutoff = NULL)

  return(wdpa_data)
}
