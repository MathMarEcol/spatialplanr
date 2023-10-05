#' Get the boundary of the planning region.
#'
#' `splnr_get_boundary()` allows to create an `sf` object of your planning region either based on specific coordinate information, or `rnaturalearth` inputs such as ocean data. Creating a boundary is often the first step in conservation planning and a requirement for downstream function sin `spatialplanr`.
#'
#' @param Limits The limits of the boundary. This can either be a 4 element numeric named vector (c(xmin = 150, xmax = 160, ymin = -40, ymax = -30)), a vector of ocean/sea names, or a vector of EEZs.,
#' @param Type The type of Limits being provided. Options are "Ocean" or "EEZ"
#' @param res The resolution (in degrees) from which to create the boundary polygon if numeric limits are provided.
#' @param cCRS The CRS the boundary is to be returned in
#'
#' @return The boundary of the planning region
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' Bndry <- splnr_get_boundary("North Atlantic Ocean", "Ocean")
splnr_get_boundary <- function(Limits,
                               Type,
                               res = 1,
                               cCRS = "ESRI:54009" # Mollweide
) {
  if (is.numeric(Limits)) {
    Bndry <- dplyr::tibble(x = seq(Limits["xmin"], Limits["xmax"], by = res), y = Limits["ymin"]) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = res))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(Limits["xmax"], Limits["xmin"], by = -res), y = Limits["ymax"])) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmin"], y = seq(Limits["ymax"], Limits["ymin"], by = -res))) %>%
      splnr_create_polygon(cCRS) %>%
      sf::st_sf()

    return(Bndry)
  }

  if (Limits == "Global") {
    Bndry <- dplyr::tibble(x = seq(-180, 180, by = res), y = -90) %>%
      dplyr::bind_rows(dplyr::tibble(x = 180, y = seq(-90, 90, by = res))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(180, -180, by = -res), y = 90)) %>%
      dplyr::bind_rows(dplyr::tibble(x = -180, y = seq(90, -90, by = -res))) %>%
      splnr_create_polygon(cCRS) %>%
      sf::st_sf()

    return(Bndry)
  }


  ## TODO Disable EEZ until offshoredatr publicly online.
  # if (Type == "EEZ"){
  #   Bndry <- offshoredatr::get_area(area_name = Limits) %>%
  #     dplyr::filter(.data$territory1 %in% Limits) %>%
  #     sf::st_union() %>%
  #     sf::st_transform(cCRS)
  #   return(Bndry)
  # }

  if (Type == "Oceans" | Type == "Ocean") {
    Bndry <- rnaturalearth::ne_download(
      scale = "large",
      category = "physical",
      type = "geography_marine_polys",
      returnclass = "sf"
    ) %>%
      dplyr::filter(.data$name %in% Limits) %>%
      sf::st_union() %>%
      sf::st_transform(cCRS)
    return(Bndry)
  }
}
