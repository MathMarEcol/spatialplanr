#' Get the boundary of the planning region.
#'
#' @param Limits The limits of the boundary. This can either be a 4 element numeric named vector (c(xmin = 150, xmax = 160, ymin = -40, ymax = -30)), a vector of ocean/sea names, or a vector of EEZs.,
#' @param Type The type of Limits being provided. Options are "Ocean" or "EEZ"
#' @param cCRS The CRS the boundary is to be returned in
#'
#' @return The boundary of the planning region
#' @export
#'
#' @examples
#' Bndry <- splnr_get_Boundary("North Atlantic Ocean", "Ocean")
#'
#' @importFrom rlang .data
splnr_get_Boundary <- function(Limits,
                                  Type,
                                  cCRS = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"){

  if (is.numeric(Limits)){
    Bndry <- dplyr::tibble(x = seq(Limits["xmin"], Limits["xmax"], by = 1), y = Limits["ymin"]) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(Limits["xmax"], Limits["xmin"], by = -1), y = Limits["ymax"])) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmin"], y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>%
      splnr_create_polygon(cCRS)

    return(Bndry)
  }

  if (Limits == "Global"){
    Bndry <- dplyr::tibble(x = seq(-180, 180, by = 1), y = -90) %>%
      dplyr::bind_rows(dplyr::tibble(x = 180, y = seq(-90, 90, by = 1))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(180, -180, by = -1), y = 90)) %>%
      dplyr::bind_rows(dplyr::tibble(x = -180, y = seq(90, -90, by = -1))) %>%
      splnr_create_polygon(cCRS)

    return(Bndry)
  }

  # TODO Can't use mregions because it seems to depend on rgdal and rgeos which are being deprecated
  #
  # if (Type == "EEZ"){
  #   Bndry <- mregions::mr_shp(key = "MarineRegions:eez") %>%
  #     dplyr::filter(.data$territory1 %in% Limits) %>%
  #     sf::st_union() %>%
  #     sf::st_transform(cCRS)
  #   return(Bndry)
  # }

  if (Type == "Oceans"){
    Bndry <- rnaturalearth::ne_download(scale = "large",
                                        category = "physical",
                                        type = "geography_marine_polys",
                                        returnclass = "sf") %>%
      dplyr::filter(.data$name %in% Limits) %>%
      sf::st_union() %>%
      sf::st_transform(cCRS)
    return(Bndry)
  }

}
