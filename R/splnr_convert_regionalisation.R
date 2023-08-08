
#' Function to interpolate regionalisation data onto Planning Units
#'
#' This is a wrapper for `splnr_Convert2PUs()` but deals with need to processes each layer seperately
#'
#' The dataset needs to be raster or vector format.
#' If the input contains continuous data, the output is an area-averaged mean for each planning unit.
#' If the input is binary, the output is the proportion of the planning unit covered.
#'
#' @param dat Dataset in raster or sf format.
#' @param PUs `sf` object of Planning Units
#' @param cat_name A character string of all categories in the regionalisation
#' @param col_name The name of the layer
#'
#' @return `sf` object containing the Planning Units and the feature.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- splnr_convert_regionalisation(dat, PUs)
#'   }
splnr_convert_regionalisation <- function(dat, PUs, cat_name = NA, col_name = NA){

  if (stringr::str_detect(class(dat)[1], "SpatRaster") == TRUE) { # Rasters

    col_name <- names(dat) # Name of layer

    out <- splnr_convert_2PUs(terra::as.factor(dat), PUs) %>% # Make categorical first
      splnr_replace_NAs(col_name) %>% # Remove Na's in variable
      dplyr::mutate(name = cat_name[!!rlang::sym(col_name)]) %>% # Apply categories to data
      tidyr::pivot_wider(names_from = .data$name, values_from = !!rlang::sym(col_name)) %>% # Change to columns
      dplyr::mutate(dplyr::across(!tidyselect::starts_with(c("cellID", "geometry")), ~tidyr::replace_na(.x, 0))) %>% # Replace NAs with 0
      sf::st_as_sf(sf_column_name = "geometry")

    return(out)
  } else if (stringr::str_detect(class(dat)[1], "sf") == TRUE){

    # At the moment this is set up for a single column with multiple data types
    # This is probably the default because most data will be irregular

    if(is.na(col_name)){
      col_name = stringr::str_subset(colnames(dat), "geometry", negate = TRUE) # Get the column name without returning "geometry"
    }

    cat_name <- unique(dplyr::pull(dat, !!rlang::sym(col_name)))

    # Run through each regionalisation type to get proportion of PU coverage.
    for (idx in 1:length(cat_name)){
      PUs <- splnr_convert_2PUs(dat %>%
                                  dplyr::filter(!!rlang::sym(col_name) == cat_name[idx]) %>%
                                  dplyr::rename(!!cat_name[idx] := col_name),
                                PUs)
    }
    return(PUs)
  }
}

