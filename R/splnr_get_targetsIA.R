#' Calculate Inverse Area Targets
#'
#' This function takes a min (`target_min`) and max (`target_max`) target range and calculates an inverse area target for each feature based on areal coverage.
#'
#' @param df An `sf` dataframe with features to calculate
#' @param target_min The minimum target for inverse area
#' @param target_max The maximum target for inverse area
#'
#' @return An `sf` dataframe with Inverse Area Targets added in `Target`
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' targets <- dat_species_prob %>%
#'    splnr_get_TargetsIA(target_min = 0.3, target_max = 0.8)
splnr_get_TargetsIA <- function(df, target_min, target_max){

  PU_area_km2 <- as.numeric(sf::st_area(df[1,1])/1e+06) # Area of each planning unit

  total_PU_area <- nrow(df) * PU_area_km2 # Total area of the study region

  feature_area <- df %>%
    dplyr::select(-.data$cellID) %>%
    sf::st_drop_geometry() %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(., is.na(.), 0))) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Species", values_to = "Area_km2") %>%
    dplyr::mutate(Species = stringr::str_replace_all(.data$Species, pattern = "_", replacement = " "),
           Area_km2 = .data$Area_km2 * PU_area_km2,
           Target = target_max-((.data$Area_km2/total_PU_area)*(target_max-target_min)))

  return(feature_area)
}

