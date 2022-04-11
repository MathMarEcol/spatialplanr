#' Calculate Inverse Area Targets
#'
#' @param df
#' @param target_min
#' @param target_max
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Get_TargetsIA <- function(df, target_min, target_max){

  PU_area_km2 <- as.numeric(sf::st_area(df[1,1])/1e+06) # Area of each planning unit

  total_PU_area <- nrow(df) * PU_area_km2 # Total area of the study region

  feature_area <- df %>%
    dplyr::as_tibble() %>%
    dplyr::select(-.data$geometry) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(., is.na(.), 0))) %>%
    tidyr::pivot_longer(dplyr::everything(.), names_to = "Species", values_to = "Area_km2") %>%
    dplyr::mutate(Species = stringr::str_replace_all(.data$Species, pattern = "_", replacement = " "),
           Area_km2 = .data$Area_km2 * PU_area_km2,
           Target = target_max-((.data$Area_km2/total_PU_area)*(target_max-target_min)))

}

