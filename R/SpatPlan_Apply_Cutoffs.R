#' Function to apply cutoffs to feature data
#'
#' @param features A sf dataframe with all the feature information
#' @param Cutoffs A single value or a named vector of cutoffs.
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @export
#'
#' @examples
#' @importFrom rlang .data
SpatPlan_Apply_Cutoffs <- function(features, Cutoffs){

  if (length(Cutoffs) == 1){ # Single cutoff for all data

    features <- features %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(dplyr::across(-dplyr::any_of(c("cellID", "geometry")), # Apply to all columns except geometry and cellID
                                  ~ dplyr::case_when(. >= Cutoffs ~ 1,
                                                     . < Cutoffs ~ 0,
                                                     is.na(.data) ~ 0))) %>%
      sf::st_as_sf()

  } else if (length(Cutoffs) > 1) { # Named vector with values for each column

    nm <- features %>%
      dplyr::as_tibble() %>%
      dplyr::select(-c(.data$cellID, .data$geometry)) %>%
      names()

    for (f in 1:length(nm)){
      features <- features %>%
        dplyr::mutate(!!nm[f] := dplyr::case_when(!!rlang::sym(nm[f]) >= Cutoffs[nm[f]] ~ 1,
                                                  !!rlang::sym(nm[f]) < Cutoffs[nm[f]] ~ 0,
                                                  is.na(!!rlang::sym(nm[f])) ~ 0))
    }
  }
}
