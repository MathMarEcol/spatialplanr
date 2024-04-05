#' Function to apply cutoffs to feature data
#'
#' @param features A sf dataframe with all the feature information
#' @param Cutoffs A single value or a named vector of cutoffs.
#' @param inverse If TRUE, values below the `Cutoffs` are used. If FALSE (default), values above are kept.
#'
#' @return A new sf dataframe that has cutoffs applied.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' df <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5)
splnr_apply_cutoffs <- function(features, Cutoffs, inverse = FALSE) {
  assertthat::assert_that(
    inherits(features, "sf"),
    is.numeric(Cutoffs) | (is.numeric(Cutoffs) & length(names(Cutoffs)) > 0),
    is.logical(inverse)
  )

  if (length(Cutoffs) == 1 & length(names(Cutoffs)) == 0) { # Single cutoff for all data if unnamed vector

    features <- features %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(dplyr::across(
        -dplyr::any_of(c("cellID", "geometry")), # Apply to all columns except geometry and cellID
        ~ dplyr::case_when(
          . >= Cutoffs ~ 1,
          . < Cutoffs ~ 0,
          is.na(.data) ~ 0
        )
      )) %>%
      sf::st_as_sf()

    if (inverse == TRUE) { # Need to flip the ones/zeros
      features <- features %>%
        dplyr::mutate(dplyr::across(-dplyr::any_of(c("cellID", "geometry")), ~ 1 - .))
    }
  } else if (length(Cutoffs) == length(names(Cutoffs))) { # Named vector with values for each column


    nm <- names(Cutoffs) # Testing - We should only be operating on the columns in the Cutoffs vector

    for (f in 1:length(nm)) {
      features <- features %>%
        dplyr::mutate(!!nm[f] := dplyr::case_when(
          !!rlang::sym(nm[f]) >= Cutoffs[nm[f]] ~ 1,
          !!rlang::sym(nm[f]) < Cutoffs[nm[f]] ~ 0,
          is.na(!!rlang::sym(nm[f])) ~ 0
        ))

      if (inverse == TRUE) { # Need to flip the ones/zeros
        features <- features %>%
          dplyr::mutate(!!nm[f] := 1 - !!rlang::sym(nm[f]))
      }
    }
  }
  return(features)
}
