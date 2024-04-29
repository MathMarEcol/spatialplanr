
#' Function for creating polygon
#'
#' `splnr_create_polygon()` allows you to create a polygon based on longitude and latitude coordinates in your input data.
#'
#' @param x A named vector of lon/lat coordinates from which to make an `sf` polygon
#' @param cCRS The CRS to use for the polygon
#'
#' @return An `sf` object for the polygon
#' @export
#'
#' @examples
#' splnr_create_polygon(x = dplyr::tibble(x = seq(-50, 50, by = 1), y = 120) %>%
#'   dplyr::bind_rows(dplyr::tibble(x = 50, y = seq(120, 180, by = 1))) %>%
#'   dplyr::bind_rows(dplyr::tibble(x = seq(50, -50, by = -1), y = 180)) %>%
#'   dplyr::bind_rows(dplyr::tibble(x = -50, y = seq(150, 120, by = -1))))
splnr_create_polygon <- function(x, cCRS = "EPSG:4326") {

  assertthat::assert_that(
    inherits(x, "tbl_df") && !is.null(attributes(x)),
    is.character(cCRS)
  )

  x <- x %>%
    as.matrix() %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = "EPSG:4326") %>%
    sf::st_transform(crs = cCRS) %>%
    sf::st_sf()
}



#' Remove NAs from spatial data using nearest neighbour
#'
#' `splnr_replace_NAs()` allows you to replace NA values in your data with the value of the nearest neighbor.
#' The nearest neighbor is determined using `st_nearest_feature()` from the `sf` package.
#'
#' @param df An `sf` dataframe
#' @param vari Variable to remove NAs from
#'
#' @return An `sf` object with NAs replaced with the nearest neighbour
#' @export
#'
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @examples
#' df <- dat_species_prob %>%
#'   splnr_replace_NAs("Spp2")
splnr_replace_NAs <- function(df, vari) {

  assertthat::assert_that(
    inherits(df, c("sf", "data.frame")),
    is.character(vari),
    vari %in% names(df)
  )

  if (sum(is.na(dplyr::pull(df, !!rlang::sym(vari)))) > 0) { # Check if there are NAs

    gp <- df %>%
      dplyr::mutate(isna = is.na(!!rlang::sym(vari)))

    gp <- split(gp, f = as.factor(gp$isna))

    d <- sf::st_nearest_feature(gp$`TRUE`, gp$`FALSE`)

    gp$`TRUE` <- gp$`TRUE` %>%
      dplyr::mutate(!!rlang::sym(vari) := dplyr::pull(gp$`FALSE`, !!rlang::sym(vari))[d])

    df <- rbind(gp$`FALSE`, gp$`TRUE`) %>%
      dplyr::select(-"isna") %>%
      dplyr::arrange(.data$cellID)
  }
  return(df)
}



#' Substitute numbers for all_names in regionalisations
#'
#' Many regionalisations have numeric values in the shape files that correspond
#' to a vector of names. Here we provide a function to quickly replace the
#' numbers with names.
#'
#' @param dat `sf` data frame with one column of numeric/integer corresponding to `nam`
#' @param nam Named character vector of names corresponding to column of dat to recode
#'
#' @return An `sf` dataframe with numeric regionalisations substituted for category names
#' @export
#'
#' @importFrom rlang :=
#' @examples
#' dat <- dat_region %>% dplyr::select(-cellID)
#' nam <- c("Region1" = "SE Aust", "Region2" = "Tas", "Region3" = "NE Aust")
#' df <- splnr_match_names(dat, nam)
splnr_match_names <- function(dat, nam) {

  assertthat::assert_that(
    inherits(dat, "sf"),
    is.character(nam) && length(nam) > 0
  )

  col_name <- stringr::str_subset(colnames(dat), "geometry", negate = TRUE)[[1]]

  out <- dat %>%
    dplyr::mutate(!!col_name := nam[!!rlang::sym(col_name)]) # Apply categories to data
}



#' Scale spatial layers to between 0 and 1
#'
#' `splnr_scale_01()` allows you to re-scale your data from values that are greater than 1 to values that are between 0 and 1.
#'
#' @param dat `sf` dataframe
#' @param col_name Name of the column to scale
#'
#' @return `sf` dataframe
#' @export
#'
#' @importFrom rlang :=
#'
#' @examples
#' df <- dat_species_prob %>%
#'   dplyr::mutate(Spp1 = Spp1 * 100) %>%
#'   splnr_scale_01(col_name = "Spp1")
splnr_scale_01 <- function(dat, col_name) {

  assertthat::assert_that(
    inherits(dat, c("sf", "data.frame")),
    is.character(col_name),
    col_name %in% names(dat)
  )

  mx <- max(dplyr::pull(dat, !!rlang::sym(col_name)), na.rm = TRUE) # Get max probability

  if (mx > 100) {
    divi <- 1000
  } else if (mx > 10) {
    divi <- 100
  } else if (mx > 1) {
    divi <- 10
  } else if (mx < 1) {
    divi <- 1 # Do nothing
  }

  dat <- dat %>%
    dplyr::mutate(!!col_name := !!rlang::sym(col_name) / divi)
}




#' Returns the feature names
#'
#' `splnr_featureNames()` allows you to extract the names of features you want to pass to a `prioritizr` prioritization.
#' It requires an `sf` object input and returns the column names of the object excluding any columns you specify in the `exclude` argument.
#'
#' @param dat sf dataframe of features
#' @param exclude Character vector of any columns to exclude
#'
#' @return A character vector of names
#' @export
#'
#' @examples
#' df <- dat_species_prob %>%
#'   splnr_featureNames(exclude = c("cellID"))
splnr_featureNames <- function(dat, exclude = NA) {

  assertthat::assert_that(
    inherits(dat, "sf"),
    is.character(exclude) || is.na(exclude)
  )

  if (is.na(exclude)) {
    exclude <- c("Cost_", "cellID")
  } else {
    exclude <- c("Cost_", "cellID", exclude)
  }

  dat <- dat %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-tidyselect::starts_with(exclude)) %>%
    colnames()

  return(dat)
}



#' Ensure all features are in the same order.
#'
#' `splnr_arrangeFeatures()` sorts your data based on longitude and latitude values.
#'
#' @param df An sf object to sort by Lon and Lat
#'
#' @return A sorted sf object with the additionl cellID column
#' @export
#'
#' @examples
#' df <- dat_species_prob %>%
#'   splnr_arrangeFeatures()
splnr_arrangeFeatures <- function(df) {

  assertthat::assert_that(inherits(df, "sf"),
                          msg = "Input must be an sf object.")
  # Sort rows to ensure all features are in the same order.
  suppressWarnings(
    xy <- sf::st_coordinates(sf::st_centroid(df))
  )
  df <- df[order(xy[, "X"], xy[, "Y"]), ]

  df <- df %>%
    dplyr::mutate(cellID = dplyr::row_number())
}




#' Prepare data to plot Cohen's Kappa correlation matrix
#'
#' Conservation planning often requires the comparison of the outputs of the solutions of different conservation problems.
#' One way to compare solutions is by correlating the solutions using Cohen's Kappa.
#' `splnr_get_kappaCorrData()` takes a list of `prioritizr` solutions to perform the Cohen's Kappa correlation between the solution.
#' The resulting correlation matrix is symmetrical along the main diagonal and contains Cohen's Kappa of pairwise correlation between the solutions.
#'  The main diagonal should always be 1. The correlation matrix obtained from this function can be passed onto [splnr_plot_corrMat()].
#'
#' @param sol List of `prioritizr` solutions (`sf` objects) with solutions having a column name `solution_1`
#' @param name_sol Name tags to the different solutions
#'
#' @return `matrixOut` matrix
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # 30 % target for problem/solution 1
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # 50 % target for problem/solution 2
#' dat_problem2 <- prioritizr::problem(
#'   dat_species_bin %>%
#'     dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.5) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln2 <- dat_problem2 %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' corrMat <- splnr_get_kappaCorrData(list(dat_soln, dat_soln2), name_sol = c("soln1", "soln2"))
splnr_get_kappaCorrData <- function(sol, name_sol) {

  assertthat::assert_that(
    is.list(sol),
    length(sol) > 1,
    is.character(name_sol),
    length(name_sol) == length(sol)
  )

  s_list <- lapply(seq_along(sol), function(x) {
    sol[[x]] %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      dplyr::select("solution_1") %>%
      stats::setNames(name_sol[[x]])
  })

  y <- 1
  s_matrix <- list()
  for (i in 1:length(s_list)) {
    for (j in 1:length(s_list)) {
      kappa_temp <- irr::kappa2(dplyr::bind_cols(s_list[[i]], s_list[[j]]))
      kappa_corrvalue <- kappa_temp$value
      kappa_pvalue <- kappa_temp$p.value
      s_matrix[[y]] <- cbind(colnames(s_list[[i]]), colnames(s_list[[j]]), kappa_corrvalue, kappa_pvalue)
      y <- y + 1
    }
  }

  s_matrix_all <- do.call(rbind, s_matrix) %>%
    tibble::as_tibble(.name_repair = "unique")
  colnames(s_matrix_all)[1:2] <- c("plan1", "plan2")

  matrix_final <- s_matrix_all %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::select(-kappa_pvalue) %>%
    tidyr::pivot_wider(names_from = "plan2", values_from = kappa_corrvalue) %>%
    as.matrix()

  matrix_x <- s_matrix_all %>%
    tibble::as_tibble(.name_repair = "unique")

  # creating corrplot
  rownames(matrix_final) <- matrix_final[, 1]
  n <- length(s_list) + 1 # 4 is the number of inputted scenarios
  matrixOut <- matrix_final[, 2:n]
  class(matrixOut) <- "numeric"

  return(matrixOut)
}

#' Prepare data to plot Selection Frequency of planning units
#'
#' When multiple spatial plans are generated, we are often interested in how many times a planning unit is selected across an array of solutions. This array can either be a `list` of the solutions of different conservation problems or generated through a [portfolio approach]{https://prioritizr.net/reference/portfolios.html} with `prioritizr`.
#' `splnr_get_selFreq()` allows you to calculate the selection frequency of each planning unit of either a `list` or a `portfolio` of solutions. The resulting `sf` object can be passed for visualization to the `spatialplanr` function [splnr_plot_selectionFreq()].
#'
#' @param solnMany List or portfolio of `prioritizr` solutions
#' @param type Either "portfolio" (`sf` object) with a portfolio produced using `prioritizr` or "list" with a list of solutions
#'
#' @return `selFreq` `sf` object containing a column with the selection frequency (sum over all solutions).
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # create conservation problem that contains a portfolio of solutions
#' dat_soln_portfolio <- dat_problem %>%
#'   prioritizr::add_cuts_portfolio(number_solutions = 5) %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' selFreq <- splnr_get_selFreq(solnMany = dat_soln_portfolio, type = "portfolio")
#' (splnr_plot_selectionFreq(selFreq))
#'
splnr_get_selFreq <- function(solnMany, type = "portfolio") {

  assertthat::assert_that(
    type %in% c("portfolio", "list"),
    (type == "portfolio" && inherits(solnMany, "sf")) || (type == "list" && is.list(solnMany)),
    (!is.null(solnMany) && length(solnMany) > 0)
  )

  if (type == "portfolio") { # check if provided input is a protfolio

    if (class(solnMany)[[1]] != "sf") {
      print("You did not provide a portfolio of solutions. Please check your input.")
    } else {
      selFreq <- solnMany %>%
        dplyr::mutate(selFreq = as.factor(rowSums(dplyr::select(tibble::as_tibble(solnMany), dplyr::starts_with("solution_"))))) %>%
        sf::st_as_sf(geometry = solnMany$geometry) %>%
        dplyr::select("selFreq")
      return(selFreq)
    }
  } else if (type == "list") { # if not portfolio check if input is list of solutions
    if (class(solnMany)[[1]] != "list") {
      print("You did not provide a list of solutions. Please check your input.")
    } else {
      name_sol <- stringr::str_c("soln", stringr::str_pad(1:length(solnMany), width = 1, pad = 0))

      s_list <- lapply(seq_along(solnMany), function(x) {
        solnMany[[x]] %>%
          tibble::as_tibble() %>%
          dplyr::select("solution_1") %>%
          stats::setNames(name_sol[[x]])
      })

      soln <- data.frame(matrix(unlist(s_list), ncol = length(s_list)))
      colnames(soln) <- name_sol

      selFreq <- soln %>%
        dplyr::mutate(selFreq = as.factor(rowSums(dplyr::select(soln, dplyr::starts_with("soln"))))) %>%
        sf::st_as_sf(geometry = solnMany[[1]]$geometry) %>%
        dplyr::select("selFreq")
      return(selFreq)
    }
  } else {
    print("This function requires either a prioritizr portfolio or a list of solutions. Please check your input.")
  }
}
