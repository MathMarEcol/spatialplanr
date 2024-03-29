utils::globalVariables("where")



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
#' @param exclude Character vector of any columes to exclude
#'
#' @return A character vector of names
#' @export
#'
#' @examples
#' df <- dat_species_prob %>%
#'   splnr_featureNames(exclude = c("cellID"))
splnr_featureNames <- function(dat, exclude = NA) {
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


#' Convert a world sf object to a Pacific-centred one
#' Defaults to assuming Robinson projection
#'
#' Written by Jason D. Everett
#' UQ/CSIRO/UNSW
#' Last edited 8th Sept 2021
#'
#' @param df An sf dataframe
#' @param buff The buffer too apply to features that cross after merge
#' @param cCRS The crs to use for the output.
#'
#' @return An sf object in the Robinson projection
#' @export
#'
#' @examples
#' df_rob <- rnaturalearth::ne_coastline(returnclass = "sf") %>%
#'   splnr_convert_toPacific()
splnr_convert_toPacific <- function(df,
                                    buff = 0,
                                    cCRS = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") {
  # TODO add a warning if df doesn't cross the pacific dateline

  longlat <- "EPSG:4326"

  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match
  # that of world Adapted from here:
  # https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2

  polygon <- sf::st_polygon(x = list(rbind(
    c(-0.0001, 90),
    c(0, 90),
    c(0, -90),
    c(-0.0001, -90),
    c(-0.0001, 90)
  ))) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(longlat)

  # Modify world dataset to remove overlapping portions with world's polygons
  # TODO add a warning if the input df is not unprojected
  suppressWarnings({
    df_proj <- df %>%
      sf::st_transform(longlat) %>% # The input needs to be unprojected.
      sf::st_make_valid() %>% # Just in case....
      sf::st_difference(polygon) %>%
      sf::st_transform(crs = cCRS) # Perform transformation on modified version of polygons
    rm(polygon)
  })

  # # notice that there is a line in the middle of Antarctica. This is because we have
  # # split the map after reprojection. We need to fix this:
  bbox <- sf::st_bbox(df_proj)
  bbox[c(1, 3)] <- c(-1e-5, 1e-5)
  polygon_proj <- sf::st_as_sfc(bbox)

  crosses <- df_proj %>%
    sf::st_intersects(polygon_proj) %>%
    sapply(length) %>%
    as.logical() %>%
    which()

  # # Adding buffer (usually 0)
  df_proj <- df_proj[crosses, ] %>%
    sf::st_buffer(buff)

  return(df_proj)
}



# Create one polygon that we can use to populate with PUs
#
# splnr_Create_SinglePolygon <- function (df, res){
#
#   # Creating a empty raster
#   rs <- raster::raster(ncol = 360*(1/res), nrow = 180*(1/res))
#   rs[] <- 1:raster::ncell(rs)
#   raster::crs(rs) <- sf::st_crs(df) # Make raster crs the same as the sf object.
#
#   # Fasterize the land object
#   df_rs <- fasterize::fasterize(df, rs)
#
#   pol <- stars::as(df_rs,  "SpatialPolygonsDataFrame")
#   pol$layer <- seq(1, length(pol))
#
#   # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
#   pol_sf <- sf::st_as_sf(pol) %>%
#     dplyr::select(.data$layer) %>%
#     dplyr::summarise(total_layer = sum(.data$layer, do_union = TRUE))
# }




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
