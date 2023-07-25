utils::globalVariables("where")



#' Function for creating polygon
#'
#' @param x A named vector of lon/lat coordinates from which to make an `sf` polygon
#' @param cCRS The CRS to use for the polygon
#'
#' @return An `sf` object for the polygon
#' @export
#'
#' @examples
#'splnr_create_polygon(x = dplyr::tibble(x = seq(-50, 50, by = 1), y = 120) %>%
#'                       dplyr::bind_rows(dplyr::tibble(x = 50, y = seq(120, 180, by = 1))) %>%
#'                       dplyr::bind_rows(dplyr::tibble(x = seq(50, -50, by = -1), y = 180)) %>%
#'                       dplyr::bind_rows(dplyr::tibble(x = -50, y = seq(150, 120, by = -1))))
splnr_create_polygon <- function(x, cCRS = "EPSG:4326"){
  x <- x %>%
    as.matrix() %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = "EPSG:4326") %>%
    sf::st_transform(crs = cCRS)
}



#' Remove NAs from spatial data using nearest neighbour
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
#'     splnr_replace_NAs("Spp2")
splnr_replace_NAs <- function(df, vari){
  if (sum(is.na(dplyr::pull(df, !!rlang::sym(vari)))) > 0){ # Check if there are NAs

    gp <- df %>%
      dplyr::mutate(isna = is.na(!!rlang::sym(vari)))
    gp <- split(gp, f = as.factor(gp$isna))

    d <- sf::st_nearest_feature(gp$`TRUE`, gp$`FALSE`)

    gp$`TRUE` <- gp$`TRUE` %>%
      dplyr::mutate(!!rlang::sym(vari) := dplyr::pull(gp$`FALSE`, !!rlang::sym(vari))[d])

    df <- rbind(gp$`FALSE`, gp$`TRUE`) %>%
      dplyr::select(-.data$isna) %>%
      dplyr::arrange(.data$cellID)

  }
  return(df)
}



#' Substitute numbers for all_names in regionalisations
#'
#' Many regionalisations have numeric values in teh shape files that correpond
#' to a vector of names. Here we provide a function to quickly replace the
#' numbers with names.
#'
#' @param dat `sf` data frame with one column of numeric/integer corresponding to `nam`
#' @param nam character vector of names corresponding to numeric column of dat
#'
#' @return An `sf` dataframe with numeric regionalisations substituted for category names
#' @export
#'
#' @importFrom rlang :=
#' @examples
splnr_match_names <- function(dat, nam){
  col_name = stringr::str_subset(colnames(dat), "geometry", negate = TRUE)[[1]]

  out <- dat %>%
    dplyr::mutate(!!col_name := nam[!!rlang::sym(col_name)]) # Apply categories to data
}



#' Scale spatial layers to between 0 and 1
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
#'     dplyr::mutate(Spp1 = Spp1 * 100) %>%
#'     splnr_scale_01(col_name = "Spp1")
splnr_scale_01 <- function(dat, col_name){

  mx  <- max(dplyr::pull(dat, !!rlang::sym(col_name)), na.rm = TRUE) # Get max probability

  if (mx > 100){
    divi <- 1000
  } else if(mx > 10){
    divi <- 100
  } else if(mx > 1){
    divi <- 10
  } else if(mx < 1){
    divi <- 1 # Do nothing
  }

  dat <- dat %>%
    dplyr::mutate(!!col_name := !!rlang::sym(col_name)/divi)

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
#'   splnr_convert2Pacific()
splnr_convert2Pacific <- function(df,
                                  buff = 0,
                                  cCRS = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){

  #TODO add a warning if df doesn't cross the pacific dateline

  longlat <- "EPSG:4326"

  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match
  # that of world Adapted from here:
  # https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2

  polygon <- sf::st_polygon(x = list(rbind(c(-0.0001, 90),
                                           c(0, 90),
                                           c(0, -90),
                                           c(-0.0001, -90),
                                           c(-0.0001, 90)))) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(longlat)

  # Modify world dataset to remove overlapping portions with world's polygons
  #TODO add a warning if the input df is not unprojected
  df_proj <- df %>%
    sf::st_transform(longlat) %>% # The input needs to be unprojected.
    sf::st_make_valid() %>% # Just in case....
    sf::st_difference(polygon) %>%
    sf::st_transform(crs = cCRS) # Perform transformation on modified version of polygons
  rm(polygon)

  # # notice that there is a line in the middle of Antarctica. This is because we have
  # # split the map after reprojection. We need to fix this:
  bbox <-  sf::st_bbox(df_proj)
  bbox[c(1,3)] <- c(-1e-5, 1e-5)
  polygon_proj <- sf::st_as_sfc(bbox)

  crosses <- df_proj %>%
    sf::st_intersects(polygon_proj) %>%
    sapply(length) %>%
    as.logical %>%
    which

  # # Adding buffer (usually 0)
  df_proj <- df_proj[crosses,] %>%
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
#' @param df An sf object to sort by Lon and Lat
#'
#' @return A sorted sf object with the additionl cellID column
#' @export
#'
#' @examples
#' df <- dat_species_prob %>%
#'       splnr_arrangeFeatures()
splnr_arrangeFeatures <- function(df){

  # Sort rows to ensure all features are in the same order.
  xy <- sf::st_coordinates(sf::st_centroid(df))
  df <- df[order(xy[,"X"], xy[,"Y"]),]

  df <- df %>%
    dplyr::mutate(cellID = dplyr::row_number())

}



#' Prepare data to plot how well targets are met
#'
#' @param soln The `prioritizr` solution
#' @param pDat The `prioritizr` problem
#' @param allDat `sf` dataframe containing all features
#' @param Category `tibble` with information what category the features belong to (if NA, each feature has its own category)
#' @param climsmart logical denoting whether spatial planning was done climate-smart (and targets have to be calculated differently)
#' @param solnCol Name of the column with the sollution
#'
#' @return `tbl_df` dataframe
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' #not including incidental species coverage
#' dfNInc <- splnr_prepTargetData(soln = dat_soln, pDat = dat_problem, allDat = dat_species_bin,
#'                           Category = Category_vec, solnCol = "solution_1")
#'
#' #including incidental species coverage
#' dfInc <- splnr_prepTargetData(soln = dat_soln, pDat = dat_problem, allDat = dat_species_bin2,
#'                          Category = Category_vec2, solnCol = "solution_1")
splnr_prepTargetData <- function(soln, pDat, allDat, Category = NA,
                                 climsmart = FALSE, solnCol = "solution_1"){

  # ## Calculate the incidental protection for features not chosen
  not_selected <- allDat %>%
    dplyr::select(-tidyselect::starts_with("Cost"), -tidyselect::any_of(c("metric", "cellID", "FishResBlock"))) %>%
    sf::st_drop_geometry()

  selected <- dat_problem$data$features[[1]]#soln %>%
  #dplyr::select(-tidyselect::starts_with("Cost"), -tidyselect::any_of(c("metric", "FishResBlock")))

  ns_cols <- setdiff(not_selected %>% colnames(), selected)# %>% colnames())

  if (length(ns_cols) > 0){
    ns1 <- not_selected %>%
      dplyr::select(c(tidyselect::all_of(ns_cols))) %>% #, "geometry"
      dplyr::mutate(solution = dplyr::pull(soln, !!rlang::sym(solnCol)))

    area_feature <- ns1 %>%
      dplyr::select(-c("solution")) %>%
      #sf::st_drop_geometry() %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "total_amount") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(total_amount = sum(.data$total_amount))

    held_feature <- ns1 %>%
      dplyr::filter(.data$solution == 1) %>%
      dplyr::select(-c("solution")) %>%
      #sf::st_drop_geometry() %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "absolute_held") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(absolute_held = sum(.data$absolute_held))

    ns1 <- dplyr::left_join(area_feature, held_feature, by = "feature") %>%
      dplyr::mutate(incidental_held = (.data$absolute_held / .data$total_amount) * 100)
  } else {
    ns1 <- tibble::tibble(feature = "DummyVar",
                          total_amount = 0,
                          absolute_held = 0,
                          incidental_held = 0)
  }

  ## Now do the selected features

  s1 <- soln %>%
    dplyr::rename(solution = solution_1) %>%
    tibble::as_tibble()

  df_rep_imp <- prioritizr::eval_feature_representation_summary(pDat, s1[, 'solution'])

  if (climsmart == TRUE){

    df_rep_imp <- df_rep_imp %>%
      dplyr::select(-.data$relative_held) %>%
      dplyr::mutate(feature = stringr::str_remove_all(.data$feature, "_CS"),
                    feature = stringr::str_remove_all(.data$feature, "_NCS")) %>% # Ensure all features have the same name.
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(total_amount = sum(.data$total_amount), # Sum the features together
                       absolute_held = sum(.data$absolute_held)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(relative_held = .data$absolute_held/.data$total_amount) %>% # Calculate proportion
      dplyr::select(-.data$total_amount, -.data$absolute_held) # Remove extra columns

  }

  # Create named vector to do the replacement
  # rpl <- Dict[ order(match(Dict$Abbrev, df_rep_imp$feature)), ] %>%
  #   dplyr::select(.data$Abbrev, .data$Common) %>%
  #   tibble::deframe()

  df <- df_rep_imp %>%
    dplyr::mutate(relative_held = .data$relative_held * 100) %>%
    #dplyr::mutate(group = dplyr::if_else(.data$feature %in% imp_layers, "important", "representative")) %>%
    stats::na.omit() %>%
    dplyr::rename(value = .data$relative_held)# %>%
  #dplyr::mutate(feature = stringr::str_replace_all(.data$feature, rpl))

  df<- dplyr::full_join(df, ns1 %>% # Now join the non-selected values
                          dplyr::select(c(.data$feature, .data$incidental_held)),
                        by = "feature")

  if (is.data.frame(Category)){
    df <- dplyr::left_join(df, Category_vec, by = "feature")
  } else if (is.na(Category)) {
    df <- df %>%
      dplyr::mutate(category = feature)
  } else {
    print("Check that your Category input is in the right format.")
  }


  return(df)
}
