#' Function to plot data.
#'
#' (For now can replace splnr_plot_cost(), splnr_plot_binFeature(), splnr_plot_MPAs(), splnr_plot_featureNo())
#'
#' Written by Kilian Barreiro and Jason Everett
#' Written: February 2024
#'
#' @param df The dataframe containing the data to be plotted. It must include a geometry column to be used with geom_sf.
#' @param col_names A list of column names to include in the plot. If specified, only these columns will be used to colour the plot.
#' @param paletteName The name of the colour palette to use for filling. Default is "YlGnBu".
#' @param colourVals The colour values to use if col_names is specified and the data is binary.
#' @param plot_title The title of the plot.
#' @param legend_title The title of the legend.
#' @param legend_labels A vector of strings containing the labels to use for legend values.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' # Binary plot of species distribution
#' splnr_plot(df = dat_species_bin,
#'            col_names = "Spp1",
#'            legend_title = "Legend",
#'            legend_labels = c("Absent", "Present"))
#'
#' # Logical plot of species distribution
#' splnr_plot(df = dat_species_bin %>%
#'                 dplyr::mutate(dplyr::across(
#'                   tidyselect::starts_with("Spp"), as.logical)),
#'            col_names = "Spp1",
#'            legend_title = "Legend",
#'            legend_labels = c("Absent", "Present"))
#'
#' # Continuous plot of bathymetry#
#' splnr_plot(df = dat_bathy,
#'            col_names = "bathymetry",
#'            plot_title = "Bathymetry",
#'            legend_title = "Bathymetry (m)")
#'
#' # Plot Planning Units
#' splnr_plot(df = dat_PUs)
#'
#' # Multi binary features
#' splnr_plot(df = dat_species_bin,
#'            col_names = colnames(dat_species_bin %>%
#'                                   sf::st_drop_geometry() %>%
#'                                   dplyr::select(
#'                                     tidyselect::starts_with("Spp"))),
#'            legend_title = "Number of features")

splnr_plot <- function(df,
                       col_names = NULL,
                       paletteName = "YlGnBu",
                       colourVals = c("#c6dbef", "#3182bd"),
                       plot_title = "",
                       legend_title = NULL,
                       legend_labels = NULL) {

  # Assertions
  assertthat::assert_that(
    is.data.frame(df),
    inherits(df, "sf"), # Check it is an sf
    all(c("xmin", "xmax", "ymin", "ymax") %in% names(sf::st_bbox(df))),
    is.null(col_names) | is.character(col_names),
    all(col_names %in% colnames(df)), # Do the column names exist
    # Check all col_names are in the the dataset
    is.character(paletteName),
    is.null(legend_title) | is.character(legend_title),
    is.null(legend_labels) | is.character(legend_labels)
  )

  # Set the defaults
  is_binary <- FALSE
  is_logi <- FALSE
  is_continuous <- FALSE
  showFeatureSum <- FALSE

  # Figure out data type

  if (!is.null(col_names)){ # If there is a column name

    if (length(col_names) == 1){ # One column name

      if (is.logical(df[[col_names]])){ # Is the column logical?
        is_logi <- TRUE
      } else { # See if it is binary
        df0 <- df %>%
          # Replace NA with 0 in selected columns for binary data verification
          dplyr::mutate(dplyr::across(tidyselect::all_of(col_names), ~tidyr::replace_na(., 0)))

        is_binary <- all(purrr::map_vec(col_names, function(x) all(df0[[x]] %in% c(0, 1))))
      }

      ## Is the data continuous?
      if (isFALSE(is_binary) & isFALSE(is_logi)){
        is_continuous <- TRUE # May not always be true but plotting as continuous will work, and highlight the issue
      }

    } else if (length(col_names) > 1){ # Mutliple columns
      showFeatureSum <- TRUE # FeatureSum
    }

  }
  # DEFAULT PLOT CODE
  gg <- ggplot2::ggplot() +
    ggplot2::coord_sf(xlim = sf::st_bbox(df)$xlim, ylim = sf::st_bbox(df)$ylim) +
    ggplot2::labs(subtitle = plot_title)

  # Plot logic based on data type

  if (showFeatureSum) { #TODO I don't think this is used anymore. Remove?

    # Calculate feature sum if multiple features
    df <- df %>%
      dplyr::as_tibble() %>%
      dplyr::select(tidyselect::all_of(c(col_names, "geometry"))) %>%
      dplyr::mutate(FeatureSum = rowSums(dplyr::across(tidyselect::where(is.numeric)), na.rm = TRUE)) %>%
      sf::st_as_sf(sf_column_name = "geometry") %>%
      dplyr::select("FeatureSum")

    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$FeatureSum), colour = NA, size = 0.1) +
      ggplot2::scale_fill_distiller(
        name = legend_title,
        palette = paletteName,
        aesthetics = c("fill"),
        oob = scales::squish) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(order = -1))

    return(gg)
  } else if (is_binary | is_logi) {

    if (is.null(legend_labels)){
      legend_labels = c("Absence", "Presence")
    }

    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = factor(.data[[col_names]])), colour = "grey80", size = 0.1)

    if (isTRUE(is_binary)) {
      gg <- gg +
        ggplot2::scale_fill_manual(values = c("0" = colourVals[1], "1" = colourVals[2]),
                                   labels = legend_labels,
                                   name = legend_title)
    }

    if (isTRUE(is_logi)) {
      gg <- gg +
        ggplot2::scale_fill_manual(values = c("FALSE" = colourVals[1], "TRUE" = colourVals[2]),
                                 labels = legend_labels,
                                 name = legend_title)}


  } else if (is_continuous) {

    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data[[col_names]], colour = .data[[col_names]])) +
      ggplot2::scale_fill_viridis_c(name = legend_title, aesthetics = c("colour", "fill")) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(order = 1),
                      colour = "none")

  } else if (is.null(col_names)){ # No column to plot by

    gg <- gg +
      ggplot2::geom_sf(data = df, colour = "grey80", fill = NA, size = 0.1)
  }

  return(gg)
}
