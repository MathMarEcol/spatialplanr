#' Function to plot data.
#'
#' (For now can replace splnr_plot_cost(), splnr_plot_binFeature(), splnr_plot_MPAs(),splnr_plot_featureNo())
#'
#' Written by Kilian Barreiro
#' Written: February 2024
#'
#' Plot Spatial Data, returns a gg object
#'
#' @param df The dataframe containing the data to be plotted. It must include a geometry column to be used with geom_sf.
#' @param col_names A list of column names to include in the plot. If specified, only these columns will be used to color the plot.
#' @param paletteName The name of the color palette to use for filling. Default is "YlGnBu".
#' @param colourVals The color values to use if col_names is specified and the data is binary.
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
#' # Continuous plot of bathymetry
#' bathymetry <- oceandatr::get_bathymetry(spatial_grid = dat_PUs,
#'                                         keep = FALSE,
#'                                         classify_bathymetry = FALSE)
#
#' splnr_plot(df = bathymetry,
#'            col_names = "bathymetry",
#'            plot_title = "bathymetry",
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
  is_continuous <- FALSE
  showFeatureSum <- FALSE

  # Figure out data type

  if (!is.null(col_names)){ # One column name

    if (length(col_names) == 1){
      ## Is the data binary?
      df <- df %>%
        # Replace NA with 0 in selected columns for binary data verification
        dplyr::mutate(dplyr::across(tidyselect::all_of(col_names), ~tidyr::replace_na(., 0)))

      is_binary <- all(purrr::map_vec(col_names, function(col_name) all(df[[col_name]] %in% c(0, 1, TRUE, FALSE))))

      ## Is the data continuous?
      if (!is_binary){
        is_continuous <- TRUE # May not always be true but plotting as continuous will work, and highlight the issue
      }
    } else if (length(col_names) > 1){ # Mutliple columns
      showFeatureSum <- TRUE # FeatureSum
    }
  } else {
    is_binary <- FALSE
    is_continuous <- FALSE
    showFeatureSum <- FALSE

  }


  # DEFAULT PLOT CODE
  gg <- ggplot2::ggplot() +
    ggplot2::coord_sf(xlim = sf::st_bbox(df)$xlim, ylim = sf::st_bbox(df)$ylim) +
    ggplot2::labs(subtitle = plot_title)

    # ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(linetype = 0),
    #                                              nrow = 2,
    #                                              order = 1,
    #                                              direction = "horizontal",
    #                                              title = legend_title,
    #                                              title.position = "top",
    #                                              title.hjust = 0.5,
    #                                              labels = legend_labels))

  if (showFeatureSum) {

    # Calculate feature sum if multiple features
    df <- df %>%
      dplyr::as_tibble() %>%
      # dplyr::select(-tidyselect::any_of(c("cellID"))) %>%
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
  } else if (is_binary) {

    if (is.null(legend_labels)){
      legend_labels = c("Absence", "Presence")
    }

    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = factor(.data[[col_names]])), colour = "grey80", size = 0.1) +
      ggplot2::scale_fill_manual(values = c("0" = colourVals[1], "1" = colourVals[2]),
                                 labels = legend_labels,
                                 name = legend_title)


  } else if (is_continuous) {

    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = {{col_names}}), colour = "grey80", size = 0.1) +
      # ggplot2::scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(9, paletteName)), limits = NULL) + # TODO Remove RColourBrewer if possible to reduce dependencies
      ggplot2::guides(fill = ggplot2::guide_colourbar(order = 1))

  } else if (is.null(col_names)){ # No column to plot by

    gg <- gg +
      ggplot2::geom_sf(data = df, colour = "grey80", fill = NA, size = 0.1)
  }

  return(gg)
}
