#' Function to plot data. (For now can replace splnr_plot_cost(), splnr_plot_binFeature(), splnr_plot_MPAs(),splnr_plot_featureNo())
#'
#' Written by Kilian Barreiro
#' Written: February 2024
#'
#' Plot Spatial Data, returns a gg object
#'
#' @param df The dataframe containing the data to be plotted. It must include a geometry column to be used with geom_sf.
#' @param col_names A list of column names to include in the plot. If specified, only these columns will be used to color the plot.
#' @param paletteName The name of the color palette to use for filling. Default is "YlGnBu".
#' @param colorVals The color values to use if col_names is specified and the data is binary.
#' @param plot_title The title of the plot.
#' @param showFeatureSum A boolean indicator to specify if the feature sum should be displayed.
#' @param legend_title The title of the legend.
#' @param legend_labels A vector of strings containing the labels to use for legend values.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' # Binary
#' splnr_plot(dat_species_bin, col_names = "Spp1", legend_title = "Legend", legend_labels = c("Absent", "Present"))
#'
#' # Continuous
#' bathymetry <- oceandatr::get_bathymetry(spatial_grid = dat_PUs, keep = FALSE,
#' classify_bathymetry = FALSE)
#' splnr_plot(df = bathymetry, col_names = "bathymetry", plot_title = "bathymetry", legend_title = "Bathymetry (m)")
#'
#' # Multi binary features
#' splnr_plot(dat_species_bin, showFeatureSum = TRUE, legend_title = "Number of features")
splnr_plot <- function(df,
                       col_names = NULL,
                       paletteName = "YlGnBu",
                       colorVals = NULL,
                       plot_title = "",
                       showFeatureSum = FALSE,
                       legend_title = "Legend",
                       legend_labels = NULL) {

  # Assertions
  assertthat::assert_that(
    is.data.frame(df),
    "geometry" %in% names(df),
    all(c("xmin", "xmax", "ymin", "ymax") %in% names(sf::st_bbox(df))),
    is.null(col_names) | is.character(col_names),
    is.character(paletteName),
    is.logical(showFeatureSum),
    is.character(legend_title),
    is.null(legend_labels) | is.character(legend_labels)
  )

  gg <- ggplot2::ggplot() +
    ggplot2::coord_sf(xlim = sf::st_bbox(df)$xlim, ylim = sf::st_bbox(df)$ylim) +
    ggplot2::labs(subtitle = plot_title) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(linetype = 0),
                                                 nrow = 2,
                                                 order = 1,
                                                 direction = "horizontal",
                                                 title = legend_title,
                                                 title.position = "top",
                                                 title.hjust = 0.5,
                                                 labels = legend_labels))

  if (showFeatureSum) {
    # Calculate feature sum if requested
    df <- df %>%
      dplyr::as_tibble() %>%
      dplyr::select(-tidyselect::any_of(c("cellID"))) %>%
      dplyr::mutate(FeatureSum = rowSums(dplyr::across(tidyselect::where(is.numeric)), na.rm = TRUE)) %>%
      sf::st_as_sf(sf_column_name = "geometry") %>%
      dplyr::select("FeatureSum")

    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$FeatureSum), colour = NA, size = 0.1) +
      ggplot2::scale_fill_distiller(
        name = legend_title,
        palette = paletteName,
        aesthetics = c("fill"),
        oob = scales::squish
      ) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(order = -1))

    return(gg)
  }

  if (!is.null(col_names)) {
    if (!all(col_names %in% colnames(df))) {
      stop("Invalid column name(s).")
    }

    # Replace NA with 0 in selected columns for binary data verification
    df <- df %>%
      dplyr::mutate_at(col_names, ~tidyr::replace_na(., 0))

    is_binary <- all(sapply(col_names, function(col_name) all(df[[col_name]] %in% c(0, 1))))

    if (is_binary) {
      # We build a new column that add a 1 if a 1 is already present in one of the selected columns.
      # ex : col1 = 1, col2 = 1, col3 = 1, UnionColumn = 1
      # ex : col1 = 0, col2 = 0, col3 = 0, UnionColumn = 0
      # ex : col1 = 1, col2 = 0, col3 = 1, UnionColumn = 1
      df$UnionColumn <- apply(df[, col_names, drop = FALSE], 1, function(x) as.numeric(any(x == 1, na.rm = TRUE)))

      gg <- gg +
        ggplot2::geom_sf(data = df, ggplot2::aes(fill = factor(.data$UnionColumn)), colour = "grey80", size = 0.1) +
        ggplot2::scale_fill_manual(values = c("0" = "#c6dbef", "1" = "#3182bd"), name = "Binary", labels = legend_labels)
    } else {
      for (col_name in col_names) {
        gg <- gg +
          ggplot2::geom_sf(data = df, ggplot2::aes(fill = !!rlang::sym(col_name)), colour = "grey80", size = 0.1) +
          ggplot2::scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(9, paletteName)), limits = NULL) +
          ggplot2::guides(fill = ggplot2::guide_colourbar(order = 1))
      }
    }
  } else if (!showFeatureSum) {
    stop("Please specify the necessary arguments.")
  }

  return(gg)
}
