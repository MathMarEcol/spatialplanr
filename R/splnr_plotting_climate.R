#' Plot climate data
#'
#' @param df An `sf` object with climate metric information with
#' @param colInterest column of data frame that contains the metric informatin
#' @param PlanUnits Planning Units as an `sf` object
#' @param landmass An `sf` object of land polygon
#' @param colorMap A character string indicating the color map to use (see https://ggplot2.tidyverse.org/reference/scale_viridis.html for all options)
#' @param colorPUs A color value for the outline of planning units.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' splnr_plot_climData(dat_clim, dat_clim$metric, dat_PUs)
splnr_plot_climData <- function(df, colInterest, PlanUnits, landmass = NA,
                                  colorMap = "C",
                                  colorPUs = "grey80",
                                  plotTitle = " ", legendTitle = "Climate metric") {

  df <- df %>%
    dplyr::mutate(metric = colInterest)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df %>% sf::st_as_sf(), ggplot2::aes(fill = .data$metric), colour = NA) +
    ggplot2::scale_fill_viridis_c(name = legendTitle,
                                  option = colorMap#,
                                  #guide = ggplot2::guide_colourbar(
                                  # title.position = "bottom",
                                  #  title.hjust = 0.5,
                                  #  order = 1,
                                  #barheight = grid::unit(0.03, "npc"),
                                  #barwidth = grid::unit(0.25, "npc"))
                                  #)
    ) +
    ggplot2::geom_sf(data = PlanUnits, colour = colorPUs, fill = NA, size = 0.1, show.legend = FALSE)

  if (class(landmass)[[1]] == "sf"){
    gg <- gg + ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE)
  }

  gg <- gg +
    ggplot2::theme_bw() +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim)+
    ggplot2::labs(subtitle = plotTitle)

}


