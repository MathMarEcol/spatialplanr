
#' Plot prioritizr solution
#'
#' @param soln The `prioritizr` solution
#' @param PlanUnits Planning Units as an `sf` object
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
SpatPlan_Plot_Solution <- function(soln, PlanUnits, landmass){

  soln <- soln %>%
    dplyr::select(.data$solution_1) %>%
    dplyr::mutate(solution_1 = as.logical(.data$solution_1)) # Making it logical helps with the plotting

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$solution_1), colour = NA, size = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "blue",
                                            "FALSE" = "white"),
                                 aesthetics = c("colour", "fill")) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Solution")

}


#' Plot Planning Units
#'
#' @param PlanUnits Planning Units as an `sf` object
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
SpatPlan_Plot_PUs <- function(PlanUnits, landmass){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Planning Units")

}


#' Plot MPAs
#'
#' @param df An `sf` object of marine protected areas
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
SpatPlan_Plot_MPAs <- function(df, landmass){

  if (class(df$wdpa) != "logical"){
    df <- df %>%
      dplyr::mutate(wdpa = as.logical(.data$wdpa))
  }

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$wdpa), colour = "lightblue", size = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "blue",
                                            "FALSE" = "grey50")) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "blue",
                                          "FALSE" = "white")) +
    ggplot2::theme_bw() +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(df)$xlim,
      ylim = sf::st_bbox(df)$ylim) +
    ggplot2::labs(subtitle = "Locked In Areas")

}


#' Plot cost
#'
#' @param Cost An `sf` object of cost for `prioritizr`
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
SpatPlan_Plot_Cost <- function(Cost, landmass){

  col_name = stringr::str_subset(colnames(Cost), "geometry", negate = TRUE)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = Cost, ggplot2::aes_string(fill = col_name), colour = "grey80", size = 0.1, show.legend = TRUE) +
    ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(Cost)$xlim, ylim = sf::st_bbox(Cost)$ylim) +
    ggplot2::scale_fill_distiller(palette = "YlGnBu",
                                  aesthetics = c("colour", "fill"),
                                  limits = c(0,
                                             as.numeric(stats::quantile(dplyr::pull(Cost,col_name), 0.99))),
                                  oob = scales::squish) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Cost (USD)")

}


#' Plot solution comparison
#'
#' @param soln1 The first `prioritizr` solution
#' @param soln2 The second `prioritizr` solution
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' @importFrom rlang .data
SpatPlan_Plot_Comparison <- function(soln1, soln2, landmass){

  soln <- soln1 %>%
    dplyr::select(.data$solution_1) %>%
    dplyr::bind_cols(soln2 %>%
                       dplyr::as_tibble() %>%
                       dplyr::select(.data$solution_1) %>%
                       dplyr::rename(solution_2 = .data$solution_1)) %>%
    dplyr::mutate(Combined = .data$solution_1 + .data$solution_2) %>%
    dplyr::mutate(Compare = dplyr::case_when(Combined == 2 ~ "Same",
                                             solution_1 == 1 & solution_2 == 0 ~ "Removed (-)",
                                             solution_1 == 0 & solution_2 == 1 ~ "Added (+)"),
                  Compare = factor(.data$Compare, levels = c("Added (+)", "Same", "Removed (-)"))) %>%
    dplyr::filter(!is.na(.data$Compare))

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$Compare), colour = NA, size = 0.0001) +
    ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = c("Added (+)" = "Red", "Same" = "ivory3", "Removed (-)" = "Blue"), drop = FALSE)

}


#' Plot number of features
#'
#' @param df An `sf` object of features
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
SpatPlan_Plot_FeatureNo <- function(df, landmass){

  df <- df %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(FeatureSum = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE)) %>%
    sf::st_as_sf(sf_column_name = "geometry") %>%
    dplyr::select(.data$FeatureSum)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$FeatureSum), colour = "grey80", size = 0.1, show.legend = TRUE) +
    # ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(df)$xlim, ylim = sf::st_bbox(df)$ylim) +
    ggplot2::scale_fill_distiller(palette = "YlGnBu",
                                  aesthetics = c("fill"),
                                  # limits = c(0,
                                  #            as.numeric(quantile(Cost$Cost, 0.99))),
                                  oob = scales::squish,
                                  trans = "log10") +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Number of Features")

}

#' Plot Longhurst Provinces
#'
#' @param PlanUnits Planning Units as an `sf` object
#' @param landmass An `sf` object of land polygon
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
SpatPlan_Plot_Longhurst <- function(PlanUnits, landmass){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = PlanUnits, colour = "lightblue", ggplot2::aes(fill = .data$ProvDescr), size = 0.1, show.legend = TRUE) +
    ggplot2::geom_sf(data = landmass, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Longhurst Provinces")

}
