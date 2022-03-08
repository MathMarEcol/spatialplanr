
#' Plot prioritizr solution
#'
#' @param s1
#' @param PlanUnits
#' @param world
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Plot_Solution <- function(s1, PlanUnits, world){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = s1, ggplot2::aes(fill = solution_1), colour = NA, size = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "blue",
                                            "FALSE" = "white"),
                                 aesthetics = c("colour", "fill")) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Solution")

}


#' Plot Planning Units
#'
#' @param PlanUnits
#' @param world
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Plot_PUs <- function(PlanUnits, world){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Planning Units")

}


#' Plot MPAs
#'
#' @param LockedIn
#' @param world
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Plot_MPAs <- function(LockedIn, world){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = LockedIn, ggplot2::aes(fill = locked_in), colour = "lightblue", size = 0.1, show.legend = FALSE) +
    ggplot2::geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "blue",
                                            "FALSE" = "grey50")) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "blue",
                                          "FALSE" = "white")) +
    ggplot2::theme_bw() +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(LockedIn)$xlim,
      ylim = sf::st_bbox(LockedIn)$ylim) +
    ggplot2::labs(subtitle = "Locked In Areas")

}


#' Plot cost
#'
#' @param Cost
#' @param world
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Plot_Cost <- function(Cost, world){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = Cost, ggplot2::aes(fill = Cost), colour = "grey80", size = 0.1, show.legend = TRUE) +
    ggplot2::geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(Cost)$xlim, ylim = sf::st_bbox(Cost)$ylim) +
    cmocean::scale_fill_cmocean(name = "deep",
                                aesthetics = c("colour", "fill"),
                                limits = c(0,
                                           as.numeric(quantile(Cost$Cost, 0.99))),
                                oob = scales::squish) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Cost (USD)")

}


#' Plot solution comparison
#'
#' @param soln1
#' @param soln2
#' @param world
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Plot_Comparison <- function(soln1, soln2, world){

  soln <- soln1 %>%
    dplyr::select(solution_1) %>%
    dplyr::bind_cols(soln2 %>%
                       dplyr::as_tibble() %>%
                       dplyr::select(solution_1) %>%
                       dplyr::rename(solution_2 = solution_1)) %>%
    dplyr::mutate(Combined = solution_1 + solution_2) %>%
    dplyr::mutate(Compare = dplyr::case_when(Combined == 2 ~ "Same",
                                             solution_1 == 1 & solution_2 == 0 ~ "Removed (-)",
                                             solution_1 == 0 & solution_2 == 1 ~ "Added (+)"),
                  Compare = factor(Compare, levels = c("Added (+)", "Same", "Removed (-)"))) %>%
    dplyr::filter(!is.na(Compare))

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = Compare), colour = NA, size = 0.0001) +
    ggplot2::geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = c("Added (+)" = "Red", "Same" = "ivory3", "Removed (-)" = "Blue"), drop = FALSE)

}


#' Plot number of features
#'
#' @param df
#' @param world
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Plot_FeatureNo <- function(df, world){

  df <- df %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(FeatureSum = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE)) %>%
    sf::st_as_sf(sf_column_name = "geometry") %>%
    dplyr::select(FeatureSum)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = FeatureSum), colour = "grey80", size = 0.1, show.legend = TRUE) +
    ggplot2::geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(df)$xlim, ylim = sf::st_bbox(df)$ylim) +
    cmocean::scale_fill_cmocean(name = "deep",
                                aesthetics = c("fill"),
                                # limits = c(0,
                                # as.numeric(quantile(Cost$Cost, 0.99))),
                                oob = scales::squish,
                                trans = "log10") +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Number of Features")

}

#' Plot Longhurst Provinces
#'
#' @param PlanUnits
#' @param world
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Plot_Longhurst <- function(PlanUnits, world){
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = PlanUnits, colour = "lightblue", ggplot2::aes(fill = ProvDescr), size = 0.1, show.legend = TRUE) +
    ggplot2::geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PlanUnits)$xlim, ylim = sf::st_bbox(PlanUnits)$ylim) +
    ggplot2::theme_bw() +
    ggplot2::labs(subtitle = "Longhurst Provinces")

}
