#' Add-ons for plotting
#'
#' This function allows to customise plots in a simple and reproducible way, by
#' giving the option for several inputs that can be included in maps produced
#' with the other functions of this package.It can be combined with the
#' `spatialplanr` spatial plotting functions.
#'
#' @param PUs Planning Units as an `sf` object
#' @param colorPUs A color value for the outline of planning units.
#' @param Bndry The planning region boundaries as an `sf` object
#' @param colorBndry A color value for the outline of the boundary.
#' @param overlay An `sf` object of overlay polygon.
#' @param colorOverlay A color value for overlay.
#' @param overlay2 An `sf` object of overlay polygon.
#' @param colorOverlay2 A color value for overlay.
#' @param overlay3 An `sf` object of overlay polygon.
#' @param colorOverlay3 A color value for overlay.
#' @param cropOverlay An `sf` object with the boundary box used for cropping the
#'   overlay object.
#' @param contours An `sf` object of contours that are important to visualise
#'   (e.g. outline of sea mounts, ridges; can be produced with
#'   terra::as.contour()); up to 6 different contours possible.
#' @param colorConts A color value for contours.
#' @param lockIn An `sf` object with binary data of locked in areas in
#'   the prioritisation (e.g. MPAs).
#' @param typeLockIn Either "Full" or "Contours"; "Full" maps the locked in areas on
#'   top of the planning units; "Contours" draws the outline of the locked in
#'   areas.
#' @param nameLockIn column of data frame that contains binary information of
#'   the locked in areas to plot
#' @param alphaLockIn A value (0-1) for the opacity of the locked in areas when
#'   plotted on top of other plots.
#' @param colorLockIn A color value for the locked in areas.
#' @param legendLockIn A character value for the title of the legend of the locked in
#'   areas. Can be empty ("").
#' @param labelLockIn The legend label of the locked in area (e.g. MPAs)
#' @param ggtheme The theme applied to the plot. Can either be NA (default
#'   ggplot), "Default" (default spatialplanr: theme_bw() and some basic theme
#'   settings) or a user-defined list of theme properties.
#'
#' @return A ggplot object of the plot
#' @export
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
#' splnr_plot_solution(dat_soln) +
#'   splnr_gg_add(PUs = dat_PUs, ggtheme = "Default")
splnr_gg_add <- function(PUs = NULL, colorPUs = "grey80",
                         Bndry = NULL, colorBndry = "black",
                         overlay = NULL, colorOverlay = "grey20",
                         overlay2 = NULL, colorOverlay2 = "grey30",
                         overlay3 = NULL, colorOverlay3 = "grey40",
                         contours = NULL, colorConts = "black",
                         cropOverlay = NULL,
                         lockIn = NULL, typeLockIn = "Full", nameLockIn = NULL,
                         alphaLockIn = 0.5, colorLockIn = "black", legendLockIn = "",
                         labelLockIn = "MPAs",
                         ggtheme = "Default" # splnr_theme
) {

  if(!is.null(PUs)){assertthat::assert_that(inherits(PUs, "sf"))}
  if(!is.null(Bndry)){assertthat::assert_that(inherits(Bndry, "sf"))}
  if(!is.null(overlay)){assertthat::assert_that(inherits(overlay, "sf"))}
  if(!is.null(overlay2)){assertthat::assert_that(inherits(overlay2, "sf"))}
  if(!is.null(overlay3)){assertthat::assert_that(inherits(overlay3, "sf"))}
  if(!is.null(contours)){assertthat::assert_that(inherits(contours, "sf"))}
  if(!is.null(lockIn)){assertthat::assert_that(inherits(lockIn, "sf"))}

  ggList <- list()

  if (inherits(PUs, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = PUs, colour = colorPUs, fill = NA, size = 0.1, show.legend = FALSE),
      ggplot2::coord_sf(xlim = sf::st_bbox(PUs)$xlim, ylim = sf::st_bbox(PUs)$ylim)
    )
  }

  if (inherits(Bndry, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = Bndry, colour = colorBndry, size = 0.4, fill = NA, show.legend = FALSE),
      ggplot2::coord_sf(xlim = sf::st_bbox(Bndry)$xlim, ylim = sf::st_bbox(Bndry)$ylim)
    )
  }

  if (inherits(overlay, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay, colour = colorOverlay, fill = colorOverlay, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )}

  if (inherits(overlay2, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay2, colour = colorOverlay2, fill = colorOverlay2, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )}

  if (inherits(overlay3, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay3, colour = colorOverlay3, fill = colorOverlay3, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )}


  if (inherits(contours, "sf")) { # needs a geometry col and one names Category that has the wanted contours and their names
    namesConts <- unique(contours$Category)
    contoursRowNum <- length(namesConts)
    vals <- 1:contoursRowNum
    if (length(vals) > 6) {
      cat("Only 6 categories allowed for plotting contours.")
    } else {
      ggList <- c(
        ggList,
        list(
          ggnewscale::new_scale_colour(),
          ggplot2::geom_sf(data = contours, colour = colorConts, fill = NA, ggplot2::aes(linetype = .data$Category), size = 0.5, show.legend = "line"),
          ggplot2::scale_linetype_manual(" ",
                                         breaks = namesConts,
                                         values = vals,
                                         guide = ggplot2::guide_legend(
                                           override.aes = list(fill = NA),
                                           nrow = 2,
                                           direction = "horizontal",
                                           order = 3,
                                           keywidth = grid::unit(0.05, "npc")
                                         )
          )
        )
      )
    }
  }

  if (inherits(lockIn, "sf")) {
    lockIn <- lockIn %>%
      dplyr::mutate(lockedIn = as.logical(.data[[nameLockIn]])) %>%
      dplyr::filter(.data$lockedIn == 1) # TODO Add ability for TRUE as well

    if (typeLockIn == "Full") {
      ggList <- c(
        ggList,
        list(
          ggnewscale::new_scale_fill(),
          ggnewscale::new_scale_colour(),
          ggplot2::geom_sf(data = lockIn, ggplot2::aes(fill = .data$lockedIn), alpha = alphaLockIn),
          ggplot2::scale_fill_manual(
            name = legendLockIn,
            values = c("TRUE" = colorLockIn),
            labels = labelLockIn,
            aesthetics = c("colour", "fill"),
            guide = ggplot2::guide_legend(
              override.aes = list(linetype = 0),
              nrow = 2,
              order = 1,
              direction = "horizontal",
              title.position = "top",
              title.hjust = 0.5
            )
          )
        )
      )
    } else if (typeLockIn == "Contours") {
      lockIn <- lockIn %>%
        sf::st_union() %>%
        sf::st_as_sf() %>%
        dplyr::rename(geometry = "x") %>%
        dplyr::mutate(lockedIn = 1) %>%
        dplyr::mutate(lockedIn = as.factor(.data$lockedIn))

      ggList <- c(
        ggList,
        list(
          ggnewscale::new_scale_fill(),
          ggnewscale::new_scale_colour(),
          ggplot2::geom_sf(data = lockIn, colour = colorLockIn, fill = NA, ggplot2::aes(linetype = .data$lockedIn), size = 0.5, show.legend = "line"),
          ggplot2::scale_linetype_manual("",
                                         values = 1,
                                         labels = labelLockIn,
                                         guide = ggplot2::guide_legend(
                                           override.aes = list(fill = NA),
                                           # nrow = 2,
                                           direction = "horizontal",
                                           # order = 3,
                                           keywidth = grid::unit(0.05, "npc")
                                         )
          )
        )
      )
    }
  }

  if (inherits(cropOverlay, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::coord_sf(xlim = sf::st_bbox(cropOverlay)$xlim, ylim = sf::st_bbox(cropOverlay)$ylim)
    )
  }


  if (inherits(ggtheme, "character")) {
    ggList <- c(
      ggList,
      list(
        ggplot2::theme_bw(),
        ggplot2::theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          text = ggplot2::element_text(size = 20, colour = "black"),
          axis.text = ggplot2::element_text(size = 16, colour = "black"),
          plot.title = ggplot2::element_text(size = 16),
          axis.title = ggplot2::element_blank()
        )
      )
    )
  } else if (inherits(ggtheme, "list")) {
    ggList <- c(ggList, ggtheme)
  } else if (inherits(ggtheme, "logical")) {
    ggList <- ggList
  }
}
