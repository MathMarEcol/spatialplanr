

#' Reprocess the geomorphic data to rds to make it easier to use.
#'
#' @param Direc The directory where the MME data is being stored. If not specified, the default location is assumed.
#'
#' @export
#'
#' @examples
#'
#' @importFrom rlang .data
SpatPlan_Process_Geomorphic <- function(Direc = file.path("~", "SpatPlan_Data")){

  Layers <- c("Abyss", "Abyssal_Classification", "Basins", "Bridges", "Canyons",
              "Escarpments", "Fans", "Glacial_troughs", "Guyots", "Hadal", "Plateaus",
              "Ridges", "Rift_valleys", "Rises", "Seamounts", "Shelf_Classification",
              "Shelf_valleys", "Shelf", "Sills", "Slope", "Spreading_ridges", "Terraces",
              "Trenches", "Troughs")

  fd <- do.call(paste0, expand.grid(file.path(Direc, "Geomorphic"), .Platform$file.sep, Layers, ".shp"))

  dat <- purrr::map_dfr(.x = fd, .f = ~sf::st_make_valid(sf::st_read(.x))) # 152079

  Shelf <- dat %>%
    dplyr::filter(.data$Geomorphic == "Shelf" & .data$Class != "NA") %>%
    dplyr::mutate(Geomorphic = paste(.data$Geomorphic, .data$Class, sep = "_"))

  Abyss <- dat %>%
    dplyr::filter(.data$Geomorphic == "Abyss" & .data$Class != "NA") %>%
    dplyr::mutate(Geomorphic = paste(.data$Geomorphic, .data$Class, sep = "_"))


  dat2 <- dat %>%
    dplyr::filter(is.na(.data$Class)) %>%
    dplyr::bind_rows(.data$Shelf) %>%
    dplyr::bind_rows(.data$Abyss)

  if (nrow(dat2) != nrow(dat)){
    stop("The number of rows in dat2 is incorrect. Check filtering")
    }

  saveRDS(dat2, "~/SpatPlan_Data/GeomorphicData.rds")

  return(invisible(NULL)) # Don't return anything.
}
