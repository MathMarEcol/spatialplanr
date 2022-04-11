



#' Assign geomorphic features to each planning unit.
#'
#' Options for `Feature` are:
#' "Abyss", "Abyss_Hills", "Abyss_Mountains", "Abyss_Plains", "Basin",
#' "Bridge", "Canyon", "Escarpment", "Fan", "Glacial Trough", "Guyot",
#' "Hadal", "Plateau", "Ridge", "Rift Valley", "Rise", "Seamount",
#' "Shelf", "Shelf Valley", "Shelf_medium", "Shelf_high", "Shelf_low",
#' "Sill", "Slope", "Spreading Ridge", "Terrace", "Trench", "Trough".
#'
#' For more information look at: https://www.bluehabitats.org
#'
#' Seafloor Geomorphic Features Map by Harris, P.T., Macmillan-Lawler, M., Rupp, J. and Baker, E.K. 2014.
#' Geomorphology of the oceans. Marine Geology, 352: 4-24.
#' Licensed under a Creative Commons Attribution 4.0 International License
#'
#' @param PUs Planning units sf object
#' @param Feature A vector of the geomprphic features you wish to retain. Defaults to all.
#' @param Direc The directory where the MME data is being stored. If not specified, the default location is assumed.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{features <- SpatPlan_Get_Geomorphic(PUs, Feature = "Seamount")}
#' @importFrom rlang .data
#' @importFrom rlang :=
SpatPlan_Get_Geomorphic <- function(PUs,
                                    Feature = c("Abyss", "Abyss_Hills", "Abyss_Mountains", "Abyss_Plains", "Basin",
                                                "Bridge", "Canyon", "Escarpment", "Fan", "Glacial Trough", "Guyot",
                                                "Hadal", "Plateau", "Ridge", "Rift Valley", "Rise", "Seamount",
                                                "Shelf", "Shelf Valley", "Shelf_medium", "Shelf_high", "Shelf_low",
                                                "Sill", "Slope", "Spreading Ridge", "Terrace", "Trench", "Trough"),
                                    Direc = file.path("~", "SpatPlan_Data")){

  dat <- readRDS(file.path(Direc, "GeomorphicData.rds")) %>%
    dplyr::filter(.data$Geomorphic %in% Feature) %>%
    dplyr::select(.data$Geomorphic) %>%
    sf::st_transform(sf::st_crs(PUs)) %>%
    sf::st_make_valid() %>%
    sf::st_crop(PUs)


  for (f in 1:length(Feature)){
    PUs <- SpatPlan_Convert_Poly2PUs(dat %>% dplyr::filter(.data$Geomorphic == Feature[f]) %>% dplyr::rename(!!Feature[f] := .data$Geomorphic),
                                     PUs, binary = TRUE)
  }

  return(PUs)

}
