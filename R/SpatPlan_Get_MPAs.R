
# Status <- c("Designated", "Adopted", "Inscribed", "Established")
# Parent <-
# Desig <- c("National", "Regional", "International", "Not Applicable")

#
#' Get locked in areas
#'
#' @param PUs The planning units
#' @param Status The status of the MPA. Options are: "Designated", "Adopted", "Inscribed", "Established"
#' @param Desig The designation of the MPA. Options are "National", "Regional", "International", "Not Applicable"
#' @param Direc The directory where the MME data is being stored. If not specified, the default location is assumed.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom rlang .data
SpatPlan_Get_MPAs <- function(PUs,
                              Status = "Designated",
                              Desig = c("National", "Regional", "International", "Not Applicable"),
                              Direc = file.path("~", "SpatPlan_Data")){

  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  MPAs <- readr::read_rds(file.path(Direc, "MPAs.rds")) %>%
    sf::st_transform(sf::st_crs(PUs)) %>% # Transform if need to
    dplyr::filter(.data$STATUS %in% Status & .data$DESIG_TYPE %in% Desig)

  LockedIn_vec <- PUs %>%
    sf::st_centroid() %>% #Second, get all the pu's with < 50 % area on land (approximated from the centroid)
    sf::st_within(MPAs, sparse = FALSE) %>%
    rowSums() %>%
    as.logical()

  LockedIn <- PUs %>%
    dplyr::mutate(locked_in = LockedIn_vec)

  return(LockedIn)

}
