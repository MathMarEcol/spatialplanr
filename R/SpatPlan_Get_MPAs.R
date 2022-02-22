
# Status <- c("Designated", "Adopted", "Inscribed", "Established")
# Parent <-
# Desig <- c("National", "Regional", "International", "Not Applicable")

#
#' Get locked in areas
#'
#' @param PUs
#' @param cCRS
#' @param Status
#' @param Desig
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Get_MPAs <- function(PUs,
                               cCRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                               Status = "Designated",
                               Desig = c("National", "Regional", "International", "Not Applicable"),
                               Direc = file.path("~", "SpatPlan_Data")){

  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  MPAs <- readr::read_rds(file.path(Direc, "MPAs.rds")) %>%
    sf::st_transform(cCRS) %>% # Transform if need to
    dplyr::filter(STATUS %in% Status & DESIG_TYPE %in% Desig)

  LockedIn_vec <- PUs %>%
    sf::st_centroid() %>% #Second, get all the pu's with < 50 % area on land (approximated from the centroid)
    sf::st_within(MPAs, sparse = FALSE) %>%
    rowSums() %>%
    as.logical()

  LockedIn <- PUs %>%
    dplyr::mutate(locked_in = LockedIn_vec)

  return(LockedIn)

}
