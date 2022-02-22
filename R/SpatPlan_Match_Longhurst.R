#' Assign Longhurst Provinces to PUs
#'
#' @param PUs
#' @param cCRS
#' @param Direc
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Match_Longhurst <- function(PUs,
                                     cCRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                     Direc = file.path("~", "SpatPlan_Data")){


  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  longh <- sf::st_read(file.path(Direc,"LonghurstProvinces","Longhurst_world_v4_2010.shp")) %>%
    sf::st_transform(cCRS)

  nr <- sf::st_nearest_feature(PUs, longh)

  LPs <- PUs %>%
    dplyr::mutate(ProvCode = longh$ProvCode[nr],
                  ProvDescr = longh$ProvDescr[nr])

  return(LPs)
}
