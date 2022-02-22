#' Get the boundary of the planning region.
#'
#' @param Limits
#' @param cCRS
#'
#' @return
#' @export
#'
#' @examples
SpatPlan_Get_Boundary <- function(Limits,
                                  cCRS,
                                  Direc = file.path("~", "SpatPlan_Data")){

  if (!file.exists(Direc)) {
    stop(paste("The Data folder does not exist at ",Direc,". Please download from the RDM and then try again. See https://github.com/MathMarEcol/spatialplanr for details."))
  }

  # Create function for creating polygon
  polygon <- function(x){
    x <- x %>%
      as.matrix() %>%
      list() %>%
      sf::st_polygon() %>%
      sf::st_sfc(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
      sf::st_transform(crs = cCRS)
  }

  # if Pacific-centered, two polygons then union them:
  if (is.numeric(Limits) && cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){

    Bndry1 <- dplyr::tibble(x = seq(-180, Limits["xmin"], by = 1), y = Limits["ymin"]) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmin"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(Limits["xmin"], -180, by = -1), y = Limits["ymax"])) %>%
      dplyr::bind_rows(dplyr::tibble(x = -180, y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>%
      polygon()

    Bndry2 <- dplyr::tibble(x = seq(180, Limits["xmax"], by = -1), y = Limits["ymin"]) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(Limits["xmax"], 180, by = 1), y = Limits["ymax"])) %>%
      dplyr::bind_rows(dplyr::tibble(x = 180, y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>%
      polygon()

    Bndry <- sf::st_union(Bndry1, Bndry2)

    return(Bndry)
  }

  if (is.numeric(Limits)){
    Bndry <- dplyr::tibble(x = seq(Limits["xmin"], Limits["xmax"], by = 1), y = Limits["ymin"]) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(Limits["xmax"], Limits["xmin"], by = -1), y = Limits["ymax"])) %>%
      dplyr::bind_rows(dplyr::tibble(x = Limits["xmin"], y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>%
      polygon()

    return(Bndry)
  }

  if (Limits == "Global"){
    Bndry <- dplyr::tibble(x = seq(-180, 180, by = 1), y = -90) %>%
      dplyr::bind_rows(dplyr::tibble(x = 180, y = seq(-90, 90, by = 1))) %>%
      dplyr::bind_rows(dplyr::tibble(x = seq(180, -180, by = -1), y = 90)) %>%
      dplyr::bind_rows(dplyr::tibble(x = -180, y = seq(90, -90, by = -1))) %>%
      polygon()

    return(Bndry)
  }

  # Western Pacific
  if (Limits == "WestPacific"){

    ocean_sf <- rnaturalearth::ne_download(scale = "large", category = "physical", type = "geography_marine_polys", returnclass = "sf")

    # Create the list of water bodies to be included
    ocean_list <- c("NORTH PACIFIC OCEAN", "SOUTH PACIFIC OCEAN", "Philippine Sea",
                    "Coral Sea", "Tasman Sea", "Bay of Plenty", "Cook Strait",
                    "Bismarck Sea", "Solomon Sea", "Bass Strait", "Gulf of Papua",
                    "Banda Sea", "Java Sea", "Celebes Sea", "Makassar Strait",
                    "Molucca Sea", "Halmahera Sea", "Ceram Sea", "Flores Sea")
    # Filter out ocean_list from ocean_sf
    ocean_temp <- ocean_sf %>%
      dplyr::filter(label %in% ocean_list) %>%
      dplyr::select(label, ne_id, geometry)

    projected_ocean <- SpatPlan_Convert_PacificRobinson(ocean_temp) # Project it to Pacific Robinson

    # Call and filter the EEZs
    eez <- sf::st_read(file.path(Direc, "World_EEZ_v11","eez_v11.shp")) %>%
      dplyr::filter(SOVEREIGN1 != "Antarctica") %>%
      dplyr::select(MRGID, GEONAME, TERRITORY1, X_1, Y_1, geometry)

    # Create the list of EEZs
    eez_list <- c("Phoenix Group", "Papua New Guinea", "Guam",
                  "Micronesia", "Nauru", "Solomon Islands", "Vanuatu", "New Caledonia", "Fiji",
                  "Marshall Islands", "Tonga", "Niue", "American Samoa", "Samoa",
                  "Tokelau", "Cook Islands", "French Polynesia", "Northern Mariana Islands", "Gilbert Islands",
                  "Tuvalu", "Wallis and Futuna", "Line Group", "Palau", "Indonesia")

    eez_filtered <- eez %>%
      dplyr::filter(TERRITORY1 %in% eez_list)

    # Create polygons that will be used to include ABNJ (+: east/north; -: west/south)

    polygon_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    polygon1 <- SpatPlan_Get_Boundary(c(xmin = 155, xmax = 176, ymax = 5, ymin = -10), polygon_crs) %>%
      st_as_sf() %>%
      st_make_valid()
    polygon2 <- SpatPlan_Get_Boundary(c(xmin = -161, xmax = -153, ymax = -10, ymin = -19), polygon_crs) %>%
      st_as_sf() %>%
      st_make_valid()
    polygon3 <- SpatPlan_Get_Boundary(c(xmin = 135, xmax = 152, ymax = 6, ymin = 0), polygon_crs) %>%
      st_as_sf() %>%
      st_make_valid()
    polygon4 <- SpatPlan_Get_Boundary(c(xmin = 170, xmax = 176, ymax = -10, ymin = -20), polygon_crs) %>%
      st_as_sf() %>%
      st_make_valid()

    # Include areas in ocean_temp that are within eez_temp & merge ABNJ
    # Note, you might want to increase memory limit here
    # memory.limit(size = 56000) # for Windows
    merged <- SpatPlan_Get_MaskedPolygon(projected_ocean, res = 0.25, mask = eez_filtered, inverse = FALSE)

    merged <- merged %>%  # change to inverse = TRUE, if you want the inverse of the intersection of ocean_temp and eez_temp
      st_union(., polygon1) %>% # merge with all the individual polygons
      st_union(., polygon2) %>%
      st_union(., polygon3) %>%
      st_union(., polygon4)

    # Filter out land masses found within the boundaries of the EEZs of interest
    land_list <- c("Palau", "Guam", "Indonesia", "Northern Mariana Islands", "Federated States of Micronesia", "Papua New Guinea",
                   "Solomon Islands", "New Caledonia", "Vanuatu", "Fiji", "Marshall Islands", "Nauru", "Kiribati", "Tuvalu",
                   "Wallis and Futuna", "Tonga", "Samoa", "American Samoa", "Niue", "Cook Islands",
                   "French Polynesia", "United States Minor Outlying Islands", "Pitcairn Islands")

    land <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>%
      dplyr::filter(name_en %in% land_list)

    indonesia <- eez %>%
      dplyr::filter(TERRITORY1 %in% "Indonesia") # remove Indonesia EEZ

    land_mask <- sf::st_union(land, indonesia) # we also want to remove Indonesia EEZs at this point

    # Finally, remove land_mask from merged to end up with desired planning region
    Bndry <- SpatPlan_Get_MaskedPolygon(merged, res = 0.25, mask = land_mask, inverse = TRUE)

    return(Bndry)
  }

  # Add Pacific
  if (Limits == "Pacific"){

    # This example is for the Pacific Basin: The Pacific Ocean and Marginal Seas
    ocean_sf <- rnaturalearth::ne_download(scale = "large",
                            category = "physical",
                            type = "geography_marine_polys",
                            returnclass = "sf") %>%
      dplyr::filter(name %in% c("North Pacific Ocean", "South Pacific Ocean", "Philippine Sea", "Coral Sea", "Tasman Sea", "South China Sea",
                         "Sea of Japan", "Sea of Okhotsk", "Celebes Sea", "Sulu Sea", "Banda Sea", "Luzon Strait", "Java Sea",
                         "Yellow Sea", "East China Sea", "Arafura Sea", "Timor Sea", "Gulf of Thailand", "Gulf of Carpentaria",
                         "Bay of Plenty", "Molucca Sea", "Bismarck Sea", "Solomon Sea", "Gulf of Tonkin", "Strait of Singapore",
                         "Makassar Strait", "Ceram Sea", "Korea Strait", "Inner Sea", "Taiwan Strait", "Shelikhova Gulf", "Bo Hai",
                         "Great Barrier Reef", "Bering Sea", "Gulf of Alaska", "Kronotskiy Gulf", "Uda Bay", "Uchiura Bay",
                         "Tsugaru Strait", "Tatar Strait", "La Pérouse Strait", "East Korea Bay", "Qiongzhou Strait", "Cook Strait",
                         "Torres Strait", "Gulf of Papua", "Hangzhou Bay", "Karaginskiy Gulf", "Gulf of Kamchatka", "Joseph Bonaparte Gulf",
                         "Gulf of Sakhalin", "Bali Sea", "Davao Gulf", "Halmahera Sea", "Selat Bali", "Gulf of Tomini", "Flores Sea",
                         "Sibuyan Sea", "Selat Dampier", "Gulf of Buli", "Gulf of Kau", "Bohol Sea", "Surigao Strait", "Ragay Gulf",
                         "Samar Sea", "Tayabas Bay", "Leyte Gulf", "Visayan Sea", "Savu Sea", "Yangtze River", "Gulf of Anadyr'",
                         "Golfo de California", "Cook Inlet", "Queen Charlotte Sound", "Bristol Bay", "Dixon Entrance", "Norton Sound",
                         "Prince William Sound", "Smith Sound", "Queen Charlotte Strait", "Baird Inlet", "Hecate Strait", "Cordova Bay",
                         "Columbia River", "Salish Sea", "Golfo de Panamá", "Golfo Corcovado", "Golfo de Penas", "Golfo de Guayaquil",
                         "Golfo de Tehuantepec", "Dixon Entrance", "Smith Sound", "Queen Charlotte Strait", "Cordova Bay" ))

    #########################################################
    # Get world map for plotting
    #########################################################

    res = 0.1 # 0.1 degree raster
    pacific_robinson <- SpatPlan_Create_SinglePolygon(ocean_sf, res) %>%
      SpatPlan_Convert_PacificRobinson()

    return(pacific_robinson)
  }

  # Add Indian


  # Add Atlantic


  # Add EEZs

  if (Limits == "Australia"){
    # Get Australia's EEZ
    Bndry <- sf::st_read(file.path(Direc, "World_EEZ_v11", "eez_v11.shp"), quiet = TRUE) %>%
      dplyr::filter(TERRITORY1 == "Australia") %>%
      sf::st_transform(cCRS)
  }

}
