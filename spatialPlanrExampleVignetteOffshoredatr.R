#Test vignette with data from offshoredatr

cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs" # Mollweide

library(offshoredatr)

Australia_eez <- offshoredatr::get_area(area_name = "Australia") %>%
  sf::st_transform(cCRS) %>%
  dplyr::select("geometry", "territory1")

Australia_PUs <- offshoredatr::get_planning_grid(area_polygon = Australia_eez,
                                                        projection_crs = cCRS,
                                                        resolution_km = 100,
                                                        option = "sf_hex") #WORKS BUT INCLUDES CELLS THAT ARE MAINLY LAND!!!

landmass <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_transform(cCRS)
(ggPU <- splnr_plot_PUs(Australia_PUs, landmass)) # Plot Planning Units

### get Feature data
# geomorphology <- offshoredatr::get_geomorphology(area_polygon = Australia_eez,
#                                                  planning_grid = Australia_PUs) #Doesnt work because it doesnt take crs as an input to make the crs of the geomorphic features the same as the input

# knolls <- offshoredatr::get_knolls(area_polygon = Australia_eez,
#                                                  planning_grid = Australia_PUs) #Doesnt work because it doesnt take crs as an input to make the crs of the geomorphic features the same as the input


#Error only when sf input; spat raster has st_transform in it
bermuda_eez <- get_area(area_name = "Bermuda")
# Specify projection
projection <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal", GEOGCS["GCS_WGS_1984", DATUM["D_WGS_1984", SPHEROID["WGS_1984",6378137.0,298.257223563]], PRIMEM["Greenwich",0.0], UNIT["Degree",0.0174532925199433]], PROJECTION["Lambert_Azimuthal_Equal_Area"], PARAMETER["False_Easting",0.0], PARAMETER["False_Northing",0.0], PARAMETER["Central_Meridian",-64.5], PARAMETER["Latitude_Of_Origin",32], UNIT["Meter",1.0]]'
# Create planning area
bermuda_planning_area <- get_planning_grid(bermuda_eez, projection)

geomorphology <- offshoredatr::get_geomorphology(area_polygon = bermuda_eez,
                                                 planning_grid = bermuda_planning_area) #Doesnt work because it doesnt take crs as an input to make the crs of the geomorphic features the same as the input
