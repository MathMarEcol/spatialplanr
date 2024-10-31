## ----setup_chunks, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("GlobalFishingWatch/gfwr")

## ----setup--------------------------------------------------------------------
library(gfwr)
library(spatialplanr)

## ----eval=FALSE---------------------------------------------------------------
#  usethis::edit_r_environ()

## ----eval=FALSE---------------------------------------------------------------
#  key <- gfwr::gfw_auth()

## ----results='hide'-----------------------------------------------------------
region_id <- gfwr::get_region_id(region_name = "Australia", 
                                 region_source = "EEZ",
                                 key = gfwr::gfw_auth())$id

## ----eval=FALSE, message=FALSE------------------------------------------------
#  gfwr::get_raster(
#    spatial_resolution = "LOW",
#    temporal_resolution = "MONTHLY",
#    group_by = "FLAGANDGEARTYPE",
#    start_date = "2022-01-01",
#    end_date = "2023-01-01",
#    region = region_id,
#    region_source = "EEZ",
#    key = gfwr::gfw_auth()
#  )

## ----message=FALSE------------------------------------------------------------
data_sf_combined <- splnr_get_gfw(region = "Australia", 
                                  start_date = "2019-01-01",
                                  end_date =  "2023-12-31",
                                  temp_res = "YEARLY",
                                  spat_res = "LOW",
                                  compress = FALSE)

## ----message = FALSE, results='hide'------------------------------------------
# Check and modify if necessary the spatial reference of data_sf_combined
data_sf_combined <- sf::st_set_crs(data_sf_combined, 
                                   sf::st_crs(rnaturalearth::ne_coastline(scale = "large")))

coast_clipped <- rnaturalearth::ne_coastline(scale = "large") %>%
  sf::st_as_sf() %>%
  sf::st_intersection(sf::st_as_sfc(sf::st_bbox(data_sf_combined)))

# Load EEZ polygons
eezs <- spatialgridr::get_boundary(name = "Australia", type = "eez", country_type = "country") %>%
  sf::st_transform(crs = sf::st_crs(data_sf_combined)) %>%
  sf::st_make_valid() %>%
  sf::st_intersection(sf::st_as_sfc(sf::st_bbox(data_sf_combined)))

## ----echo=FALSE---------------------------------------------------------------
main_plot <- ggplot2::ggplot(data_sf_combined) +
  ggplot2::geom_sf(ggplot2::aes(color = log10(ApparentFishingHrs))) +
  ggplot2::geom_sf(data = coast_clipped, color = "black", fill = NA) + # Add coastline
  ggplot2::geom_sf(data = eezs, fill = NA, color = "red") + # Add the EEZ with hatching
  ggplot2::scale_color_viridis_c(guide = "legend") +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "2022 Vessel Activity Map", 
                subtitle = "Fishing Hours recorded by GFW in Australia", 
                color = "Fishing Hours (log10)") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 8),
    legend.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::guides(color = ggplot2::guide_colorbar(
    title.position = "top",
    title.vjust = 0.5,
    title.hjust = -0.5,
    label.theme = ggplot2::element_text(size = 8),
    barwidth = 5,
    barheight = 0.5
  ))

# The display and writing in this section is for information purposes only, to understand how the information on the grid is translated.
overlay_plot <- ggplot2::ggplot(data_sf_combined) +
  ggplot2::geom_rect(ggplot2::aes(xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf), fill = "white") +
  ggplot2::geom_sf(ggplot2::aes(color = log10(ApparentFishingHrs))) +
  ggplot2::geom_sf(data = coast_clipped, color = "black", fill = NA) + # Add coastline
  ggplot2::geom_sf(data = eezs, fill = NA, color = "red") +
  ggplot2::scale_color_viridis_c(guide = "legend") +
  ggplot2::labs(title = "Vessel Activity Map in Australia between 2019 and 2023", 
                subtitle = "Fishing Hours data recorded by GFW", 
                color = "Fishing Hours \n (log10)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  ggplot2::coord_sf(xlim = c(152, 155), ylim = c(-27, -29))

main_plot +
  ggplot2::annotation_custom(
    ggplot2::ggplotGrob(overlay_plot),
    xmin = 130,
    xmax = 170,
    ymin = -20,
    ymax = -36
  )

## ----echo=FALSE---------------------------------------------------------------
ggplot2::ggplot(data_sf_combined) +
  ggplot2::geom_sf(ggplot2::aes(color = as.factor(Year))) +
  ggplot2::geom_sf(data = coast_clipped, color = "black", fill = NA) + # Add coastline
  ggplot2::geom_sf(data = eezs, color = "red", fill = NA) + # Add the EEZ
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_d(guide = "legend") +
  ggplot2::labs(title = "Vessel Activity Map in Australia between 2019 and 2024", subtitle = "Fishing Hours data recorded by GFW", color = "Years") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 8),
    legend.title = ggplot2::element_text(size = 10)
  )

## ----message=FALSE------------------------------------------------------------
# We need to change the temporal range according to our need group by it to display the total fishing hours. <br>
data_sf_combined <- splnr_get_gfw(region = "Australia", 
                                  start_date = "2019-01-01", 
                                  end_date = "2023-12-31", 
                                  temp_res = "MONTHLY", 
                                  key = gfwr::gfw_auth()) %>%
  dplyr::group_by(Year, Month) %>%
  dplyr::summarize(Total_Fishing_Hours = sum(ApparentFishingHrs))

## ----echo=FALSE---------------------------------------------------------------
ggplot2::ggplot(data_sf_combined, ggplot2::aes(x = Month, y = Total_Fishing_Hours, color = Year, group = Year)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "Total Fishing Hours per month (2014-2023)",
    x = "Month", y = "Total Fishing Hours"
  ) +
  ggplot2::theme_minimal()

## ----message=FALSE------------------------------------------------------------
data_sf_combined <- splnr_get_gfw(region = "Micronesia", 
                                  start_date = "2019-12-31", 
                                  end_date = "2021-01-01", 
                                  temp_res = "MONTHLY")

## ----echo=FALSE, message=FALSE, results='hide'--------------------------------
# Check and modify if necessary the spatial reference of data_sf_combined
data_sf_combined <- sf::st_set_crs(data_sf_combined, sf::st_crs(rnaturalearth::ne_coastline(scale = "large")))

coast_clipped <- rnaturalearth::ne_coastline(scale = "large") %>%
  sf::st_as_sf() %>%
  sf::st_intersection(sf::st_as_sfc(sf::st_bbox(data_sf_combined)))

# Load EEZ polygons
eezs <- spatialgridr::get_boundary(name = "Micronesia", type = "eez", country_type = "country") %>%
  sf::st_transform(crs = sf::st_crs(data_sf_combined)) %>%
  sf::st_make_valid() %>%
  sf::st_intersection(sf::st_as_sfc(sf::st_bbox(data_sf_combined)))

## ----echo=FALSE---------------------------------------------------------------
# Create the map
ggplot2::ggplot(data_sf_combined) +
  ggplot2::geom_sf(ggplot2::aes(color = `Geartype`)) +
  ggplot2::geom_sf(data = coast_clipped, color = "black", fill = NA) + # Add coastline
  ggplot2::geom_sf(data = eezs, color = "red", fill = NA) + # Ajouter la EEZ avec hachures
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "2020 Vessel Activity Map", subtitle = "recorded by GFW in Micronesia", color = "Gear types") +
  ggplot2::theme(legend.position = "right")

## ----echo=FALSE, message=FALSE, results='hide'--------------------------------
data_sf_combined <- splnr_get_gfw(region = "Papua New Guinea", 
                                  start_date = "2019-12-31", 
                                  end_date = "2021-01-01", 
                                  temp_res = "YEARLY", 
                                  spat_res = "LOW") %>%
  sf::st_set_crs(sf::st_crs(rnaturalearth::ne_coastline(scale = "large")))

coast_clipped <- rnaturalearth::ne_coastline(scale = "large") %>%
  sf::st_as_sf() %>%
  sf::st_intersection(sf::st_as_sfc(sf::st_bbox(data_sf_combined)))

# Load EEZ polygons
eezs <- spatialgridr::get_boundary(name = "Papua New Guinea", type = "eez", country_type = "country") %>%
  sf::st_transform(crs = sf::st_crs(data_sf_combined)) %>%
  sf::st_make_valid() %>%
  sf::st_intersection(sf::st_as_sfc(sf::st_bbox(data_sf_combined)))

## ----echo=FALSE---------------------------------------------------------------
# Create the map
ggplot2::ggplot(data_sf_combined) +
  ggplot2::geom_sf(ggplot2::aes(color = `Flag`)) +
  ggplot2::geom_sf(data = coast_clipped, color = "black", fill = NA) + # Add coastline
  ggplot2::geom_sf(data = eezs, color = "red", fill = NA) + # Add EEZ
  ggplot2::scale_size_continuous(range = c(1, 10), guide = "legend", name = "Flag") +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "2021 Vessel Activity Map", subtitle = "recorded by GFW in Papua New Guinea", color = "Flag") +
  ggplot2::theme(legend.position = "right")

