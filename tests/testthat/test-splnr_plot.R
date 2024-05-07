# Continuous plot of bathymetry


Region <- "Coral Sea" # "Australia"
Type <- "Oceans" # "EEZ"
PU_diam <- 107460 # m2 (10,000 km2)
cCRS <- "ESRI:54009"

Bndry <- splnr_get_boundary(Limits = Region, Type = Type, cCRS = cCRS)

landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

PUs <- spatialgridr::get_grid(area_polygon = Bndry,
                              projection_crs = cCRS,
                              option = "sf_hex",
                              resolution = PU_diam,
                              sf_method = "centroid")

splnr_theme <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    text = ggplot2::element_text(size = 9, colour = "black"),
    axis.text = ggplot2::element_text(size = 9, colour = "black"),
    plot.title = ggplot2::element_text(size = 9),
    axis.title = ggplot2::element_blank()
  )
)


distance <- splnr_get_distCoast(dat_PUs)

testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = dat_species_bin,
               col_names = "Spp1",
               legend_title = "Legend",
               legend_labels = c("Absent", "Present"))
    , "gg"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = distance,
               col_names = "coastDistance_km",
               plot_title = "Distance to Coast",
               legend_title = "Distance (km)")
    , "gg"
  )
})

# Plot Planning Units
testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = dat_PUs)
    , "gg"
  )
})

# Multi binary features
testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot(df = dat_species_bin,
               col_names = colnames(dat_species_bin %>%
                                      sf::st_drop_geometry() %>%
                                      dplyr::select(
                                        tidyselect::starts_with("Spp"))),
               legend_title = "Number of features")

    , "gg"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    ggPU <- splnr_plot(PUs) +
      splnr_gg_add(
        Bndry = Bndry, overlay = landmass,
        cropOverlay = PUs, ggtheme = splnr_theme
      )
    , "gg"
  )
})



testthat::test_that("Correct function output", {
  expect_s3_class(
    ggPU <- splnr_plot(df = PUs) +
      splnr_gg_add(
        Bndry = Bndry, overlay = landmass,
        cropOverlay = PUs, ggtheme = "Default"
      )
    , "gg"
  )
})




# testthat::test_that("Correct function output", {
#   expect_s3_class(
#
#     , "gg"
#   )
# )
# })


