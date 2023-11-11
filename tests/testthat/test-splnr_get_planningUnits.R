
dat_bndry_moll <- dat_bndry %>% sf::st_transform("ESRI:54009")
landmass <- rnaturalearth::ne_countries(returnclass = "sf") %>% sf::st_transform("ESRI:54009")


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_get_planningUnits(dat_bndry_moll, landmass, CellArea = 10000, Shape = "hexagon"), "sf"
  )
})
