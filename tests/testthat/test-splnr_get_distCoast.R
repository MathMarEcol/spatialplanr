testthat::test_that("Correct function output", {
  crs <- "ESRI:54009" 
  grid <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))))) %>%
    sf::st_set_crs(crs) %>%
    sf::st_cast("POLYGON") %>%
    dplyr::mutate(cellID = dplyr::row_number())
  result <- splnr_get_distCoast(grid)
  
  expect_s3_class(result, "sf")
})

testthat::test_that("Correct function output", {
  crs_2 <- sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
  bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 3, ymax = 3))
  grid_2 <- sf::st_make_grid(bbox, n = c(3, 3), what = "polygons") 
  grid_2 <- sf::st_sf(cellID = 1:length(grid_2), geometry = grid_2) %>%
    sf::st_set_crs(crs_2)
  result <- splnr_get_distCoast(grid_2)
  
  expect_s3_class(result, "sf")
})

testthat::test_that("Correct function input", {
  non_sf_object <- data.frame(x = 1:10, y = 11:20)
  
  expect_error(splnr_get_distCoast(non_sf_object), "Input data should be an 'sf' spatial object.")
})

