
targets <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)


testthat::test_that("Correct function output", {
  expect_true(
    rlang::is_list(splnr_climate_priorityAreaApproach(
      features = dat_species_bin,
      metric = dat_clim,
      targets = targets,
      direction = -1))
    , "sf"
  )
})



testthat::test_that("Correct function output", {
  expect_true(
    rlang::is_list(
    splnr_climate_featureApproach(
      features = dat_species_bin,
      metric = dat_clim,
      targets = targets,
      direction = 1))
    , "sf"
  )
})


testthat::test_that("Correct function output", {
  expect_true(
    rlang::is_list(
    splnr_climate_percentileApproach(
      features = dat_species_bin,
      metric = dat_clim,
      targets = targets,
      direction = 1))
    , "sf"
  )
})

