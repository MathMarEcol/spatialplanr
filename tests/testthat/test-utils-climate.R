
Features <- dat_species_bin %>%
  dplyr::select(-"cellID")

target <- Features %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

metric_df <- dat_clim

dat_species_binDF <- dat_species_bin %>%
  sf::st_drop_geometry()



testthat::test_that("Correct function output", {
  expect_true(
    rlang::is_list(splnr_climate_priorityAreaApproach(
      featuresDF = dat_species_bin,
      metricDF = metric_df, targetsDF = target, direction = -1))
    , "sf"
  )
})



testthat::test_that("Correct function output", {
  expect_true(
    rlang::is_list(
    splnr_climate_featureApproach(
      featuresDF = dat_species_bin,
      metricDF = metric_df, targetsDF = target, direction = 1))
    , "sf"
  )
})


testthat::test_that("Correct function output", {
  expect_true(
    rlang::is_list(
    splnr_climate_percentileApproach(
      featuresDF = dat_species_bin,
      metricDF = metric_df, targetsDF = target, direction = 1))
    , "sf"
  )
})

