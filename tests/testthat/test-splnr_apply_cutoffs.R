
testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5), "sf"
  )
})
