
testthat::test_that("Correct function output", {
  expect_s3_class(
    suppressWarnings({splnr_get_MPAs(dat_PUs, "Australia")}), "sf"
  )
})

