
testthat::test_that("Correct function output", {
  testthat::expect_s3_class(
    splnr_get_MPAs(dat_PUs, "Australia"), "sf"
  )
})

