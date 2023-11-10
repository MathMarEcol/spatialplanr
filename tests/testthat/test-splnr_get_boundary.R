
testthat::test_that("Correct function output", {
 expect_s3_class(
   splnr_get_boundary("North Atlantic Ocean", "Ocean"), "sf")
})
