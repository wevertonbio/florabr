test_that("check names works", {
  #Load data
  data("bf_data", package = "florabr")
  #Test with two species
  spp <- c("Butia cattarinensis", "Araucaria angustifolia")
  df <- check_names(data = bf_data, species = spp)
  expect_equal(class(df), "data.frame")
  expect_equal(nrow(df), length(spp))
  expect_equal(ncol(df), 8)

  ####It does not work when we don't set data or species####
  expect_error(check_names(species = spp))
  expect_error(check_names(data = bf_data))

  ##More errors####
  expect_error(check_names(data = TRUE, species = spp))
  expect_error(check_names(data = bf_data, species = TRUE))
  expect_error(check_names(data = bf_data, species = spp,
                           max_distance = TRUE))
  expect_error(check_names(data = bf_data, species = spp,
                           max_distance = 100))
  expect_error(check_names(data = bf_data, species = spp,
                           Kingdom = "Animal"))
})
