test_that("subset_species works", {
  #Load Brazilian Flora data
  data("bf_data")
  #Species to extract from database
  spp <- c("Araucaria angustifolia", "Adesmia paranensis")
  spp_bf <- subset_species(data = bf_data, species = spp,
                           include_subspecies = FALSE,
                           include_variety = FALSE)
  expect_equal(class(spp_bf), "data.frame")
  expect_equal(nrow(spp_bf), length(spp))
  expect_equal(ncol(spp_bf), ncol(bf_data))

  ####It does not work when we don't specifie data and species
  expect_error(subset_species(species = spp,
                              include_subspecies = FALSE,
                              include_variety = FALSE))
  expect_error(subset_species(species = spp,
                              include_subspecies = FALSE,
                              include_variety = FALSE))
})
