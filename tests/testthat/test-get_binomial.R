test_that("get_binomial works", {
  #Create vector of species
  spp <- c("Araucaria angustifolia (Bertol.) Kuntze",
           "Butia catarinensis Noblick & Lorenzi",
           "Adesmia paranensis Burkart")
  #Get binomial
  spp_new <- get_binomial(species_names = spp)
  expect_equal(length(spp_new), length(spp))
  expect_equal(length(unlist(strsplit(spp_new, " "))), length(spp)*2)

  #Error
  expect_error(get_binomial(species_names = T))

})
