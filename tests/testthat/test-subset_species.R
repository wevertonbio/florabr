test_that("subset_species works", {
  #Load Brazilian Flora data
  data("bf_data")
  #Species to extract from database
  spp <- c("Araucaria angustifolia", "Adesmia paranensis")
  #Not including subspecies or varieties
  spp_bf <- subset_species(data = bf_data, species = spp,
                           include_subspecies = FALSE,
                           include_variety = FALSE)
  expect_equal(class(spp_bf), "data.frame")
  expect_equal(nrow(spp_bf), length(spp))
  expect_equal(ncol(spp_bf), ncol(bf_data))

  #Including subspecies
  spp_bf <- subset_species(data = bf_data, species = spp,
                           include_subspecies = TRUE,
                           include_variety = FALSE)
  expect_equal(class(spp_bf), "data.frame")
  expect_equal(nrow(spp_bf), length(spp))
  expect_equal(ncol(spp_bf), ncol(bf_data))

  #Including variety
  spp_bf <- subset_species(data = bf_data, species = spp,
                           include_subspecies = FALSE,
                           include_variety = FALSE)
  expect_equal(class(spp_bf), "data.frame")
  expect_equal(nrow(spp_bf), length(spp))
  expect_equal(ncol(spp_bf), ncol(bf_data))

  #Including species and variery
  spp_bf <- subset_species(data = bf_data, species = spp,
                           include_subspecies = TRUE,
                           include_variety = TRUE)
  expect_equal(class(spp_bf), "data.frame")
  expect_equal(ncol(spp_bf), ncol(bf_data))

  ####It does not work when we don't specifie data and species
  expect_error(subset_species(species = spp,
                              include_subspecies = FALSE,
                              include_variety = FALSE))
  expect_error(subset_species(species = spp,
                              include_subspecies = FALSE,
                              include_variety = FALSE))

  #Return a message when there are species absent in database
  expect_warning(subset_species(data = bf_data,
                                species = "Homo sapiens",
                                include_subspecies = FALSE,
                                include_variety = FALSE))
})

####Test errors to increase coverage####
test_that("subset_specied does not work", {
  #Load Brazilian Flora data
  data("bf_data")
  #Species to extract from database
  spp <- c("Araucaria angustifolia", "Adesmia paranensis")
  #Data is not a dataframe
  expect_error(subset_species(data = TRUE, species = spp,
                           include_subspecies = FALSE,
                           include_variety = FALSE))
  #Species is not a character
  expect_error(subset_species(data = bf_data, species = TRUE,
                              include_subspecies = FALSE,
                              include_variety = FALSE))
  #Incluse subspecies or variety is not logical
  expect_error(subset_species(data = bf_data, species = spp,
                              include_subspecies = "FALSE",
                              include_variety = FALSE))
  expect_error(subset_species(data = bf_data, species = spp,
                              include_subspecies = FALSE,
                              include_variety = "FALSE"))
  #Invalid Kingdom
  expect_error(subset_species(data = bf_data, species = spp,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Animal"))
})

