test_that("get_spat_occ works", {
  library(terra)
  #Load Brazilian Flora data
  data("bf_data")
  #Example species
  spp <- c("Araucaria angustifolia", "Adesmia paranensis")
  #Get states, biomes and intersection states-biomes of species
  spp_spt <- get_spat_occ(data = bf_data, species = spp, State = TRUE,
                          Biome = TRUE, intersection = TRUE, State_vect = NULL,
                          Biome_vect = NULL, verbose = TRUE)

  expect_equal(class(spp_spt), "list")
  expect_equal(length(spp_spt), length(spp))
  expect_equal(unique(sapply(spp_spt, length)), 3)
  expect_equal(unique(sapply(spp_spt[[1]], class)), "SpatVector")
  expect_equal(unique(sapply(spp_spt[[2]], class)), "SpatVector")
  expect_equal(names(spp_spt), spp)


  #Get only states
  spp_states <- get_spat_occ(data = bf_data, species = spp, State = TRUE,
                          Biome = FALSE, intersection = FALSE, State_vect = NULL,
                          Biome_vect = NULL, verbose = TRUE)

  expect_equal(class(spp_states), "list")
  expect_equal(length(spp_states), length(spp))
  expect_equal(unique(sapply(spp_states, length)), 1)
  expect_equal(unique(sapply(spp_states[[1]], class)), "SpatVector")
  expect_equal(unique(sapply(spp_states[[2]], class)), "SpatVector")
  expect_equal(names(spp_states), spp)


  #Get only biomes
  spp_biomes <- get_spat_occ(data = bf_data, species = spp, State = FALSE,
                             Biome = TRUE, intersection = FALSE, State_vect = NULL,
                             Biome_vect = NULL, verbose = TRUE)

  expect_equal(class(spp_biomes), "list")
  expect_equal(length(spp_biomes), length(spp))
  expect_equal(unique(sapply(spp_biomes, length)), 1)
  expect_equal(unique(sapply(spp_biomes[[1]], class)), "SpatVector")
  expect_equal(unique(sapply(spp_biomes[[2]], class)), "SpatVector")
  expect_equal(names(spp_biomes), spp)

  ####Intersection does not work without States and Biomes####
  expect_error(get_spat_occ(data = bf_data, species = spp, State = FALSE,
                            Biome = FALSE, intersection = TRUE, State_vect = NULL,
                            Biome_vect = NULL, verbose = TRUE))

  #It does not work if we don't set data and species
  expect_error(get_spat_occ(species = spp, State = FALSE,
                            Biome = FALSE, intersection = TRUE, State_vect = NULL,
                            Biome_vect = NULL, verbose = TRUE))
  expect_error(get_spat_occ(data = bf_data, State = FALSE,
                            Biome = FALSE, intersection = TRUE, State_vect = NULL,
                            Biome_vect = NULL, verbose = TRUE))
})
