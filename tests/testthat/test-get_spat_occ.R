test_that("get_spat_occ works", {
  library(terra)
  #Load Brazilian Flora data
  data("bf_data")
  #Example species
  spp <- c("Araucaria angustifolia", "Adesmia paranensis")
  #Get states, biomes and intersection states-biomes of species
  spp_spt <- get_spat_occ(data = bf_data, species = spp, state = TRUE,
                          biome = TRUE, intersection = TRUE, state_vect = NULL,
                          biome_vect = NULL, verbose = TRUE)

  expect_equal(class(spp_spt), "list")
  expect_equal(length(spp_spt), length(spp))
  expect_equal(unique(sapply(spp_spt, length)), 3)
  expect_equal(unique(sapply(spp_spt[[1]], class)), "SpatVector")
  expect_equal(unique(sapply(spp_spt[[2]], class)), "SpatVector")
  expect_equal(names(spp_spt), spp)


  #Get only states
  spp_states <- get_spat_occ(data = bf_data, species = spp, state = TRUE,
                          biome = FALSE, intersection = FALSE, state_vect = NULL,
                          biome_vect = NULL, verbose = TRUE)

  expect_equal(class(spp_states), "list")
  expect_equal(length(spp_states), length(spp))
  expect_equal(unique(sapply(spp_states, length)), 1)
  expect_equal(unique(sapply(spp_states[[1]], class)), "SpatVector")
  expect_equal(unique(sapply(spp_states[[2]], class)), "SpatVector")
  expect_equal(names(spp_states), spp)


  #Get only biomes
  spp_biomes <- get_spat_occ(data = bf_data, species = spp, state = FALSE,
                             biome = TRUE, intersection = FALSE, state_vect = NULL,
                             biome_vect = NULL, verbose = TRUE)

  expect_equal(class(spp_biomes), "list")
  expect_equal(length(spp_biomes), length(spp))
  expect_equal(unique(sapply(spp_biomes, length)), 1)
  expect_equal(unique(sapply(spp_biomes[[1]], class)), "SpatVector")
  expect_equal(unique(sapply(spp_biomes[[2]], class)), "SpatVector")
  expect_equal(names(spp_biomes), spp)

  ####Intersection does not work without states and biomes####
  expect_error(get_spat_occ(data = bf_data, species = spp, state = FALSE,
                            biome = FALSE, intersection = TRUE, state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))

  #It does not work if we don't set data and species
  expect_error(get_spat_occ(species = spp, state = FALSE,
                            biome = FALSE, intersection = TRUE, state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))
  expect_error(get_spat_occ(data = bf_data, state = FALSE,
                            biome = FALSE, intersection = TRUE, state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))
})


####Test errors to increase coverage####
test_that("get_spat_occdoes not work", {
  library(terra)
  #Load Brazilian Flora data
  data("bf_data")
  #Example species
  spp <- c("Araucaria angustifolia", "Adesmia paranensis")
  #data is not a dataframe
  expect_error(get_spat_occ(data = TRUE, species = spp, state = TRUE,
                          biome = TRUE, intersection = TRUE, state_vect = NULL,
                          biome_vect = NULL, verbose = TRUE))
  #species is not a vector
  expect_error(get_spat_occ(data = bf_data, species = TRUE, state = TRUE,
               biome = FALSE, intersection = FALSE, state_vect = NULL,
               biome_vect = NULL, verbose = TRUE))
  #Verbose, state, biome or intersection is not logical
  expect_error(get_spat_occ(data = bf_data, species = spp, state = TRUE,
                            biome = FALSE, intersection = FALSE, state_vect = NULL,
                            biome_vect = NULL, verbose = "TRUE"))
  expect_error(get_spat_occ(data = bf_data, species = spp, state = "SP",
                            biome = FALSE, intersection = FALSE, state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))
  expect_error(get_spat_occ(data = bf_data, species = spp, state = TRUE,
                            biome = "Amazon", intersection = FALSE, state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))
  expect_error(get_spat_occ(data = bf_data, species = spp, state = TRUE,
                            biome = FALSE, intersection = "All", state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))
  #state_vect and biome_vect are not spatvectors
  expect_error(get_spat_occ(data = bf_data, species = spp, state = TRUE,
                            biome = FALSE, intersection = FALSE,
                            state_vect = TRUE,
                            biome_vect = NULL, verbose = TRUE))
  expect_error(get_spat_occ(data = bf_data, species = spp, state = TRUE,
                            biome = FALSE, intersection = FALSE,
                            state_vect = NULL,
                            biome_vect = TRUE, verbose = TRUE))
  #Data without important columns
  d <- bf_data[,setdiff(colnames(bf_data), "species")]
  expect_error(get_spat_occ(data = d, species = spp, state = TRUE,
                            biome = FALSE, intersection = FALSE,
                            state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))
  #Intersect only works when states and biomes are TRUE
  expect_error(get_spat_occ(data = bf_data, species = spp, state = TRUE,
                            biome = FALSE, intersection = TRUE,
                            state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))
  #When there are species absent in data
  expect_error(get_spat_occ(data = bf_data,
                            species = c(spp, "Homo sapiens"),
                            state = TRUE,
                            biome = FALSE, intersection =  FALSE,
                            state_vect = NULL,
                            biome_vect = NULL, verbose = TRUE))
  #Lack info about biome
  expect_message(get_spat_occ(data = bf_data,
                              species = c(spp, "Conchocarpus cuneifolius"),
                              state = TRUE,
                              biome = TRUE, intersection = TRUE,
                              state_vect = NULL,
                              biome_vect = NULL, verbose = TRUE))
  #Lack info about state
  expect_message(get_spat_occ(data = bf_data,
                              species = c(spp, "Conchocarpus cuneifolius"),
                              state = TRUE,
                              biome = TRUE, intersection = TRUE,
                              state_vect = NULL,
                              biome_vect = NULL, verbose = TRUE))

  })



