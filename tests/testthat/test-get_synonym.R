test_that("get_synonym works", {
  data("bf_data") #Load Brazilian Flora data
  #Species to extract synonyms
  spp <- c("Araucaria angustifolia", "Adesmia paranensis")
  spp_synonyms <- get_synonym(data = bf_data, species = spp)
  expect_equal(class(spp_synonyms), "data.frame")
  expect_equal(ncol(spp_synonyms), 4)

  #Return a message when there are species absent in database
  spp2 <- c("Araucaria angustifolia", "Adesmia paranensis", "Homo sapiens")
  expect_warning(get_synonym(data = bf_data,
                             species = spp2))
  expect_warning(get_synonym(data = bf_data,
                             species = "Homo sapiens"))
  })

test_that("get_synonym does not works", {
  data("bf_data") #Load Brazilian Flora data
  #Species to extract synonyms
  spp <- c("Araucaria angustifolia", "Adesmia paranensis")
  expect_error(get_synonym(species = spp))
  expect_error(get_synonym(data = bf_data))
  expect_error(get_synonym(data = bf_data, species = TRUE))
  expect_error(get_synonym(data = TRUE, species = spp))
})
