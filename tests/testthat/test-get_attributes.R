test_that("get_attributes works", {
  #Load Brazilian Flora data
  data("bf_data")

  #Group attributes
  Group <- get_attributes(data = bf_data, kingdom = "Plantae",
                          attribute = "Group")
  expect_equal(class(Group), "data.frame")


  #SubGroup attributes
  SubGroup <- get_attributes(data = bf_data, kingdom = "Plantae",
                              attribute = "Subgroup")
  expect_equal(class(SubGroup), "data.frame")

  #family attributes
  family <- get_attributes(data = bf_data, kingdom = "Plantae",
                             attribute = "family")
  expect_equal(class(family), "data.frame")
  expect_equal(ncol(family), 1)
  expect_equal(colnames(family), c("family"))

  #lifeForm attributes
  lifeForm <- get_attributes(data = bf_data, kingdom = "Plantae",
                           attribute = "lifeForm")
  expect_equal(class(lifeForm), "data.frame")

  #habitat attributes
  habitat <- get_attributes(data = bf_data, kingdom = "Plantae",
                             attribute = "habitat")
  expect_equal(class(habitat), "data.frame")

  #vegetationType attributes
  vegetationType <- get_attributes(data = bf_data, kingdom = "Plantae",
                            attribute = "vegetation")
  expect_equal(class(vegetationType), "data.frame")


  #Origin attributes
  Origin <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "Origin")
  expect_equal(class(Origin), "data.frame")

  #Endemism attributes
  Endemism <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "Endemism")
  expect_equal(class(Endemism), "data.frame")


  #Biome attributes
  Biome <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "Biome")
  expect_equal(class(Biome), "data.frame")


  #States attributes
  States <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "States")
  expect_equal(class(States), "data.frame")


  #taxonomicStatus attributes
  taxonomicStatus <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "taxonomicStatus")
  expect_equal(class(taxonomicStatus), "data.frame")


  #vegetationType attributes
  nomenclaturalStatus <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "nomenclaturalStatus")
  expect_equal(class(nomenclaturalStatus), "data.frame")


  ####It does not work with attribut not available####
  expect_error(get_attributes(data = bf_data, kingdom = "Plantae",
                 attribute = "size"))

  ####It does not work when we don't set data ####
  expect_error(get_attributes(kingdom = "Plantae",
                              attribute = "Biome"))

  ####It does not work when we don't set correct kingdom ####
  expect_error(get_attributes(data = bf_data, kingdom = "Animal",
                              attribute = "Biome"))
})

####Get more errors####
test_that("get_attributes works", {
  #Load Brazilian Flora data
  data("bf_data")
  expect_error(get_attributes(data = bf_data))
  expect_error(get_attributes(data = TRUE, attribute = "Biome"))
  expect_error(get_attributes(data = bf_data, attribute = TRUE))
  })
