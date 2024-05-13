test_that("get_attributes works", {
  #Load Brazilian Flora data
  data("bf_data")

  #Group attributes
  Group <- get_attributes(data = bf_data,
                          attribute = "Group")
  expect_equal(class(Group), "list")


  #SubGroup attributes
  SubGroup <- get_attributes(data = bf_data,
                              attribute = "Subgroup")
  expect_equal(class(SubGroup), "list")

  #family attributes
  family <- get_attributes(data = bf_data,
                             attribute = "family")
  expect_equal(class(family), "list")
  expect_equal(ncol(family[[1]]), 1)
  expect_equal(colnames(family[[1]]), c("family"))

  #lifeForm attributes
  lifeForm <- get_attributes(data = bf_data,
                           attribute = "lifeForm")
  expect_equal(class(lifeForm), "list")

  #habitat attributes
  habitat <- get_attributes(data = bf_data,
                             attribute = "habitat")
  expect_equal(class(habitat), "list")

  #vegetationType attributes
  vegetationType <- get_attributes(data = bf_data,
                            attribute = "vegetation")
  expect_equal(class(vegetationType), "list")


  #Origin attributes
  Origin <- get_attributes(data = bf_data,
                                   attribute = "Origin")
  expect_equal(class(Origin), "list")

  #Endemism attributes
  Endemism <- get_attributes(data = bf_data,
                                   attribute = "Endemism")
  expect_equal(class(Endemism), "list")


  #Biome attributes
  Biome <- get_attributes(data = bf_data,
                                   attribute = "Biome")
  expect_equal(class(Biome), "list")


  #States attributes
  States <- get_attributes(data = bf_data,
                                   attribute = "States")
  expect_equal(class(States), "list")
  expect_equal(ncol(States[[1]]), 2)


  #taxonomicStatus attributes
  taxonomicStatus <- get_attributes(data = bf_data,
                                   attribute = "taxonomicStatus")
  expect_equal(class(taxonomicStatus), "list")


  #vegetationType attributes
  nomenclaturalStatus <- get_attributes(data = bf_data,
                                   attribute = "nomenclaturalStatus")
  expect_equal(class(nomenclaturalStatus), "list")


  ####It does not work with attribut not available####
  expect_error(get_attributes(data = bf_data,
                 attribute = "size"))

  ####It does not work when we don't set data ####
  expect_error(get_attributes(
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
