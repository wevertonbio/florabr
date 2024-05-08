test_that("get_attributes works", {
  #Load Brazilian Flora data
  data("bf_data")

  #Group attributes
  Group <- get_attributes(data = bf_data, kingdom = "Plantae",
                          attribute = "Group")
  expect_equal(class(Group), "data.frame")
  expect_equal(ncol(Group), 2)
  expect_equal(colnames(Group), c("group", "grupo"))

  #SubGroup attributes
  SubGroup <- get_attributes(data = bf_data, kingdom = "Plantae",
                              attribute = "Subgroup")
  expect_equal(class(SubGroup), "data.frame")
  expect_equal(ncol(SubGroup), 2)
  expect_equal(colnames(SubGroup), c("subgroup", "subgrupo"))

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
  expect_equal(ncol(lifeForm), 2)
  expect_equal(colnames(lifeForm), c("lifeForm", "formaDeVida"))

  #habitat attributes
  habitat <- get_attributes(data = bf_data, kingdom = "Plantae",
                             attribute = "habitat")
  expect_equal(class(habitat), "data.frame")
  expect_equal(ncol(habitat), 2)
  expect_equal(colnames(habitat), c("habitat_en", "habitat_pt"))

  #vegetationType attributes
  vegetationType <- get_attributes(data = bf_data, kingdom = "Plantae",
                            attribute = "vegetation")
  expect_equal(class(vegetationType), "data.frame")
  expect_equal(ncol(vegetationType), 2)
  expect_equal(colnames(vegetationType), c("vegetation", "vegetacao"))

  #Origin attributes
  Origin <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "Origin")
  expect_equal(class(Origin), "data.frame")
  expect_equal(ncol(Origin), 2)
  expect_equal(colnames(Origin), c("origin", "origem"))

  #Endemism attributes
  Endemism <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "Endemism")
  expect_equal(class(Endemism), "data.frame")
  expect_equal(ncol(Endemism), 2)
  expect_equal(colnames(Endemism), c("endemism", "endemismo"))

  #Biome attributes
  Biome <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "Biome")
  expect_equal(class(Biome), "data.frame")
  expect_equal(ncol(Biome), 2)
  expect_equal(colnames(Biome), c("biome", "bioma"))

  #States attributes
  States <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "States")
  expect_equal(class(States), "data.frame")
  expect_equal(ncol(States), 2)
  expect_equal(colnames(States), c("sigla_acronym", "nome_name"))

  #taxonomicStatus attributes
  taxonomicStatus <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "taxonomicStatus")
  expect_equal(class(taxonomicStatus), "data.frame")
  expect_equal(ncol(taxonomicStatus), 2)
  expect_equal(colnames(taxonomicStatus),
               c("taxonomicStatus", "statusTaxonomico"))

  #vegetationType attributes
  nomenclaturalStatus <- get_attributes(data = bf_data, kingdom = "Plantae",
                                   attribute = "nomenclaturalStatus")
  expect_equal(class(nomenclaturalStatus), "data.frame")
  expect_equal(ncol(nomenclaturalStatus), 2)
  expect_equal(colnames(nomenclaturalStatus),
               c("nomenclaturalStatus", "statusNomenclatural"))

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
