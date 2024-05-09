test_that("select_species works", {
  #Load Brazilian Flora data
  data("bf_data")
  #Select endemic and native species of trees with disjunct occurrence in
  # Only in Atlantic Forest and Amazon
  am_af_only <- select_species(data = bf_data,
                               include_subspecies = FALSE,
                               include_variety = FALSE,
                               kingdom = "Plantae",
                               group = "All", subgroup = "All",
                               family = "All", genus = "All",
                               lifeForm = "Tree", filter_lifeForm = "only",
                               habitat = "All", filter_habitat = "in",
                               biome = c("Atlantic_Forest","Amazon"),
                               filter_biome = "only", #ONLY
                               state = "All", filter_state = "and",
                               vegetation = "All",
                               filter_vegetation = "in",
                               endemism = "Endemic", origin = "Native",
                               taxonomicStatus = "All",
                               nomenclaturalStatus = "All")
  expect_equal(class(am_af_only), "data.frame")
  expect_equal(ncol(am_af_only), ncol(bf_data))
  expect_equal(unique(am_af_only$endemism), "Endemic")
  expect_equal(unique(am_af_only$origin), "Native")
  expect_equal(unique(am_af_only$lifeForm), "Tree")
  expect_equal(unique(am_af_only$biome), "Amazon;Atlantic_Forest")

  #Now, in Atlantic Forest or Amazon (or both)
  am_af_in <- select_species(data = bf_data,
                             include_subspecies = FALSE,
                             include_variety = FALSE,
                             kingdom = "Plantae",
                             group = "All", subgroup = "All",
                             family = "All", genus = "All",
                             lifeForm = "Tree", filter_lifeForm = "only",
                             habitat = "All", filter_habitat = "in",
                             biome = c("Atlantic_Forest","Amazon"),
                             filter_biome = "in", #IN
                             state = "All", filter_state = "and",
                             vegetation = "All",
                             filter_vegetation = "in",
                             endemism = "Endemic", origin = "Native",
                             taxonomicStatus = "All",
                             nomenclaturalStatus = "All")
  expect_equal(class(am_af_in), "data.frame")
  expect_equal(ncol(am_af_in), ncol(bf_data))
  expect_equal(unique(am_af_in$endemism), "Endemic")
  expect_equal(unique(am_af_in$origin), "Native")
  expect_equal(unique(am_af_in$origin), "Native")
  expect_in("Amazon", unlist(strsplit(am_af_in$biome, ";")))
  expect_in("Atlantic_Forest", unlist(strsplit(am_af_in$biome, ";")))

  ####It does not work when we don't set data####
  expect_error(select_species(include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree", filter_lifeForm = "only",
                              habitat = "All", filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in", #IN
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))

  ####It does not work when we set an invalid kindom ####
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Animal", #Invalid kingdom
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree", filter_lifeForm = "only",
                              habitat = "All", filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in", #IN
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
})

####Test for more errors####
test_that("select_species does not work when", {
  #Load Brazilian Flora data
  data("bf_data")

  #Data is not a dataframe
  expect_error(select_species(data = TRUE, #ERROR
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree", filter_lifeForm = "only",
                              habitat = "All", filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in", #IN
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))

  #Include subspescies and variety is not logical
  expect_error(select_species(data = bf_data,
                              include_subspecies = 3, #ERROR
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree", filter_lifeForm = "only",
                              habitat = "All", filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in", #IN
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = TRUE,
                              include_variety = 3, #ERROR
                              kingdom = "Plantae",
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree", filter_lifeForm = "only",
                              habitat = "All", filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in", #IN
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))

  #Use invalid filter type
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "Tree", #ERROR
                              habitat = "All", filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in", #IN
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "All", #ERROR
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in", #IN
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "All", #ERROR
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All", subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "All", #ERROR
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  #Invalid attributes
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "Plants", #ERROR
                              subgroup = "All",
                              family = "All", genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All",
                              subgroup = "Any", #ERROR
                              family = "All", genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All",
                              subgroup = "All",
                              family = "Felidae", #ERROR
                              genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All",
                              subgroup = "All",
                              family = "All",
                              genus = "Felis", #ERROR
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Endemic", origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))

  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All",
                              subgroup = "All",
                              family = "All",
                              genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "Native", #ERROR
                              origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All",
                              subgroup = "All",
                              family = "All",
                              genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "All",
                              origin = "Endemic", #ERROR
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All",
                              subgroup = "All",
                              family = "All",
                              genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "All",
                              origin = "Native",
                              taxonomicStatus = "any", #ERROR
                              nomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              kingdom = "Plantae",
                              group = "All",
                              subgroup = "All",
                              family = "All",
                              genus = "All",
                              lifeForm = "Tree",
                              filter_lifeForm = "in",
                              habitat = "All",
                              filter_habitat = "in",
                              biome = c("Atlantic_Forest","Amazon"),
                              filter_biome = "in",
                              state = "All", filter_state = "and",
                              vegetation = "All",
                              filter_vegetation = "in",
                              endemism = "All",
                              origin = "Native",
                              taxonomicStatus = "All",
                              nomenclaturalStatus = "Any")) #ERROR


})

####Test using different filters####
test_that("select_species with differente filters", {
  #Load Brazilian Flora data
  data("bf_data")

  expect_no_error(select_species(data = bf_data, include_subspecies = TRUE))
  expect_no_error(select_species(data = bf_data, include_variety = TRUE))
  expect_no_error(select_species(data = bf_data, include_subspecies = TRUE,
                                 include_variety = TRUE))
  expect_no_error(select_species(data = bf_data, include_subspecies = TRUE,
                                 include_variety = TRUE))
  expect_no_error(select_species(data = bf_data,
                                 group = "Bryophytes", subgroup = "Mosses"))
  expect_no_error(select_species(data = bf_data,
                                 family = "Acanthaceae", lifeForm = "All"))
  expect_warning(select_species(data = bf_data,
                 family = "All", lifeForm = "Aquatic", taxonomicStatus = "All"))
  expect_no_error(select_species(data = bf_data,
                                 lifeForm = "Tree", filter_lifeForm = "in"))
  expect_no_error(select_species(data = bf_data,
                                 lifeForm = "Tree", filter_lifeForm = "not_in"))
  expect_no_error(select_species(data = bf_data,
                                 lifeForm = c("Tree", "Shrub"),
                                 filter_lifeForm = "and"))
  expect_no_error(select_species(data = bf_data,
                                 habitat = "Terrestrial",
                                 filter_habitat = "in"))
  expect_warning(expect_warning(select_species(data = bf_data,
                                habitat = "Tree",
                                filter_habitat = "in",
                                taxonomicStatus = "All")))
  expect_no_error(select_species(data = bf_data,
                                 habitat = "Terrestrial",
                                 filter_habitat = "only"))
  expect_no_error(select_species(data = bf_data,
                                 habitat = "Terrestrial",
                                 filter_habitat = "not_in"))
  expect_no_error(select_species(data = bf_data, biome = "All",
                                 habitat = c("Rupicolous", "Terrestrial"),
                                 filter_habitat = "and"))
  expect_warning(expect_warning(expect_warning(
    select_species(data = bf_data, biome = "Brazil"))))
  expect_no_error(select_species(data = bf_data, biome = "Amazon",
                                 filter_biome = "not_in"))
  expect_no_error(select_species(data = bf_data, biome = "Amazon",
                                 filter_biome = "only"))
  expect_no_error(select_species(data = bf_data, biome = c("Amazon", "Cerrado"),
                                 filter_biome = "and"))
  expect_warning(expect_warning(select_species(data = bf_data,
                                               taxonomicStatus = "All",
                                               state = "Rio")))
  expect_no_error(select_species(data = bf_data, state = "PR",
                                 filter_biome = "in"))
  expect_no_error(select_species(data = bf_data, state = "PR",
                                 filter_state = "only"))
  expect_no_error(select_species(data = bf_data, state = "PR",
                                 filter_state = "not_in"))
  expect_no_error(select_species(data = bf_data, state = c("PR", "SP"),
                                 filter_state = "and"))

  expect_warning(select_species(data = bf_data,
                                taxonomicStatus = "All",
                                vegetation = "Amazon"))
  expect_no_error(select_species(data = bf_data, vegetation = "Grassland",
                                 filter_vegetation =  "in"))
  expect_no_error(select_species(data = bf_data, vegetation = "Grassland",
                                 filter_vegetation =  "only"))
  expect_no_error(select_species(data = bf_data, vegetation = "Grassland",
                                 filter_vegetation =  "not_in"))
  expect_no_error(select_species(data = bf_data,
                                 vegetation = c("Grassland", "Cerrado"),
                                 filter_vegetation =  "and", endemism = "All",
                                 origin = "All"))
  expect_warning(expect_warning(select_species(data = bf_data,
                                 group = "Bryophytes", subgroup = "Mosses",
                                filter_biome = "in", habitat = "Corticolous",
                                taxonomicStatus = "All",
                                genus = "Acroporium",
                                endemism = "Endemic")))
  expect_warning(expect_warning(select_species(data = bf_data,
                                               group = "Bryophytes",
                                               subgroup = "Mosses",
                                               filter_biome = "in",
                                               habitat = "Corticolous",
                                               taxonomicStatus = "All",
                                               genus = "Acroporium",
                                               endemism = "All",
                                               origin = "Naturalized")))
  expect_warning(expect_warning(select_species(data = bf_data,
                                               group = "Bryophytes",
                                               subgroup = "Mosses",
                                               filter_biome = "in",
                                               habitat = "Corticolous",
                                               genus = "Acroporium",
                                               nomenclaturalStatus = "Rejected")))
  })
