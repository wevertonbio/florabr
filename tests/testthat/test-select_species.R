test_that("select_species works", {
  #Load Brazilian Flora data
  data("bf_data")
  #Select endemic and native species of trees with disjunct occurrence in
  # Only in Atlantic Forest and Amazon
  am_af_only <- select_species(data = bf_data,
                               include_subspecies = FALSE,
                               include_variety = FALSE,
                               Kingdom = "Plantae",
                               Group = "All", Subgroup = "All",
                               Family = "All", Genus = "All",
                               LifeForm = "Tree", filter_LifeForm = "only",
                               Habitat = "All", filter_Habitat = "in",
                               Biome = c("Atlantic_Forest","Amazon"),
                               filter_Biome = "only", #ONLY
                               State = "All", filter_State = "and",
                               VegetationType = "All",
                               filter_Vegetation = "in",
                               Endemism = "Endemic", Origin = "Native",
                               TaxonomicStatus = "All",
                               NomenclaturalStatus = "All")
  expect_equal(class(am_af_only), "data.frame")
  expect_equal(ncol(am_af_only), ncol(bf_data))
  expect_equal(unique(am_af_only$Endemism), "Endemic")
  expect_equal(unique(am_af_only$Origin), "Native")
  expect_equal(unique(am_af_only$lifeForm), "Tree")
  expect_equal(unique(am_af_only$Biome), "Amazon;Atlantic_Forest")

  #Now, in Atlantic Forest or Amazon (or both)
  am_af_in <- select_species(data = bf_data,
                             include_subspecies = FALSE,
                             include_variety = FALSE,
                             Kingdom = "Plantae",
                             Group = "All", Subgroup = "All",
                             Family = "All", Genus = "All",
                             LifeForm = "Tree", filter_LifeForm = "only",
                             Habitat = "All", filter_Habitat = "in",
                             Biome = c("Atlantic_Forest","Amazon"),
                             filter_Biome = "in", #IN
                             State = "All", filter_State = "and",
                             VegetationType = "All",
                             filter_Vegetation = "in",
                             Endemism = "Endemic", Origin = "Native",
                             TaxonomicStatus = "All",
                             NomenclaturalStatus = "All")
  expect_equal(class(am_af_in), "data.frame")
  expect_equal(ncol(am_af_in), ncol(bf_data))
  expect_equal(unique(am_af_in$Endemism), "Endemic")
  expect_equal(unique(am_af_in$Origin), "Native")
  expect_equal(unique(am_af_in$Origin), "Native")
  expect_in("Amazon", unlist(strsplit(am_af_in$Biome, ";")))
  expect_in("Atlantic_Forest", unlist(strsplit(am_af_in$Biome, ";")))

  ####It does not work when we don't set data####
  expect_error(select_species(include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree", filter_LifeForm = "only",
                              Habitat = "All", filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in", #IN
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))

  ####It does not work when we set an invalid kindom ####
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Animal", #Invalid kingdom
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree", filter_LifeForm = "only",
                              Habitat = "All", filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in", #IN
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
})

####Test for more errors####
test_that("select_species does not work when", {
  #Load Brazilian Flora data
  data("bf_data")

  #Data is not a dataframe
  expect_error(select_species(data = TRUE, #ERROR
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree", filter_LifeForm = "only",
                              Habitat = "All", filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in", #IN
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))

  #Include subspescies and variety is not logical
  expect_error(select_species(data = bf_data,
                              include_subspecies = 3, #ERROR
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree", filter_LifeForm = "only",
                              Habitat = "All", filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in", #IN
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = TRUE,
                              include_variety = 3, #ERROR
                              Kingdom = "Plantae",
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree", filter_LifeForm = "only",
                              Habitat = "All", filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in", #IN
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))

  #Use invalid filter type
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "Tree", #ERROR
                              Habitat = "All", filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in", #IN
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "All", #ERROR
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in", #IN
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "All", #ERROR
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All", Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "All", #ERROR
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  #Invalid attributes
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "Plants", #ERROR
                              Subgroup = "All",
                              Family = "All", Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All",
                              Subgroup = "Any", #ERROR
                              Family = "All", Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All",
                              Subgroup = "All",
                              Family = "Felidae", #ERROR
                              Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All",
                              Subgroup = "All",
                              Family = "All",
                              Genus = "Felis", #ERROR
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Endemic", Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))

  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All",
                              Subgroup = "All",
                              Family = "All",
                              Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "Native", #ERROR
                              Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All",
                              Subgroup = "All",
                              Family = "All",
                              Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "All",
                              Origin = "Endemic", #ERROR
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All",
                              Subgroup = "All",
                              Family = "All",
                              Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "All",
                              Origin = "Native",
                              TaxonomicStatus = "any", #ERROR
                              NomenclaturalStatus = "All"))
  expect_error(select_species(data = bf_data,
                              include_subspecies = FALSE,
                              include_variety = FALSE,
                              Kingdom = "Plantae",
                              Group = "All",
                              Subgroup = "All",
                              Family = "All",
                              Genus = "All",
                              LifeForm = "Tree",
                              filter_LifeForm = "in",
                              Habitat = "All",
                              filter_Habitat = "in",
                              Biome = c("Atlantic_Forest","Amazon"),
                              filter_Biome = "in",
                              State = "All", filter_State = "and",
                              VegetationType = "All",
                              filter_Vegetation = "in",
                              Endemism = "All",
                              Origin = "Native",
                              TaxonomicStatus = "All",
                              NomenclaturalStatus = "Any")) #ERROR


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
                                 Group = "Bryophytes", Subgroup = "Mosses"))
  expect_no_error(select_species(data = bf_data,
                                 Family = "Acanthaceae", LifeForm = "All"))
  expect_warning(select_species(data = bf_data,
                 Family = "All", LifeForm = "Aquatic", TaxonomicStatus = "All"))
  expect_no_error(select_species(data = bf_data,
                                 LifeForm = "Tree", filter_LifeForm = "in"))
  expect_no_error(select_species(data = bf_data,
                                 LifeForm = "Tree", filter_LifeForm = "not_in"))
  expect_no_error(select_species(data = bf_data,
                                 LifeForm = c("Tree", "Shrub"),
                                 filter_LifeForm = "and"))
  expect_no_error(select_species(data = bf_data,
                                 Habitat = "Terrestrial",
                                 filter_Habitat = "in"))
  expect_warning(expect_warning(select_species(data = bf_data,
                                Habitat = "Tree",
                                filter_Habitat = "in",
                                TaxonomicStatus = "All")))
  expect_no_error(select_species(data = bf_data,
                                 Habitat = "Terrestrial",
                                 filter_Habitat = "only"))
  expect_no_error(select_species(data = bf_data,
                                 Habitat = "Terrestrial",
                                 filter_Habitat = "not_in"))
  expect_no_error(select_species(data = bf_data, Biome = "All",
                                 Habitat = c("Rupicolous", "Terrestrial"),
                                 filter_Habitat = "and"))
  expect_warning(select_species(data = bf_data, Biome = "Brazil"))
  expect_no_error(select_species(data = bf_data, Biome = "Amazon",
                                 filter_Biome = "not_in"))
  expect_no_error(select_species(data = bf_data, Biome = "Amazon",
                                 filter_Biome = "only"))
  expect_no_error(select_species(data = bf_data, Biome = c("Amazon", "Cerrado"),
                                 filter_Biome = "and"))
  expect_warning(expect_warning(select_species(data = bf_data,
                                               TaxonomicStatus = "All",
                                               State = "Rio")))
  expect_no_error(select_species(data = bf_data, State = "PR",
                                 filter_Biome = "in"))
  expect_no_error(select_species(data = bf_data, State = "PR",
                                 filter_State = "only"))
  expect_no_error(select_species(data = bf_data, State = "PR",
                                 filter_State = "not_in"))
  expect_no_error(select_species(data = bf_data, State = c("PR", "SP"),
                                 filter_State = "and"))

  expect_warning(select_species(data = bf_data,
                                TaxonomicStatus = "All",
                                VegetationType = "Amazon"))
  expect_no_error(select_species(data = bf_data, VegetationType = "Grassland",
                                 filter_Vegetation =  "in"))
  expect_no_error(select_species(data = bf_data, VegetationType = "Grassland",
                                 filter_Vegetation =  "only"))
  expect_no_error(select_species(data = bf_data, VegetationType = "Grassland",
                                 filter_Vegetation =  "not_in"))
  expect_no_error(select_species(data = bf_data,
                                 VegetationType = c("Grassland", "Cerrado"),
                                 filter_Vegetation =  "and", Endemism = "All",
                                 Origin = "All"))
  expect_warning(expect_warning(select_species(data = bf_data,
                                 Group = "Bryophytes", Subgroup = "Mosses",
                                filter_Biome = "in", Habitat = "Corticolous",
                                TaxonomicStatus = "All",
                                Genus = "Acroporium",
                                Endemism = "Endemic")))
  expect_warning(expect_warning(select_species(data = bf_data,
                                               Group = "Bryophytes",
                                               Subgroup = "Mosses",
                                               filter_Biome = "in",
                                               Habitat = "Corticolous",
                                               TaxonomicStatus = "All",
                                               Genus = "Acroporium",
                                               Endemism = "All",
                                               Origin = "Naturalized")))
  expect_warning(expect_warning(select_species(data = bf_data,
                                               Group = "Bryophytes",
                                               Subgroup = "Mosses",
                                               filter_Biome = "in",
                                               Habitat = "Corticolous",
                                               Genus = "Acroporium",
                                               NomenclaturalStatus = "Rejected")))
  })
