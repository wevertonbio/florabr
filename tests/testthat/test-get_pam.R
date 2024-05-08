test_that("get_pam works", {
  require(terra)
  #Load Brazilian Flora data
  data("bf_data")
  #Select endemic and native species of trees with occurrence only in Amazon
  am_trees <- select_species(data = bf_data,
                             include_subspecies = FALSE,
                             include_variety = FALSE,
                             kingdom = "Plantae",
                             group = "All", subgroup = "All",
                             family = "All", genus = "All",
                             lifeForm = "Tree", filter_lifeForm = "only",
                             habitat = "All", filter_habitat = "in",
                             biome = "Amazon",
                             filter_biome = "only",
                             state = "All", filter_state = "and",
                             vegetation = "All",
                             filter_vegetation = "in",
                             endemism = "Endemic", origin = "Native",
                             taxonomicStatus = "Accepted",
                             nomenclaturalStatus = "All")
  #Get presence-absence matrix
  pam_am <- get_pam(data = am_trees, by_biome = TRUE, by_state = TRUE,
                    by_vegetation = FALSE, remove_empty_sites = TRUE,
                    return_richness_summary = TRUE,
                    return_spatial_richness = TRUE,
                    return_plot = TRUE)
  expect_equal(class(pam_am), "list")
  expect_equal(length(pam_am), 3)
  expect_equal(class(pam_am[[1]]), "data.frame")
  expect_equal(ncol(pam_am[[1]]), nrow(am_trees) + 2)
  expect_equal(class(pam_am[[2]]), "data.frame")
  expect_equal(class(pam_am[[3]]), "SpatVector", ignore_attr = TRUE)


  #Get presence-absence matrix, without return richness summary
  pam_am_not_richness <- get_pam(data = am_trees, by_biome = TRUE, by_state = TRUE,
                    by_vegetation = FALSE, remove_empty_sites = TRUE,
                    return_richness_summary = FALSE,
                    return_spatial_richness = TRUE,
                    return_plot = TRUE)
  expect_equal(class(pam_am_not_richness), "list")
  expect_equal(length(pam_am_not_richness), 2)
  expect_equal(class(pam_am_not_richness[[1]]), "data.frame")
  expect_equal(ncol(pam_am_not_richness[[1]]), nrow(am_trees) + 2)
  expect_equal(class(pam_am_not_richness[[2]]), "SpatVector", ignore_attr = TRUE)

  #Get presence-absence matrix, without return spatial_richness
  pam_am_not_spatial <- get_pam(data = am_trees, by_biome = TRUE, by_state = TRUE,
                                 by_vegetation = FALSE, remove_empty_sites = TRUE,
                                 return_richness_summary = TRUE,
                                 return_spatial_richness = FALSE,
                                 return_plot = TRUE)
  expect_equal(class(pam_am_not_spatial), "list")
  expect_equal(length(pam_am_not_spatial), 2)
  expect_equal(class(pam_am_not_spatial[[1]]), "data.frame")
  expect_equal(ncol(pam_am_not_spatial[[1]]), nrow(am_trees) + 2)
  expect_equal(class(pam_am_not_spatial[[2]]), "data.frame")

  #Get presence-absence matrix, only PAM
  pam_am_only_pam <- get_pam(data = am_trees, by_biome = TRUE, by_state = TRUE,
                                by_vegetation = FALSE, remove_empty_sites = TRUE,
                                return_richness_summary = FALSE,
                                return_spatial_richness = FALSE,
                                return_plot = TRUE)
  expect_equal(class(pam_am_only_pam), "data.frame")
  expect_equal(ncol(pam_am_only_pam), nrow(am_trees) + 2)

  #Get presence-absence matrix, only PAM, using vegetation type
  expect_no_error(get_pam(data = am_trees, by_biome = TRUE, by_state = TRUE,
                             by_vegetation = TRUE, remove_empty_sites = TRUE,
                             return_richness_summary = FALSE,
                             return_spatial_richness = FALSE,
                             return_plot = TRUE))

  #Get presence-absence matrix, only PAM, using vegetation type, but not state
  expect_no_error(get_pam(data = am_trees, by_biome = TRUE, by_state = FALSE,
                          by_vegetation = TRUE, remove_empty_sites = TRUE,
                          return_richness_summary = FALSE,
                          return_spatial_richness = FALSE,
                          return_plot = TRUE))
  #Get presence-absence matrix, only PAM, using vegetation type, but not biome
  expect_no_error(get_pam(data = am_trees, by_biome = FALSE, by_state = TRUE,
                          by_vegetation = TRUE, remove_empty_sites = TRUE,
                          return_richness_summary = FALSE,
                          return_spatial_richness = FALSE,
                          return_plot = TRUE))


  ####It does not work when we don't set data####
  expect_error(get_pam(by_biome = TRUE, by_state = TRUE,
                       by_vegetation = FALSE, remove_empty_sites = TRUE,
                       return_richness_summary = FALSE,
                       return_spatial_richness = FALSE,
                       return_plot = TRUE))

  ####It does not work when we biome, state and Vegetatio are set to FALSE####
  expect_error(get_pam(data = am_trees, by_biome = FALSE, by_state = FALSE,
                       by_vegetation = FALSE, remove_empty_sites = TRUE,
                       return_richness_summary = FALSE,
                       return_spatial_richness = FALSE,
                       return_plot = TRUE))
})

####Test errors####
test_that("get_pam does not work", {
  require(terra)
  #Load Brazilian Flora data
  data("bf_data")
  #Select endemic and native species of trees with occurrence only in Amazon
  am_trees <- select_species(data = bf_data,
                             include_subspecies = FALSE,
                             include_variety = FALSE,
                             kingdom = "Plantae",
                             group = "All", subgroup = "All",
                             family = "All", genus = "All",
                             lifeForm = "Tree", filter_lifeForm = "only",
                             habitat = "All", filter_habitat = "in",
                             biome = "Amazon",
                             filter_biome = "only",
                             state = "All", filter_state = "and",
                             vegetation = "All",
                             filter_vegetation = "in",
                             endemism = "Endemic", origin = "Native",
                             taxonomicStatus = "Accepted",
                             nomenclaturalStatus = "All")
  expect_error(get_pam(data = TRUE))
  expect_error(get_pam(data = bf_data, by_biome = "TRUE"))
  expect_error(get_pam(data = bf_data, by_state = "TRUE"))
  expect_error(get_pam(data = bf_data, by_vegetation = "TRUE"))
  expect_error(get_pam(data = bf_data, remove_empty_sites = "TRUE"))
  expect_error(get_pam(data = bf_data, return_richness_summary = "TRUE"))
  expect_error(get_pam(data = bf_data, return_spatial_richness = "TRUE"))
  bf_data2 <- bf_data[, setdiff(colnames(bf_data), "species")]
  expect_error(get_pam(data = bf_data2))
  expect_error(get_pam(data = bf_data, by_biome = FALSE, by_state = FALSE,
                       by_vegetation = TRUE,
                       return_spatial_richness = TRUE))
})
