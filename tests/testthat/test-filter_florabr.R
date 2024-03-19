test_that("filter_florabr works", {
  #Load Brazilian Flora data
  data("bf_data")
  #Load occurrences of Myrcia hatschbachii
  data("occurrences")
  pts <- subset(occurrences, species == "Myrcia hatschbachii")
  #Filter with flag and clean
  fd <- filter_florabr(data = bf_data, occ = pts,
                       by_State = TRUE, buffer_State = 20,
                       by_Biome = TRUE, buffer_Biome = 20,
                       by_Endemism = TRUE, Buffer_Brazil = 20,
                       State_vect = NULL,
                       Biome_vect = NULL, BR_vect = NULL,
                       value = "flag&clean", keep_columns = TRUE,
                       verbose = FALSE)
  expect_equal(class(fd), "list")
  expect_equal(class(fd[[1]]), "data.frame")
  expect_equal(ncol(fd[[1]]), 10)
  expect_equal(class(fd[[2]]), "data.frame")
  expect_equal(ncol(fd[[2]]), 6)

  #Flag and clean
  fd_only_flag <- filter_florabr(data = bf_data, occ = pts,
                                 by_State = TRUE, buffer_State = 20,
                                 by_Biome = TRUE, buffer_Biome = 20,
                                 by_Endemism = TRUE, Buffer_Brazil = 20,
                                 State_vect = NULL,
                                 Biome_vect = NULL, BR_vect = NULL,
                                 value = "flag", keep_columns = TRUE,
                                 verbose = FALSE)
  expect_equal(class(fd_only_flag), "data.frame")
  expect_equal(ncol(fd_only_flag), 10)

  #Only flag
  fd_only_flag <- filter_florabr(data = bf_data, occ = pts,
                                 by_State = TRUE, buffer_State = 20,
                                 by_Biome = TRUE, buffer_Biome = 20,
                                 by_Endemism = TRUE, Buffer_Brazil = 20,
                                 State_vect = NULL,
                                 Biome_vect = NULL, BR_vect = NULL,
                                 value = "flag", keep_columns = TRUE,
                                 verbose = FALSE)
  expect_equal(class(fd_only_flag), "data.frame")
  expect_equal(ncol(fd_only_flag), 10)

  #Only clean
  fd_only_clean <- filter_florabr(data = bf_data, occ = pts,
                                 by_State = TRUE, buffer_State = 20,
                                 by_Biome = TRUE, buffer_Biome = 20,
                                 by_Endemism = TRUE, Buffer_Brazil = 20,
                                 State_vect = NULL,
                                 Biome_vect = NULL, BR_vect = NULL,
                                 value = "clean", keep_columns = TRUE,
                                 verbose = FALSE)
  expect_equal(class(fd_only_clean), "data.frame")
  expect_equal(ncol(fd_only_clean), 6)

  ####It does not work without set data or occ
  expect_error(filter_florabr(occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              State_vect = NULL,
                              Biome_vect = NULL, BR_vect = NULL,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              State_vect = NULL,
                              Biome_vect = NULL, BR_vect = NULL,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))

  ####It does not work when setting wrong value
  expect_error(filter_florabr(data = bf_data,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              State_vect = NULL,
                              Biome_vect = NULL, BR_vect = NULL,
                              value = "any", keep_columns = TRUE,
                              verbose = FALSE))
})
