test_that("filter_florabr works", {
  #Load Brazilian Flora data
  data("bf_data")
  #Load occurrences of Myrcia hatschbachii
  data("occurrences")
  pts <- subset(occurrences, species == "Myrcia hatschbachii")
  #Filter with flag and clean with verbose
  fd <- filter_florabr(data = bf_data, occ = pts,
                       by_State = TRUE, buffer_State = 20,
                       by_Biome = TRUE, buffer_Biome = 20,
                       by_Endemism = TRUE, Buffer_Brazil = 20,
                       State_vect = NULL,
                       Biome_vect = NULL, BR_vect = NULL,
                       value = "flag&clean", keep_columns = TRUE,
                       verbose = TRUE)
  #Without verbose
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
                                 verbose = TRUE)
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
                                 verbose = TRUE)
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
                                 verbose = TRUE)
  expect_equal(class(fd_only_clean), "data.frame")
  expect_equal(ncol(fd_only_clean), 6)

  #Only clean and not biome
  expect_no_error(filter_florabr(data = bf_data, occ = pts,
                                  by_State = TRUE, buffer_State = 20,
                                  by_Biome = FALSE, buffer_Biome = 20,
                                  by_Endemism = TRUE, Buffer_Brazil = 20,
                                  State_vect = NULL,
                                  Biome_vect = NULL, BR_vect = NULL,
                                  value = "clean", keep_columns = TRUE,
                                  verbose = FALSE))

  #Only clean and not state
  expect_no_error(filter_florabr(data = bf_data, occ = pts,
                                 by_State = FALSE, buffer_State = 20,
                                 by_Biome = TRUE, buffer_Biome = 20,
                                 by_Endemism = TRUE, Buffer_Brazil = 20,
                                 State_vect = NULL,
                                 Biome_vect = NULL, BR_vect = NULL,
                                 value = "clean", keep_columns = TRUE,
                                 verbose = FALSE))
  #Only clean and not endemism and not keeping column
  expect_no_error(filter_florabr(data = bf_data, occ = pts,
                                 by_State = TRUE, buffer_State = 20,
                                 by_Biome = TRUE, buffer_Biome = 20,
                                 by_Endemism = FALSE, Buffer_Brazil = 20,
                                 State_vect = NULL,
                                 Biome_vect = NULL, BR_vect = NULL,
                                 value = "clean", keep_columns = FALSE,
                                 verbose = FALSE))


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

####Test errors####
test_that("filter_florabr does not work", {
  #Load Brazilian Flora data
  data("bf_data")
  #Load occurrences of Myrcia hatschbachii
  data("occurrences")
  pts <- subset(occurrences, species == "Myrcia hatschbachii")
  expect_error(filter_florabr(data = TRUE, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              State_vect = NULL,
                              Biome_vect = NULL, BR_vect = NULL,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = TRUE,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts, Long = T,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts, Lat = T,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = "TRUE", buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = "TRUE", buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = "TRUE", Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = "20",
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = "20",
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              State_vect = T,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              Biome_vect = T,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              BR_vect = T,
                              value = "flag&clean", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "clean&flag", keep_columns = TRUE,
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag", keep_columns = "TRUE",
                              verbose = FALSE))
  expect_error(filter_florabr(data = bf_data, occ = pts,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag", keep_columns = TRUE,
                              verbose = "FALSE"))
  bf_data2 <- bf_data[,setdiff("species", colnames(bf_data))]
  pts2 <- pts[,setdiff("species", colnames(pts))]
  expect_error(filter_florabr(data = bf_data, occ = pts2,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag", keep_columns = TRUE,
                              verbose = FALSE))
  pts3 <- pts
  pts3$species <- "Homo sapiens"
  expect_error(filter_florabr(data = bf_data, occ = pts3,
                              by_State = TRUE, buffer_State = 20,
                              by_Biome = TRUE, buffer_Biome = 20,
                              by_Endemism = TRUE, Buffer_Brazil = 20,
                              value = "flag", keep_columns = TRUE,
                              verbose = FALSE))
  })
