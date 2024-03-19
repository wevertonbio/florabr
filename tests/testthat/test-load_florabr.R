test_that("loading florabr works", {

  #####It works when we downloaded the data using get_florabr####
  my_dir <- file.path(file.path(tempdir(), "florabr"))
  dir.create(my_dir)
  #Download, merge and save data
  get_florabr(output_dir = my_dir, data_version = "latest", overwrite = TRUE,
              verbose = TRUE, solve_incongruences = FALSE)

  #Load data: short
  df <- load_florabr(data_dir = my_dir, data_version = "Latest_available",
                     type = "short")

  expect_equal(class(df), "data.frame")
  expect_equal(ncol(df), 19)

  #Load data: complete
  df_complete <- load_florabr(data_dir = my_dir,
                              data_version = "Latest_available",
                              type = "complete")

  expect_equal(class(df_complete), "data.frame")
  expect_equal(ncol(df_complete), 39)

  ####It does not work when we set a wrong version####
  expect_warning(expect_error(load_florabr(data_dir = my_dir, data_version = "1",
                              type = "complete")))

  ####It does not work when we set a wrong type####
  expect_error(load_florabr(data_dir = my_dir,
                            data_version = "Latest_available",
                            type = "anytype"))


  ####It does not work when we don't set the correct folder with the data downloaded####
  wrong_dir <- file.path(file.path(tempdir(), "wrong_dir"))
  dir.create(wrong_dir)
  expect_error(load_florabr(data_dir = wrong_dir,
                            data_version = "Latest_available",
                           type = "short"))
})
#unlink(my_dir, recursive = T, force = T)
#unlink(wrong_dir, recursive = T, force = T)
