test_that("loading florabr works", {
  skip_on_cran() #Skip test on CRAN
  #####It works when we downloaded the data using get_florabr####
  my_dir <- file.path(file.path(tempdir(), "florabr"))
  dir.create(my_dir, showWarnings = FALSE)
  #Download, merge and save data
  get_florabr(output_dir = my_dir, data_version = "latest", overwrite = TRUE,
              verbose = TRUE, solve_discrepancy = FALSE)

  #Load data: short
  df <- load_florabr(data_dir = my_dir, data_version = "Latest_available",
                     type = "short")

  expect_equal(class(df), "data.frame")

  #Load data: complete
  df_complete <- load_florabr(data_dir = my_dir,
                              data_version = "Latest_available",
                              type = "complete")

  expect_equal(class(df_complete), "data.frame")

  ####It does not work when we set a wrong version####
  expect_warning(expect_error(load_florabr(data_dir = my_dir, data_version = "1",
                              type = "complete")))

  ####It does not work when we set a wrong type####
  expect_error(load_florabr(data_dir = my_dir,
                            data_version = "Latest_available",
                            type = "anytype"))


  ####It does not work when we don't set the correct folder with the data downloaded####
  wrong_dir <- file.path(file.path(tempdir(), "wrong_dir"))
  dir.create(wrong_dir, showWarnings = FALSE)
  expect_error(load_florabr(data_dir = wrong_dir,
                            data_version = "Latest_available",
                           type = "short"))

})
#unlink(my_dir, recursive = T, force = T)
#unlink(wrong_dir, recursive = T, force = T)

####Others errors####
test_that("loading florabr does not work", {
  my_dir<- file.path(file.path(tempdir(), "wrong_dir"))
  dir.create(my_dir, showWarnings = FALSE)
  expect_error(load_florabr(data_version = "Latest_available",
                            type = "short"))
  expect_error(load_florabr(data_dir = TRUE,
                            data_version = "Latest_available",
                            type = "short"))
  expect_error(load_florabr(data_dir = my_dir,
                            data_version = TRUE,
                            type = "short"))

})
