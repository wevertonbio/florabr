test_that("Check_version works", {
  skip_on_cran() #Skip test on CRAN
  #####It works when we set a folder that already exists####
  my_dir <- file.path(file.path(tempdir(), "florabr"))
  dir.create(my_dir)
  #Download, merge and save LATEST data
  get_florabr(output_dir = my_dir, data_version = "latest", overwrite = TRUE,
              verbose = TRUE, solve_discrepancy = FALSE)
  expect_message(check_version(my_dir))

  #Now, Download, merge and save OLDER version in the same folder of latest
  get_florabr(output_dir = my_dir, data_version = "393.397",
              overwrite = TRUE,
              verbose = TRUE, solve_discrepancy = FALSE)
  expect_message(check_version(my_dir))

  #Now, Download, merge and save OLDER versions in another folder
  my_dir2 <- file.path(file.path(tempdir(), "florabr2"))
  dir.create(my_dir2)
  get_florabr(output_dir = my_dir2, data_version = "393.397",
              overwrite = TRUE,
              verbose = TRUE, solve_discrepancy = FALSE)
  get_florabr(output_dir = my_dir2, data_version = "393.398",
              overwrite = TRUE,
              verbose = TRUE, solve_discrepancy = FALSE)
  expect_message(check_version(my_dir2))


  #Random folder without any data
  random_dir <- file.path(tempdir(), "Random_folder")
  dir.create(random_dir)
  expect_message(check_version(random_dir))

  #It does not work if data_dir is missing or an object with invalid class
  expect_error(check_version())
  expect_error(check_version(data_dir = TRUE))

})
#unlink(my_dir, recursive = T, force = T)
#unlink(my_dir2, recursive = T, force = T)
#unlink(random_dir, recursive = T, force = T)
