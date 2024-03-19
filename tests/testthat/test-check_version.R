test_that("Check_version warning that there isn't any version available", {
  #Random folder
  my_dir <- file.path(tempdir(), "Random_folder")
  dir.create(my_dir)
  expect_message(check_version(my_dir))

  #It does not work if data_dir is missing or an object with invalid class
  expect_error(check_version())
  expect_error(check_version(data_dir = TRUE))

})
#unlink(my_dir, recursive = T, force = T)
