test_that("Check_version warning that there isn't any version available", {
  #Random folder
  my_dir <- file.path(tempdir(), "Random_folder")
  dir.create(my_dir)
  expect_message(check_version(my_dir))
})
#unlink(my_dir, recursive = T, force = T)
