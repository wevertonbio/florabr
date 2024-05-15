test_that("get florabr works", {
  skip_on_cran() #Skip test on CRAN
  #####It works when we set a folder that already exists####
  my_dir <- file.path(file.path(tempdir(), "florabr"))
  dir.create(my_dir, showWarnings = FALSE)
  #Download, merge and save data
  get_florabr(output_dir = my_dir, data_version = "latest", overwrite = TRUE,
              verbose = TRUE, solve_discrepancy = FALSE)
  #Get files downloaded
  d <- list.files(my_dir, recursive = TRUE, full.names = FALSE)
  d <- gsub(".*/","",d)
  expect_in("CompleteBrazilianFlora.rds", d)

  ####It does not work when we set a wrong data_version####
  expect_error(get_florabr(output_dir = my_dir, data_version = "any", overwrite = TRUE,
              verbose = TRUE))

  ####It does not work when we set a folder that does not exist####
  no_dir <- file.path(tempdir(), "This folder does not exist")

  expect_error(get_florabr(output_dir = no_dir,
              data_version = "latest", overwrite = TRUE,
              verbose = TRUE))

  ####Get others errors####
  expect_error(get_florabr(data_version = "latest", overwrite = TRUE,
                           verbose = TRUE))
  expect_error(get_florabr(output_dir = TRUE, data_version = "latest",
                           overwrite = TRUE, verbose = TRUE))
  expect_error(get_florabr(output_dir = my_dir, data_version = TRUE,
                           overwrite = TRUE, verbose = TRUE))
  expect_error(get_florabr(output_dir = my_dir, data_version = "latest",
                           overwrite = TRUE, verbose = TRUE,
                           solve_discrepancy = "TRUE"))
  expect_error(get_florabr(output_dir = my_dir, data_version = "latest",
                           overwrite = "TRUE", verbose = TRUE,
                           solve_discrepancy = TRUE))

})
#unlink(my_dir, recursive = T, force = T)
