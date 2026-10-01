test_that("get_florabr validates its arguments", {
  my_dir <- tempfile("florabr-test-")
  dir.create(my_dir)
  on.exit(unlink(my_dir, recursive = TRUE), add = TRUE)

  missing_dir <- tempfile("florabr-missing-")

  expect_error(
    get_florabr(data_version = "latest"),
    "output_dir"
  )
  expect_error(
    get_florabr(output_dir = TRUE),
    "output_dir"
  )
  expect_error(
    get_florabr(output_dir = missing_dir),
    "existing directory"
  )
  expect_error(
    get_florabr(output_dir = my_dir, data_version = "any"),
    "data_version"
  )
  expect_error(
    get_florabr(output_dir = my_dir, data_version = TRUE),
    "data_version"
  )
  expect_error(
    get_florabr(output_dir = my_dir, solve_discrepancy = "TRUE"),
    "solve_discrepancy"
  )
  expect_error(
    get_florabr(output_dir = my_dir, overwrite = "TRUE"),
    "overwrite"
  )
  expect_error(
    get_florabr(output_dir = my_dir, get_fixed_version = "TRUE"),
    "get_fixed_version"
  )
})

test_that("get_florabr reports an unavailable IPT", {
  my_dir <- tempfile("florabr-ipt-error-")
  dir.create(my_dir)
  on.exit(unlink(my_dir, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    ipt_latest_version = function(...) {
      stop("Forbidden (HTTP 403)")
    },
    .package = "florabr"
  )

  expect_error(
    get_florabr(
      output_dir = my_dir,
      data_version = "latest",
      verbose = FALSE
    ),
    "Forbidden \\(HTTP 403\\)"
  )
})

test_that("get_florabr validates fixed-version restrictions", {
  my_dir <- tempfile("florabr-fixed-test-")
  dir.create(my_dir)
  on.exit(unlink(my_dir, recursive = TRUE), add = TRUE)

  expect_error(
    get_florabr(
      output_dir = my_dir,
      data_version = "393.431",
      get_fixed_version = TRUE,
      verbose = FALSE
    ),
    "provides only version 393.432"
  )

  expect_error(
    get_florabr(
      output_dir = my_dir,
      get_fixed_version = TRUE,
      solve_discrepancy = TRUE,
      verbose = FALSE
    ),
    "solve_discrepancy"
  )

  # An existing destination must not be overwritten when overwrite = FALSE.
  version_dir <- file.path(my_dir, "393.432")
  dir.create(version_dir)
  file.create(file.path(version_dir, "CompleteBrazilianFlora.gz"))

  expect_error(
    get_florabr(
      output_dir = my_dir,
      get_fixed_version = TRUE,
      overwrite = FALSE,
      verbose = FALSE
    ),
    "already exists"
  )
})
