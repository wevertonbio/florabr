test_that("check_version reports an available version", {
  my_dir <- tempfile("florabr-check-")
  version_dir <- file.path(my_dir, "393.432")
  dir.create(version_dir, recursive = TRUE)
  on.exit(unlink(my_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(
    version_dir, "CompleteBrazilianFlora.gz"
  ))

  testthat::local_mocked_bindings(
    ipt_latest_version = function(...) "393.432",
    .package = "florabr"
  )

  expect_message(
    check_version(my_dir),
    "You have the latest version"
  )
})

test_that("check_version handles an unavailable IPT", {
  my_dir <- tempfile("florabr-check-offline-")
  dir.create(my_dir)
  on.exit(unlink(my_dir, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    ipt_latest_version = function(...) {
      stop("Forbidden (HTTP 403)")
    },
    .package = "florabr"
  )

  expect_message(
    check_version(my_dir),
    "could not be verified"
  )
})
