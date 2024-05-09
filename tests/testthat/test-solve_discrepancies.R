test_that("solve_discrepancies works", {
  #Load Brazilian Flora data
  data("bf_data")
  #Solve discrepancy
  d <- solve_discrepancies(data = bf_data)
  expect_equal(class(d), "data.frame")
})

test_that("solve_discrepancies does not works", {
  #Load Brazilian Flora data
  data("bf_data")
  #Solve discrepancy
  expect_error(solve_discrepancies(data))
  expect_error(solve_discrepancies(data = TRUE))
  bf_data2 <- bf_data
  attr(bf_data2, "solve_discrepancies") <- TRUE
  expect_error(solve_discrepancies(data = bf_data2))
})

