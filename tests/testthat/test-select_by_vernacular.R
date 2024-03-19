test_that("select_by_vernacular works", {
  data("bf_data") #Load Brazilian Flora data
  #Search for species whose vernacular name is 'pinheiro'
  pinheiro_exact <- select_by_vernacular(data = bf_data,
                                         names = "pinheiro",
                                         exact = TRUE)
  expect_equal(class(pinheiro_exact), "data.frame")
  expect_equal(ncol(pinheiro_exact), ncol(bf_data))
  expect_true(all(grepl("pinheiro", pinheiro_exact$vernacularName)))

  ####It does not work when we don't set data and names
  expect_error(select_by_vernacular(names = "pinheiro",
                                    exact = TRUE))
  expect_error(select_by_vernacular(data = bf_data,
                                    exact = TRUE))

})

