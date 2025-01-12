test_that(".rba_query works", {

  # Init properly
  expect_identical(
    object = .rba_query(init = list()),
    expected = list()
  )

  expect_named(
    object = .rba_query(init = list(), list("par3", TRUE, 1)),
    expected = "par3"
  )

  # Add properly
  init_input <- list(par1 = 1, par2 = "second_parameter")

  expect_identical(
    object = .rba_query(init = init_input),
    expected = init_input
  )

  expect_named(
    object = .rba_query(
      init = init_input,
      list("par3", FALSE, 1),
      list("par4", TRUE, 1)
    ),
    expected = c("par1", "par2", "par4")
  )

  expect_identical(
    object = .rba_query(
      init = list(),
      list("par3", TRUE, 1))[["par3"]],
    expected = 1
  )

})
