test_that(".msg works", {
  # basics
  verbose <- FALSE
  expect_silent(.msg("test"))
  verbose <- TRUE
  expect_message(.msg("test"), regexp = "test")
  verbose2 <- FALSE
  expect_silent(.msg("test", cond = "verbose2"))
  # use paste
  expect_message(object = .msg("1", "2", "3"),
                 regexp = "123")
  #use sprintf
  expect_message(object = .msg("1%s%s", "2", "3"),
                 regexp = "123")
  #force paste
  expect_message(object = .msg("1%s%s", "2", "3", sprintf = FALSE),
                 regexp = "1%s%s23")

})
