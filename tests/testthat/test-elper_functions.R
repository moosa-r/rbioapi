test_that("rba_options works", {
  # Return data frame when called empty
  expect_class(obj = rba_options(),
               expected = "data.frame")
  # Changes option
  rba_options(timeout = 91)
  expect_true(object = (getOption("rba_timeout") == 91))

  # Checks arguments
  expect_error(object = rba_options(verbose = 123),
               regexp = "logical")
  expect_error(object = rba_options(save_file = "test.txt"),
               regexp = "logical")

})

test_that("rba_pages works", {
  rba_test <- function(x, skip_error = NULL, ...) {
    if (isTRUE(skip_error)) {
      LETTERS[[x]]
    } else {
      paste0(LETTERS[[x]], "!", collapse = "")
    }
  }
  # Detects errors
  expect_error(object = rba_pages(input_call = Sys.sleep(0)),
               regexp = "qoute")
  expect_error(object = rba_pages(input_call = quote(Sys.sleep(0))),
               regexp = "rbioapi")
  expect_error(object = rba_pages(input_call = quote(rba_test(3))),
               regexp = "pages")
  expect_error(object = rba_pages(input_call = quote(rba_test(3))),
               regexp = "pages")
  expect_error(object = rba_pages(input_call = quote(rba_test("pages:1:999"))),
               regexp = "100")
})
