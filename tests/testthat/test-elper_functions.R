test_that("rba_options works", {

  # Return data frame when called empty
  expect_class(obj = rba_options(),expected = "data.frame")

  # Changes option
  rba_options(timeout = 91)
  expect_true(object = (getOption("rba_timeout") == 91))

  # Checks arguments
  expect_error(object = rba_options(verbose = 123), regexp = "logical")
  verbose_before <- getOption("rba_verbose")
  expect_error(object = rba_options(verbose = NA), regexp = "verbose.*NA")
  expect_identical(object = getOption("rba_verbose"), expected = verbose_before)
  expect_error(object = rba_options(save_file = "test.txt"), regexp = "logical")

  retry_before <- options("rba_retry_max", "rba_retry_wait")
  on.exit(options(retry_before), add = TRUE)

  expect_error(object = rba_options(retry_max = -1), regexp = "retry_max")
  expect_error(
    object = rba_options(retry_max = Inf),
    regexp = "retry_max.*finite whole numbers"
  )
  expect_error(
    object = rba_options(retry_wait = Inf),
    regexp = "retry_wait"
  )
  expect_identical(
    object = options("rba_retry_max", "rba_retry_wait"),
    expected = retry_before
  )

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
  expect_error(
    object = rba_pages(input_call = Sys.sleep(0)),
    regexp = "qoute"
  )
  expect_error(
    object = rba_pages(input_call = quote(Sys.sleep(0))),
    regexp = "rbioapi"
  )
  expect_error(
    object = rba_pages(input_call = quote(rba_test(3))),
    regexp = "pages"
  )
  expect_error(
    object = rba_pages(input_call = quote(rba_test(3))),
    regexp = "pages"
  )
  expect_error(
    object = rba_pages(input_call = quote(rba_test("pages:1:999"))),
    regexp = "100"
  )

})
