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
  local_mocked_bindings(
    rba_panther_info = function(what,
                                families_page,
                                skip_error = FALSE,
                                verbose = TRUE,
                                progress = TRUE,
                                ...) {
      if (identical(what, "failure") && families_page == 2) {
        if (isTRUE(skip_error)) {
          return("endpoint failure")
        }
        stop("endpoint failure")
      }

      if (identical(what, "retain_null") && families_page == 2) {
        return(NULL)
      }

      if (identical(what, "options")) {
        return(c(verbose = verbose, progress = progress))
      }

      return(families_page)
    },
    .package = "rbioapi"
  )
  local_mocked_bindings(
    Sys.sleep = function(...) NULL,
    .package = "base"
  )
  old_options <- options(rba_verbose = FALSE)
  on.exit(options(old_options), add = TRUE)

  # Both pagination forms preserve caller values, page order, and NULL results
  caller_value <- "retain_null"
  explicit_output <- rba_pages(
    input_call = quote(rba_panther_info(what = caller_value)),
    page_arg = "families_page",
    pages = 3:1
  )
  range_output <- rba_pages(
    input_call = quote(
      rbioapi::rba_panther_info(
        what = caller_value,
        families_page = "pages:3:1"
      )
    )
  )

  expected_output <- list(page_3 = 3L, page_2 = NULL, page_1 = 1L)
  expect_equal(
    object = explicit_output,
    expected = expected_output
  )
  expect_equal(object = range_output, expected = expected_output)

  # The wrapper's skip_error policy controls the complete operation
  continued_output <- rba_pages(
    input_call = quote(rba_panther_info(what = "failure")),
    page_arg = "families_page",
    pages = 1:3,
    skip_error = TRUE
  )
  expect_equal(
    object = continued_output,
    expected = list(
      page_1 = 1L,
      page_2 = "endpoint failure",
      page_3 = 3L
    )
  )
  expect_error(
    object = rba_pages(
      input_call = quote(rba_panther_info(what = "failure")),
      page_arg = "families_page",
      pages = 1:3,
      skip_error = FALSE
    ),
    regexp = "endpoint failure"
  )

  # One wrapper progress bar suppresses output from the individual calls
  capture.output(
    progress_output <- suppressWarnings(
      rba_pages(
        input_call = quote(
          rba_panther_info(
            what = "options",
            verbose = TRUE,
            progress = TRUE
          )
        ),
        page_arg = "families_page",
        pages = 1,
        progress = TRUE
      )
    )
  )
  expect_equal(
    object = progress_output,
    expected = list(page_1 = c(verbose = FALSE, progress = FALSE))
  )

})
