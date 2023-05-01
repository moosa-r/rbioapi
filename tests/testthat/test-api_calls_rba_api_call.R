test_that(".rba_api_call works", {
  skip_if_not(test_is_online)

  rba_api_call_nested <- function(..., timeout_input = 30) {
    timeout <- timeout_input
    rba_api_call_nested2 <- function(...) {
      .rba_api_call(...)
    }
    rba_api_call_nested2(...)
  }

  # Output structure is correct
  request <- .rba_httr(httr = "head",
                       url = "https://httpbin.org")
  expect_class(obj = rba_api_call_nested(input_call = request$call,
                                         skip_error = FALSE,
                                         verbose = TRUE,
                                         diagnostics = FALSE),
               expected = "response")

  # Properly return error
  # request <- .rba_httr(httr = "head",
  #                      url = "http://sdvsdgv.sgsdg",
  #                      path = "sfs")
  # expect_error(object = rba_api_call_nested(input_call = request$call,
  #                                           skip_error = FALSE,
  #                                           verbose = TRUE,
  #                                           diagnostics = FALSE),
  #              regexp = "resolve")
  request <- .rba_httr(httr = "head",
                       url = "https://httpbin.org",
                       path = "not_valid_path02091375")
  expect_error(object = rba_api_call_nested(input_call = request$call,
                                            skip_error = FALSE,
                                            verbose = TRUE,
                                            diagnostics = FALSE),
               regexp = "404|408|429|504|Timeout")

  # Respect skip_error
  expect_regex(obj = rba_api_call_nested(input_call = request$call,
                                         skip_error = TRUE,
                                         verbose = TRUE,
                                         diagnostics = FALSE),
               pattern = "404|408|429|504|Timeout")
  ## Trims output
  # request <- .rba_httr(httr = "head",
  #                      url = "https://googlvjhvjhjvje.com",
  #                      path = "search")
  # expect_regex(obj = rba_api_call_nested(input_call = request$call,
  #                                        skip_error = TRUE,
  #                                        verbose = TRUE,
  #                                        diagnostics = FALSE),
  #              pattern = "Error in",
  #              invert = TRUE)
})
