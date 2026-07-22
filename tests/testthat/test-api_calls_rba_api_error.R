test_that("API errors stop the response parser", {

  api_error <- .rba_api_error("The API reported an error")

  expect_identical(
    class(api_error),
    c("rba_api_error", "character")
  )
  parsed_response <- .rba_response_parser(
    response = NULL,
    parsers = list(
      function(x) {
        api_error
      },
      function(x) {
        stop("The parser chain did not stop", call. = FALSE)
      }
    )
  )

  expect_identical(
    parsed_response,
    api_error
  )

})

test_that(".rba_skeleton distinguishes API errors from parser failures", {

  response <- structure(
    list(
      url = "https://example.org/api",
      status_code = 200L,
      headers = structure(
        list("application/json"),
        names = "content-type"
      ),
      content = charToRaw(
        '{"error":"The API reported an error"}'
      )
    ),
    class = "response"
  )

  api_error_call <- list(
    call = quote(response),
    parser = list(
      "json->list_simp",
      function(x) {
        .rba_api_error(x[["error"]])
      }
    )
  )

  diagnostics <- FALSE
  verbose <- FALSE
  retry_max <- 0
  retry_wait <- 0

  skip_error <- TRUE
  expect_identical(
    .rba_skeleton(api_error_call),
    "The API reported an error"
  )

  skip_error <- FALSE
  expect_error(
    object = .rba_skeleton(api_error_call),
    regexp = "The API reported an error",
    fixed = TRUE
  )

  parser_failure_call <- list(
    call = quote(response),
    parser = function(x) {
      stop("The response format changed", call. = FALSE)
    }
  )

  skip_error <- TRUE

  expect_match(
    object = .rba_skeleton(parser_failure_call),
    regexp = "Internal Error: Failed to parse the server's response",
    fixed = TRUE
  )

})
