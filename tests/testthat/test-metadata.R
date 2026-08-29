.rba_test_response <- function(status_code = 200L,
                               content = "result",
                               date = as.POSIXct("2026-01-02", tz = "UTC"),
                               url = "https://example.org/api") {
  structure(
    list(
      url = url,
      status_code = status_code,
      headers = structure(
        list("text/plain"),
        names = "content-type"
      ),
      content = charToRaw(content),
      date = date,
      request = structure(list(method = "GET"), class = "request")
    ),
    class = "response"
  )
}

.rba_test_request <- function(response, parsers = list()) {
  list(
    timestamp = response$date,
    call = quote(request()),
    response = response,
    parsers = parsers
  )
}

test_that("single-request metadata follows actual parser execution", {
  old_options <- options(
    rba_metadata = FALSE,
    rba_skip_error = TRUE
  )
  on.exit(options(old_options), add = TRUE)

  response <- .rba_test_response()
  parser_2 <- function(x) { toupper(x) }
  input_call <- list(
    call = quote(response),
    parser = list("text->chr", parser_2)
  )

  plain_result <- .rba_skeleton(input_call)
  expect_identical(plain_result, "RESULT")

  options(rba_metadata = TRUE)
  metadata_result <- .rba_skeleton(input_call)
  request <- rba_metadata(metadata_result)[["requests"]][[1L]]

  result_without_metadata <- metadata_result
  attr(result_without_metadata, "rbioapi_metadata") <- NULL
  expect_identical(result_without_metadata, plain_result)
  expect_identical(
    request[c("timestamp", "call", "response")],
    list(
      timestamp = response$date,
      call = input_call$call,
      response = response
    )
  )
  expect_true(
    length(request$parsers) == 2L &&
      all(vapply(request$parsers, is.function, logical(1)))
  )

  parser_1 <- function(x) { rawToChar(x$content) }
  parser_error <- function(x) { stop("parser failed", call. = FALSE) }
  parser_not_called <- function(x) { stop("should not run", call. = FALSE) }
  failed_result <- .rba_skeleton(
    list(
      call = quote(response),
      parser = list(parser_1, parser_error, parser_not_called)
    )
  )
  expect_identical(
    rba_metadata(failed_result)[["requests"]][[1L]][["parsers"]],
    list(parser_1, parser_error)
  )
})

test_that("metadata aggregation preserves workflow history", {
  parser <- identity
  responses <- lapply(seq_len(4L), function(i) {
    .rba_test_response(
      content = as.character(i),
      date = as.POSIXct("2026-01-02", tz = "UTC") + i,
      url = sprintf("https://example.org/api/%d", i)
    )
  })
  results <- lapply(responses, function(response) {
    .rba_metadata_attach(
      rawToChar(response$content),
      list(.rba_test_request(response, parsers = list(parser)))
    )
  })

  final_result <- .rba_metadata_aggregate(
    results[[1L]],
    results[2:3],
    final_object = list(result = results[[4L]])
  )
  expect_identical(
    lapply(rba_metadata(final_result)[["requests"]], `[[`, "response"),
    responses
  )
  expect_null(rba_metadata(final_result$result))
})

test_that("response-producing retry attempts are retained", {
  old_options <- options(
    rba_metadata = TRUE,
    rba_skip_error = TRUE
  )
  on.exit(options(old_options), add = TRUE)

  response_1 <- .rba_test_response(status_code = 500L, content = "retry")
  response_2 <- .rba_test_response(
    status_code = 200L,
    content = "complete",
    date = as.POSIXct("2026-01-03", tz = "UTC")
  )
  attempts <- 0L
  next_response <- function() {
    attempts <<- attempts + 1L
    list(response_1, response_2)[[attempts]]
  }
  parser <- function(x) { rawToChar(x$content) }

  local_mocked_bindings(
    .rba_net_handle = function(...) { TRUE },
    .package = "rbioapi"
  )

  result <- .rba_skeleton(
    list(call = quote(next_response()), parser = parser)
  )
  requests <- rba_metadata(result)[["requests"]]

  expect_identical(
    lapply(
      requests,
      function(request) request[c("response", "parsers")]
    ),
    list(
      list(response = response_1, parsers = list()),
      list(response = response_2, parsers = list(parser))
    )
  )
})

test_that("metadata printing separates a common host", {
  responses <- list(
    .rba_test_response(url = "https://example.org/api/first"),
    .rba_test_response(url = "https://example.org/api/second")
  )
  printed_urls <- function(responses) {
    metadata <- structure(
      list(
        rbioapi_version = "test",
        requests = lapply(responses, .rba_test_request)
      ),
      class = "rba_metadata"
    )
    grep(
      "^(host:|    (https?://|/))",
      capture.output(print(metadata)),
      value = TRUE
    )
  }

  expect_identical(
    printed_urls(responses[1L]),
    c(
      "host: https://example.org",
      "    /api/first"
    )
  )
  expect_identical(
    printed_urls(responses),
    c(
      "host: https://example.org",
      "    /api/first",
      "    /api/second"
    )
  )
})
