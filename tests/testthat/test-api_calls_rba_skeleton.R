test_that(".rba_skeleton works", {
  timeout <- 30
  skip_error <- TRUE
  # Output structure is correct
  request <- .rba_httr(httr = "get",
                       url = "https://httpbin.org",
                       path = "get",
                       accept = "application/json",
                       parser = "json->list")
  expect_class(obj = .rba_skeleton(input_call = request),
               expected = "list")

  # Parser can be overridden
  expect_class(obj = .rba_skeleton(input_call = request,
                                   response_parser = function(x) {x}),
               expected = "response")

  # Respect options
  request <- .rba_httr(httr = "get",
                       url = "https://httpbin.org",
                       path = "not_valid_path02091375",
                       accept = "application/json",
                       parser = "json->list")

  skip_error <- TRUE
  expect_regex(obj = .rba_skeleton(input_call = request),
               pattern = "404")
  skip_error <- FALSE
  expect_error(obj = .rba_skeleton(input_call = request),
               regexp = "404")

  # Invisibly returns NULL when the parser is Null
  request <- .rba_httr(httr = "get",
                       url = "https://httpbin.org",
                       path = "get",
                       accept = "application/json",
                       parser = NULL)
  expect_null(object = .rba_skeleton(input_call = request))
  expect_invisible(call = .rba_skeleton(input_call = request))
})

