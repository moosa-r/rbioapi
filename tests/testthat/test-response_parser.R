test_that(".rba_response_parser works", {
  skip_if_not(test_is_online)

  resp <- httr::GET(url = "https://httpbin.org/json",
                    httr::accept("application/json"))

  # One parser
  expect_class(obj = .rba_response_parser(response = resp,
                                          parsers = "json->df"),
               expected = "data.frame")
  # Series of parsers
  expect_class(obj = .rba_response_parser(response = resp,
                                          parsers = list("json->df",
                                                         as.list)),
               expected = "list")
})

test_that(".rba_error_parser works", {
  skip_if_not(test_is_online)

  resp <- httr::GET(url = "https://reactome.org/ContentService/data/complex/hkbkmhbkm/subunits",
                    httr::accept("application/json"))
  # Basics functionality
  expect_class(obj = .rba_error_parser(response = resp),
               expected = "character")

  ## Pass to .rba_http_status for unknown servers
  resp <- httr::GET(url = "https://httpbin.org/not_valid_path02091375",
                    httr::accept("application/json"))
  expect_identical(object = .rba_error_parser(response = resp,
                                              verbose = FALSE),
                   expected = .rba_http_status(http_status = httr::status_code(resp),
                                               verbose = FALSE))
})
