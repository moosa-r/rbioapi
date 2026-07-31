test_that(".rba_http_status works", {

  expect_type(
    object = .rba_http_status("200", verbose = TRUE),
    type = "character"
  )
  expect_regex(
    obj = .rba_http_status("599", verbose = TRUE),
    pattern = "server error",
    ignore.case = TRUE
  )
  expect_regex(
    obj = .rba_http_status("499", verbose = TRUE),
    pattern = "client error",
    ignore.case = TRUE
  )
  expect_error(
    object = .rba_http_status("999999", verbose = TRUE),
    regexp = "single three-digit HTTP status code"
  )

})
