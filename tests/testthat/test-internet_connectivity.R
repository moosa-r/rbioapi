test_that(".rba_http_status works", {

  expect_regex(
    .rba_http_status("400", as_sentence = TRUE),
    pattern = "\\.$"
  )
  expect_regex(
    obj = .rba_http_status("599", as_sentence = TRUE),
    pattern = "server error",
    ignore.case = TRUE
  )
  expect_regex(
    obj = .rba_http_status("499", as_sentence = TRUE),
    pattern = "client error",
    ignore.case = TRUE
  )
  expect_error(
    object = .rba_http_status("999999", as_sentence = TRUE),
    regexp = NULL
  )

})
