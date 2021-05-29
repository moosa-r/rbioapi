test_that(".rba_net_handle works", {
  expect_type(object = .rba_net_handle(skip_error = TRUE),
              type = "logical")
})

test_that(".rba_http_status works", {
  expect_type(object = .rba_http_status("200", verbose = TRUE),
              type = "character")
  expect_regex(obj = .rba_http_status("599", verbose = TRUE),
               pattern = "redirection", ignore.case = TRUE)
  expect_error(object = .rba_http_status("999999", verbose = TRUE))
})
