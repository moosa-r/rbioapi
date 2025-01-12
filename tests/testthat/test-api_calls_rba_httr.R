test_that(".rba_httr return output currectly", {

  # Output structure is correct
  base_output <- .rba_httr(
    httr = "get",
    url = "url_value",
    path = "path_value",
    accept = "accept_vale",
    save_to = "save_to_value"
  )

  expect_named(object = base_output, expected = c("call", "parser"))

  expect_list_classes(obj = base_output, classes = c("call", "function"))

  # Call is correct
  expect_call_regex(obj = base_output$call, pattern = "^httr::(GET|POST|HEAD)")
  expect_call_regex(obj = base_output$call, pattern = "url\\s*=\\s*\"url_value\"")
  expect_call_regex(obj = base_output$call, pattern = "path\\s*=\\s*\"path_value\"")
  expect_call_regex(obj = base_output$call, pattern = "httr::timeout\\(timeout\\)")
  expect_call_regex(obj = base_output$call, pattern = "httr::accept\\(\"accept_vale\"\\)")
  expect_call_regex(obj = base_output$call, pattern = "httr::write_disk\\(\"save_to_value\".*\\)")

})
