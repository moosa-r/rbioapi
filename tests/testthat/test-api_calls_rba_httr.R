test_that(".rba_httr return output currectly", {
  # Output structure is correct
  base_output <- .rba_httr(httr = "get",
                           url = "url_value",
                           path = "path_value",
                           accept = "accept_vale",
                           save_to = "save_to_value")

  expect_named(object = base_output,
               expected = c("call", "parser"))

  expect_list_classes(obj = base_output,
                      classes = c("call", "function"))
  # Call is correct
  expect_call_regex(obj = base_output$call,
                    pattern = "^httr::(GET|POST|HEAD)")
  expect_call_regex(obj = base_output$call,
                    pattern = "url\\s*=\\s*\"url_value\"")
  expect_call_regex(obj = base_output$call,
                    pattern = "path\\s*=\\s*\"path_value\"")
  expect_call_regex(obj = base_output$call,
                    pattern = "httr::timeout\\(timeout\\)")
  expect_call_regex(obj = base_output$call,
                    pattern = "httr::accept\\(\"accept_vale\"\\)")
  expect_call_regex(obj = base_output$call,
                    pattern = "httr::write_disk\\(\"save_to_value\".*\\)")

})

test_that(".rba_httr adds respects global options:", {
  diagnostics = TRUE
  progress = TRUE
  timeout = 9999
  output <- .rba_httr(httr = "get",
                      url = "https://httpbin.org",
                      path = "get",
                      save_to = "save_to_value",
                      accept = "accept_value")

  expect_call_regex(obj = output$call,
                    pattern = "httr::verbose\\(.*\\)")
  expect_call_regex(obj = output$call,
                    pattern = "httr::progress\\(\\)")

})

test_that(".rba_httr handles file parser vs object oarsers senario", {
  ### Exported function uses a fixed accept and parser
  output_file <- .rba_httr(httr = "get",
                           url = "https://httpbin.org",
                           path = "get",
                           save_to = getwd(),
                           accept = "accept_value",
                           parser = "parser_value")

  expect_call_regex(obj = output_file,
                    pattern = "httr::accept\\(\"accept_value\"\\)",
                    perl = TRUE)
  expect_identical(object = output_file$parser, expected = "parser_value")

  ### Exported function lets user decided to save file or not:
  ## 1 If user supplied a path...
  output_file <- .rba_httr(httr = "get",
                           url = "https://httpbin.org",
                           path = "get",
                           save_to = getwd(),
                           file_accept = "file_accept_value",
                           obj_accept = "obj_accept_value",
                           file_parser = "file_parser_value",
                           obj_parser = "obj_parser_value")
  # ...file accept value was chosen as HTTP accept header ...
  expect_call_regex(obj = output_file,
                    pattern = "httr::accept\\(\"file_accept_value\"\\)",
                    perl = TRUE)
  # ... and the parser is file parser
  expect_identical(object = output_file$parser, expected = "file_parser_value")

  ## 2 If user didn't supply a path...
  output_obj <- .rba_httr(httr = "get",
                          url = "https://httpbin.org",
                          path = "get",
                          save_to = FALSE,
                          file_accept = "file_accept_value",
                          obj_accept = "obj_accept_value",
                          file_parser = "file_parser_value",
                          obj_parser = "obj_parser_value")
  # Obj accept value was chosen as HTTP accept header ...
  expect_call_regex(obj = output_obj,
                    pattern = "httr::accept\\(\"obj_accept_value\"\\)",
                    perl = TRUE)
  # ... and the parser is file parser
  expect_identical(object = output_obj$parser, expected = "obj_parser_value")
}
)
