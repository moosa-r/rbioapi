test_that(".rba_api_check works", {
  expect_true(object = .rba_api_check("https://httpbin.org"))
  expect_regex(obj = .rba_api_check("https://httpbin.org/not_valid_path02091375"),
               pattern = "404")
})

test_that("rba_connection_test works", {
  expect_named(object = rba_connection_test(print_output = FALSE,
                                            diagnostics = FALSE),
               expected = names(.rba_stg("tests")))
})

test_that("rba_options works", {
  # Return data frame when called empty
  expect_class(obj = rba_options(),
               expected = "data.frame")
  # Changes option
  rba_options(timeout = 91)
  expect_true(object = (getOption("rba_timeout") == 91))

  # Checks arguments
  expect_error(object = rba_options(verbose = 123),
               regexp = "logical")
  expect_error(object = rba_options(save_file = "test.txt"),
               regexp = "logical")

})

test_that("rba_pages works", {
  rba_test <- function(x, skip_error = NULL, ...) {
    if (isTRUE(skip_error)) {
      LETTERS[[x]]
    } else {
      paste0(LETTERS[[x]], "!", collapse = "")
    }
  }
  # Detects errors
  expect_error(object = rba_pages(input_call = Sys.sleep(0)),
               regexp = "qoute")
  expect_error(object = rba_pages(input_call = quote(Sys.sleep(0))),
               regexp = "rbioapi")
  expect_error(object = rba_pages(input_call = quote(rba_test(3))),
               regexp = "pages")
  expect_error(object = rba_pages(input_call = quote(rba_test(3))),
               regexp = "pages")
  expect_error(object = rba_pages(input_call = quote(rba_test("pages:1:999"))),
               regexp = "100")
  ### works in manual mode but not automated
  # # Works
  # expect_identical(object = rba_pages(quote(rba_test("pages:1:2")),
  #                                     sleep_time = 0,
  #                                     verbose = FALSE),
  #                  expected = list(page_1 = "A", page_2 = "B"))
  #
  # # Respect internal options
  # expect_identical(object = rba_pages(quote(rba_test("pages:3:2")),
  #                                     page_check = FALSE,
  #                                     add_skip_error = FALSE,
  #                                     list_names = 3:2,
  #                                     sleep_time = 0,
  #                                     verbose = FALSE),
  #                  expected = list("3" = "C!", "2" = "B!"))
})
