test_that(".rba_stg works", {

  expect_error(
    object = .rba_stg("qwerty"),
    regexp = NULL
  )

  expect_type(
    object = .rba_stg("reactome", "pth", "analysis"),
    type = "character"
  )

  expect_type(
    object = .rba_stg("reactome", "err_prs")[[2]],
    type = "closure"
  )

})

test_that(".rba_stg reports only supplied traversal arguments", {

  expect_error(
    object = .rba_stg("reactome", "pth"),
    regexp = "Traversal: `reactome` -> `pth`$"
  )

})

test_that(".rba_error_parser reports parsed, raw, and absent responses", {

  make_response <- function(status_code,
                            content,
                            content_type = "application/json") {
    structure(
      list(
        url = "https://reactome.org/ContentService/data/query/not-an-id",
        status_code = status_code,
        headers = structure(
          list(content_type),
          names = "content-type"
        ),
        content = charToRaw(content)
      ),
      class = "response"
    )
  }

  parsed_response <- make_response(
    status_code = 404L,
    content = '{"messages":["PARSED_MESSAGE_SENTINEL"]}'
  )
  parsed_message <- .rba_error_parser(parsed_response)$result
  expect_type(parsed_message, "character")
  expect_length(parsed_message, 1L)
  expect_match(
    parsed_message,
    "Reactome.*404.*Not Found",
    ignore.case = TRUE
  )
  expect_match(
    parsed_message,
    "PARSED_MESSAGE_SENTINEL",
    fixed = TRUE
  )

  raw_response <- make_response(
    status_code = 503L,
    content = "RAW_RESPONSE_SENTINEL",
    content_type = "text/plain"
  )
  raw_message <- .rba_error_parser(raw_response)$result
  expect_match(
    raw_message,
    "RAW_RESPONSE_SENTINEL",
    fixed = TRUE
  )

  absent_response <- make_response(
    status_code = 404L,
    content = ""
  )
  absent_message <- .rba_error_parser(absent_response)$result
  absent_lines <- strsplit(absent_message, "\n", fixed = TRUE)[[1]]

  expect_true(length(absent_lines) >= 2L && all(nzchar(absent_lines)))

})
