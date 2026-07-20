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

test_that("an example synthetic 4xx error response parses correctly", {

  response <- structure(
    list(
      url = paste0(
        "https://www.pantherdb.org/services/",
        "oai/pantherdb/geneinfo"
      ),
      status_code = 400L,
      headers = structure(
        list("application/json"),
        names = "content-type"
      ),
      content = charToRaw(
        '{"search":{"error":"Invalid gene identifier"}}'
      )
    ),
    class = "response"
  )

  error_message <- .rba_error_parser(response)

  expect_match(
    object = error_message,
    regexp = "PANTHER server returned"
  )

  expect_match(
    object = error_message,
    regexp = "Invalid gene identifier"
  )

})
