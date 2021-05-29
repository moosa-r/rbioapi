test_that(".rba_stg works", {
  expect_error(object = .rba_stg("qwerty"), regexp = NULL)

  expect_type(object = .rba_stg("reactome", "pth", "analysis"),
              type = "character")

  expect_type(object = .rba_stg("reactome", "err_prs")[[2]],
              type = "closure")
})
