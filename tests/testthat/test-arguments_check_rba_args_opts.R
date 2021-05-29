test_that(".rba_args_opts works", {
  .rba_args_opts_nested <- function(...) {
    dir_name <- "test"
    save_file <- TRUE
    .rba_args_opts_nested2 <- function(...) {
      .rba_args_opts(...)
    }
    .rba_args_opts_nested2(...)
  }
  expect_has_names(obj = .rba_args_opts_nested(what = "cons"),
                   expected = c("dir_name", "save_file"))
  expect_has_names(obj = .rba_args_opts_nested(what = "cond"),
                   expected = c("dir_name", "save_file"))
})
