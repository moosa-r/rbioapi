test_that(".rba_ext_args works", {
  ## adds option to the environment
  opts <- getOption("rba_user_options")
  expect_false(object = length(setdiff(opts, ls())) == 0)
  .rba_ext_args()
  expect_true(object = length(setdiff(opts, ls())) == 0)
  rm(list = opts)

  ## respects supplied options
  .rba_ext_args(verbose = 123)
  expect_true(object = (verbose != getOption("rba_verbose")))
  rm(list = opts)

  ## detects invalid args
  expect_warning(object = .rba_ext_args(qwerty = 123),
                 regexp = "qwerty")
  expect_warning(object = .rba_ext_args(123),
                 regexp = "123")
  rm(list = opts)

  ## ignore save_file option
  expect_warning(object = .rba_ext_args(save_file = TRUE,
                                        ignore_save = TRUE),
                 regexp = "save_file")
  expect_false(object = exists("save_file"))
})
