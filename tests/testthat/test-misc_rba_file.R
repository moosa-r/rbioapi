test_that(".rba_file works", {
  # basics
  expect_false(object = .rba_file(file = "test.txt",
                                  save_to = FALSE))
  expect_message(object = .rba_file(file = "test.txt",
                                    save_to = TRUE),
                 regexp = "test\\.txt")

  verbose <- FALSE
  expect_identical(object = .rba_file(file = "test.txt",
                                      save_to = TRUE,
                                      dir_name = "rbioapi_test"),
                   expected = file.path(getwd(),
                                        "rbioapi_test",
                                        "test.txt"))
  ## accepts different kind of inputs
  # only file name
  expect_identical(object = .rba_file(file = "test.txt",
                                      save_to = TRUE),
                   expected = file.path(getwd(),
                                        getOption("rba_dir_name"),
                                        "test.txt"))
  # full path
  expect_identical(object = .rba_file(file = "test.txt",
                                      save_to = "c:/rbioapi/file.txt"),
                   expected = "c:/rbioapi/file.txt")
  # directory path
  expect_identical(object = .rba_file(file = "test.txt",
                                      save_to = "c:/rbioapi/"),
                   expected = "c:/rbioapi/test.txt")

  ## detects non_valid file paths
  expect_warning(object = .rba_file(file = "test.txt",
                                    save_to = "qwerty"),
                 regexp = "qwerty")
  expect_warning(object = .rba_file(file = "test.txt",
                                    save_to = "qwerty"),
                 regexp = "qwerty")
  expect_identical(object = suppressWarnings(.rba_file(file = "test.txt",
                                                       save_to = "qwerty")),
                   expected = file.path(getwd(),
                                        getOption("rba_dir_name"),
                                        "test.txt"))
  ## detects saving in wrong file extension
  expect_warning(object = .rba_file(file = "test.txt",
                                    save_to = "test.json"),
                 regexp = "txt")
  ## increments existing file names
  dir.create(getwd(), "rbioapi_test")
  writeLines(text = "111", con = file.path("rbioapi_test",
                                           "test.txt"))
  expect_identical(object = .rba_file(file = "test.txt",
                                      save_to = TRUE,
                                      dir_name = "rbioapi_test"),
                   expected = file.path(getwd(),
                                        "rbioapi_test",
                                        "test_1.txt"))
  unlink(file.path(getwd(), "rbioapi_test"),
         recursive = TRUE)
  unlink(file.path(getwd(), "rbioapi"),
         recursive = TRUE)
})
