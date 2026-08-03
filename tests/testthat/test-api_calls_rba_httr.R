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

test_that(".rba_httr preserves save paths as character values", {

  verbose <- FALSE

  write_disk_path <- function(save_path, ...) {
    input_call <- .rba_httr("get", "url_value", save_to = save_path, ...)
    write_disk <- Filter(
      function(x) is.call(x) && identical(x[[1]], quote(httr::write_disk)),
      as.list(input_call$call)
    )
    return(write_disk[[1]][[2]])
  }

  save_paths <- c(
    windows = "C:\\Users\\Test User\\file \"quoted\".json",
    unix = "/tmp/Test User/file \"quoted\".json",
    tempfile = tempfile(fileext = ".json"),
    tempdir = .rba_file(basename(tempfile(fileext = ".json")), tempdir())
  )

  expect_identical(
    vapply(save_paths, write_disk_path, character(1), USE.NAMES = FALSE),
    unname(save_paths)
  )

  expect_identical(
    write_disk_path(
      save_paths[["windows"]],
      file_accept = "application/json",
      obj_accept = "application/json",
      file_parser = identity
    ),
    save_paths[["windows"]]
  )

})

test_that(".rba_httr preserves selected accept headers as character values", {

  accept_header <- function(...) {
    input_call <- .rba_httr("get", "url_value", ...)
    accept <- Filter(
      function(x) is.call(x) && identical(x[[1]], quote(httr::accept)),
      as.list(input_call$call)
    )
    return(accept[[1]][[2]])
  }

  object_accept <- "application/json"
  file_accept <- "text/x-peff"

  expect_identical(
    accept_header(
      save_to = FALSE,
      file_accept = file_accept,
      obj_accept = object_accept,
      file_parser = identity,
      obj_parser = identity
    ),
    object_accept
  )

  expect_identical(
    accept_header(
      save_to = "result.json",
      file_accept = file_accept,
      obj_accept = object_accept,
      file_parser = identity,
      obj_parser = identity
    ),
    file_accept
  )

})
