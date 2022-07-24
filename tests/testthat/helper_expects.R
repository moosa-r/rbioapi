test_is_online <- try(httr::status_code(httr::HEAD("https://httpbin.org",
                                                   httr::timeout(10))),
                      silent = TRUE) == 200


expect_list_classes <- function(obj, classes) {
  stopifnot(inherits(obj, "list"))

  obj_classes <- vapply(X = obj,
                        FUN = function(x) {class(x)},
                        FUN.VALUE = character(1),
                        USE.NAMES = FALSE)

  if (identical(obj_classes, classes)) {
    testthat::succeed()
    return(invisible(obj_classes))
  } else {
    testthat::fail(sprintf("objects elements' classes are: %s but expected classes are: %s",
                           paste0(obj_classes, collapse = ", "), paste0(classes, collapse = ", ")))
  }
}


expect_call_regex <- function(obj, pattern, ...) {
  obj <- paste0(deparse(obj), collapse = "")
  out <- grepl(pattern = pattern, x = obj, ...)

  if (out) {
    testthat::succeed()
    return(invisible(out))
  } else {
    testthat::fail(sprintf("The regex pattern \"%s\" did not match the call object:\n \"%s\"",
                           pattern, obj))
  }

}

expect_regex <- function(obj, pattern, invert = FALSE, ...) {
  out <- vapply(X = obj,
                FUN = function(x,...) {
                  grepl(pattern = pattern, x = x, ...)},
                FUN.VALUE = logical(1),
                USE.NAMES = FALSE,
                ...)
  if (invert) {out <- !out}
  if (all(out)) {
    testthat::succeed()
    return(invisible(out))
  } else {
    testthat::fail(sprintf("The regex pattern \"%s\" did not match the string vector:\n \"%s\"",
                           pattern, paste0(obj, collapse = ", ")))
  }

}

expect_class <- function(obj, expected, ...) {

  if (inherits(obj, expected)) {
    testthat::succeed()
    return(invisible(class(obj)))
  } else {
    testthat::fail(sprintf("Your object's class is `%s`` but `%s`` is expected.",
                           class(obj), expected))
  }

}

expect_has_names <- function(obj, expected) {
  in_obj_not_expected <- setdiff(names(obj), expected)
  in_expected_not_obj <- setdiff(expected, names(obj))
  if (length(c(in_obj_not_expected, in_expected_not_obj)) == 0) {
    testthat::succeed()
    return(invisible(expected))
  } else {
    testthat::fail(sprintf("The names does not match:\n names in object but not expected: %s\n expected name not in object: %s",
                           paste0(in_obj_not_expected, collapse = ", "),
                           paste0(in_expected_not_obj, collapse = ", ")))
  }

}

expect_na <- function(obj) {

  if (all(is.na(obj))) {
    testthat::succeed()
    return(invisible(TRUE))
  } else {
    testthat::fail("The object contain non-NA elements..")
  }

}

expect_error2 <- function(obj, pattern = NULL, invert = FALSE, ...) {
  obj <- try(obj,
             silent = TRUE)
  if (inherits(obj, "try-error")) {
    if (is.null(pattern)) {
      testthat::succeed()
      return(invisible(TRUE))
    } else {
      out <- vapply(X = pattern,
                    FUN = function(patt) {
                      grepl(pattern = patt, x = obj, ...)
                    },
                    FUN.VALUE = logical(1))
      if (invert) {out <- !out}
      if (all(out)) {
        testthat::succeed()
        return(invisible(out))
      } else {
        testthat::fail(sprintf("Error was prodced but the regex pattern(s) %s didn't match.",
                               which(!out)))
      }
    }

  } else {
    testthat::fail("obj runs with no error.")
  }

}
