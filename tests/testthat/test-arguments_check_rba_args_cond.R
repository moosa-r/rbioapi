test_that(".rba_args_cond works", {
  .rba_args_cond_nested <- function(...) {
    x <- 1
    y <- 2
    .rba_args_cond_nested2 <- function(...) {
      .rba_args_cond_nested3 <- function(...) {
        .rba_args_cond(...)
      }
      .rba_args_cond_nested3(...)
    }
    .rba_args_cond_nested2(...)
  }
  # The the format is checked:
  expect_error(object = .rba_args_cond_nested(cond_i = list(TRUE)))
  expect_error(object = .rba_args_cond_nested(cond_i = list(quote(x < y),
                                                            "sfsdf",
                                                            TRUE,
                                                            "dsf")))
  expect_list_classes(obj = .rba_args_cond_nested(cond_i = list(quote(x < y))),
                      classes = c("character", "logical"))
  expect_list_classes(obj = .rba_args_cond_nested(cond_i = list("x < y")),
                      classes = c("character", "logical"))

  ## Different cond_i types work
  # Condition with length 1 (checked above)
  # Condition with length 2, second element is message
  cond_i_l2_msg <- .rba_args_cond_nested(cond_i = list(quote(x < y),
                                                       "error_msg") )
  expect_list_classes(obj = cond_i_l2_msg, classes = c("character", "logical"))
  expect_regex(obj = cond_i_l2_msg[[1]], pattern = "error_msg")
  expect_false(object = cond_i_l2_msg[[2]])

  # Condition with length 2, second element is warning switch
  cond_i_l2_warn <- .rba_args_cond_nested(cond_i = list(quote(x < y),
                                                        TRUE))
  expect_list_classes(obj = cond_i_l2_warn, classes = c("character", "logical"))
  expect_true(object = cond_i_l2_warn[[2]])

  # Condition with length 3
  cond_i_l3 <- .rba_args_cond_nested(cond_i = list(quote(x < y),
                                                   "error_msg",
                                                   TRUE))
  expect_list_classes(obj = cond_i_l3, classes = c("character", "logical"))
  expect_regex(obj = cond_i_l3[[1]], pattern = "error_msg")
  expect_true(object = cond_i_l3[[2]])


})
