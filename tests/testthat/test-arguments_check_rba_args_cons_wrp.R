test_that(".rba_args_cons_wrp works", {
  # Optional NULL values are ignored
  cons_i_NULL <- list(arg = "arg_X",
                      evl_arg = NULL)
  expect_na(obj = .rba_args_cons_wrp(cons_i = cons_i_NULL))

  # Required NULL values are not ignored
  cons_i_no_NULL <- list(arg = "arg_X",
                         no_null = TRUE,
                         evl_arg = NULL)
  expect_regex(obj = .rba_args_cons_wrp(cons_i = cons_i_no_NULL),
               pattern = "NULL")

  # Correct values are ignored
  cons_i_correct <- list(arg = "arg_X",
                         evl_arg = 111,
                         class = "numeric",
                         val = c(111, 222, 333),
                         ran = c(110,112),
                         len = 1,
                         min_len = 1,
                         max_len = 1,
                         min_val = 111,
                         max_val = 111,
                         regex = "^111$")
  expect_na(obj = .rba_args_cons_wrp(cons_i_correct))

  # All errors are translated into messages
  cons_i_incorrect <- list(arg = "arg_X",
                           evl_arg = 111,
                           class = "character",
                           val = c(222, 333),
                           ran = c(112,113),
                           len = 3,
                           min_len = 2,
                           max_len = 0,
                           min_val = 112,
                           max_val = 110,
                           regex = "^$")
  expect_length(object = .rba_args_cons_wrp(cons_i_incorrect),
                n = length(setdiff(names(cons_i_incorrect), c("arg", "class", "evl_arg", "no_null"))))

})
