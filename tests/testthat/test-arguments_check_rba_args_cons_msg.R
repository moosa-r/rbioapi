test_that(".rba_args_cons_msg works", {
  # Create messages for incorrect constrains
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

  expect_regex(obj = vapply(X = c("no_null", names(cons_i_incorrect)[3:length(cons_i_incorrect)]),
                            FUN = function(x) {
                              .rba_args_cons_msg(cons_i = cons_i_incorrect,
                                                 what = x)
                            },
                            FUN.VALUE = character(1)),
               pattern = "arg_X")
})
