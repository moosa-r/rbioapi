test_that(".rba_args_cons_chk works", {

  # Skip NULL args
  expect_true(
    object = .rba_args_cons_chk(cons_i = list(evl_arg = NULL), what = "class")
  )
  # Produce error for wrong constrains
  expect_error(
    object = .rba_args_cons_chk(cons_i = list(evl_arg = "string"), what = "qwer")
  )

  # Integerish only applies to numeric values, which should also be finite
  expect_true(
    object = .rba_args_cons_chk(
      cons_i = list(evl_arg = c("human", "9606"), integerish = TRUE),
      what = "integerish"
    )
  )
  expect_false(
    object = .rba_args_cons_chk(
      cons_i = list(evl_arg = Inf, integerish = TRUE),
      what = "integerish"
    )
  )

  # Detect correct values
  cons_i_correct <- list(
    arg = "arg_X",
    evl_arg = 111,
    class = "numeric",
    val = c(111, 222, 333),
    ran = c(110,112),
    integerish = TRUE,
    len = 1,
    min_len = 1,
    max_len = 1,
    min_val = 111,
    max_val = 111,
    regex = "^111$"
  )

  expect_true(object = all(
    vapply(
      X = names(cons_i_correct)[3:length(cons_i_correct)],
      FUN = function(x) {
        .rba_args_cons_chk(cons_i = cons_i_correct,
                           what = x)
      },
      FUN.VALUE = logical(1)
    )
  ))

  # Detect incorrect values
  cons_i_incorrect <- list(
    arg = "arg_X",
    evl_arg = 111.5,
    class = "character",
    val = c(222, 333),
    ran = c(112,113),
    integerish = TRUE,
    len = 3,
    min_len = 2,
    max_len = 0,
    min_val = 112,
    max_val = 110,
    regex = "^$"
  )

  expect_true(object = all(
    !vapply(
      X = names(cons_i_incorrect)[3:length(cons_i_incorrect)],
      FUN = function(x) {
        .rba_args_cons_chk(cons_i = cons_i_incorrect,
                           what = x)
      },
      FUN.VALUE = logical(1)
    )
  ))

})
