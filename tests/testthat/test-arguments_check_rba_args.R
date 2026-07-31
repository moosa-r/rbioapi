test_that(".rba_args works", {

  .rba_args_nested <- function(cons = NULL,
                               cond = NULL,
                               cond_warning = FALSE,
                               ...) {
    ext_args <- list(...)
    for (i in seq_along(ext_args)) {
      assign(x = names(ext_args)[[i]],
             value = ext_args[[i]])
    }
    .rba_args(cons = cons, cond = cond, cond_warning = cond_warning)
  }

  cons_correct <- list(
    list(arg = "arg1", class = "numeric"),
    list(arg = "arg2", class = "logical")
  )

  cons_incorrect <- list(
    list(arg = "arg1", class = "logical"),
    list(arg = "arg2", class = "logical")
  )
  cond_input <- list(
    list(quote(bg < sml), "bigger < smaller")
  )

  # Runs without any side effects if no argument error was found
  expect_invisible(call = .rba_args_nested())
  expect_invisible(
    call = .rba_args_nested(cons = cons_correct, arg1 = 1, arg2 = TRUE)
  )
  expect_invisible(
    call = .rba_args_nested(cond = cond_input, bg = 2, sml = 1)
  )

  # Detects the errors
  expect_error(
    object = .rba_args_nested(cons = cons_incorrect, arg1 = 1, arg2 = TRUE)
  )
  expect_error(
    object = .rba_args_nested(cond = cond_input, bg = 1, sml = 2)
  )

  # Options appended correctly
  expect_error(
    object = .rba_args_nested(
      cons = cons_correct,
      arg1 = 1,
      arg2 = TRUE,
      verbose = 123
    ),
    regexp = "verbose"
  )
  expect_error(
    object = .rba_args_nested(cons = cons_correct,
                              arg1 = 1,
                              arg2 = TRUE,
                              save_file = "qwerty"
    ),
    regexp = "save_file"
  )

  # Create error if arg does not exist
  expect_error(
    object = .rba_args_nested(
      cons = list(
        list(arg = "arg22", class = "logical")
      )
    ),
    regexp = "arg22"
  )

  # A stale value outside the caller must not satisfy a missing local argument
  stale_limit <- 10
  expect_error(
    object = .rba_args_nested(
      cons = list(
        list(arg = "stale_limit", class = "numeric")
      )
    ),
    regexp = "stale_limit"
  )

  # Drops the argument if class is not correct
  expect_error2(
    obj = .rba_args_nested(
      cons = list(
        list(arg = "arg1", class = "character", regex = "^222$")
      ),
      arg1 = 111
    ),
    pattern = "222",
    invert = TRUE
  )

  # Collects every error and issues them at once
  expect_error2(
    obj = .rba_args_nested(
      cons = list(
        list(arg = "arg1", class = "character"),
        list(arg = "arg2", class = "numeric", min_val = 100),
        list(arg = "arg_missing", class = "numeric")
      ),
      arg1 = 10,
      arg2 = 50
    ),
    pattern = c("arg1", "arg2", "arg_missing")
  )

  # Warnings are respected
  expect_warning(
    object = .rba_args_nested(
      cond = cond_input,
      cond_warning = TRUE,
      bg = 1,
      sml = 2
    )
  )
  cond_input <- list(
    list(
      quote(bg < sml),
      "bigger < smaller",
      warn = TRUE)
  )
  expect_warning(
    object = .rba_args_nested(cond = cond_input, bg = 1, sml = 2)
    )

  # A vectorized condition must not silently pass because its result is not TRUE
  expect_error(
    object = .rba_args_nested(
      cond = list(
        list(quote(values < threshold), "values should meet the threshold")
      ),
      values = c(2, 0),
      threshold = 1
    ),
    regexp = "one non-missing logical"
  )

  # Aggregated failures retain every message and report the correct count
  expect_error2(
    obj = .rba_args_nested(
      cond = list(
        list(quote(minimum > maximum), "minimum exceeds maximum"),
        list(quote(value < minimum), "value is below minimum")
      ),
      value = 0,
      minimum = 2,
      maximum = 1
    ),
    pattern = c(
      "2 Conditional Issues",
      "minimum exceeds maximum",
      "value is below minimum"
    )
  )

})
