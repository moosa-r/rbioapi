.rba_args_na_check <- function(arg1,
                               class = "numeric",
                               cond = NULL,
                               ...) {
  cons_i <- c(
    list(arg = "arg1", class = class),
    list(...)
  )
  .rba_args(cons = list(cons_i), cond = cond)
}

.rba_args_na_nested <- function(cons = NULL, ...) {
  ext_args <- list(...)
  for (i in seq_along(ext_args)) {
    assign(
      x = names(ext_args)[[i]],
      value = ext_args[[i]]
    )
  }
  .rba_args(cons = cons)
}

.rba_args_na_optional <- function(arg1 = NULL) {
  .rba_args(
    cons = list(list(arg = "arg1", class = "numeric"))
  )
}

test_that("missing values are rejected before other argument checks", {

  missing_cases <- list(
    list(value = NA, class = "numeric"),
    list(value = NaN, class = "numeric"),
    list(value = c(1, NA_real_), class = "numeric"),
    list(value = list(outer = list(NA_character_)), class = "list")
  )

  for (case in missing_cases) {
    expect_error(
      object = .rba_args_na_check(
        arg1 = case[["value"]],
        class = case[["class"]]
      ),
      regexp = "`arg1` cannot contain `NA` or `NaN` values"
    )
  }
})

test_that("missing-value errors precede conditions and aggregate cleanly", {

  expect_error(
    object = .rba_args_na_check(
      arg1 = NA_real_,
      cond = list(list(quote(stop("condition evaluated")), "unused"))
    ),
    regexp = "`arg1` cannot contain `NA` or `NaN` values"
  )

  error_message <- tryCatch(
    .rba_args_na_nested(
      cons = list(
        list(arg = "arg1", class = "numeric"),
        list(arg = "arg2", class = "logical")
      ),
      arg1 = NA_real_,
      arg2 = 1
    ),
    error = conditionMessage
  )

  expect_match(object = error_message, regexp = "following `2 Errors`")
  expect_match(
    object = error_message,
    regexp = "`arg1` cannot contain `NA` or `NaN` values"
  )
  expect_match(object = error_message, regexp = "arg2 should be of class")
  expect_false(
    object = grepl("arg1 should be of class", error_message, fixed = TRUE)
  )
})

test_that("NULL remains independent from missing-value validation", {

  expect_invisible(call = .rba_args_na_optional())
  expect_error(
    object = .rba_args_na_optional(NA_real_),
    regexp = "`arg1` cannot contain `NA` or `NaN` values"
  )
})

test_that("no_na = FALSE checks every non-missing value constraint", {

  constraint_cases <- list(
    list(
      cons = list(class = "character", val = "ok"),
      accepted = c(NA_character_, "ok"),
      rejected = c(NA_character_, "bad"),
      error = "should be either"
    ),
    list(
      cons = list(class = "numeric", ran = c(1, 3)),
      accepted = c(NA_real_, 2),
      rejected = c(NA_real_, 4),
      error = "from 1 to 3"
    ),
    list(
      cons = list(class = "character", regex = "^[A-Z]+$"),
      accepted = c(NA_character_, "ABC"),
      rejected = c(NA_character_, "abc"),
      error = "valid format"
    )
  )

  for (case in constraint_cases) {
    expect_invisible(
      call = do.call(
        .rba_args_na_check,
        c(
          list(arg1 = case[["accepted"]]),
          case[["cons"]],
          list(no_na = FALSE)
        )
      )
    )
    expect_error(
      object = do.call(
        .rba_args_na_check,
        c(
          list(arg1 = case[["rejected"]]),
          case[["cons"]],
          list(no_na = FALSE)
        )
      ),
      regexp = case[["error"]]
    )
  }
})

test_that("no_na = FALSE preserves structured classes", {

  expect_invisible(
    call = .rba_args_na_check(
      arg1 = data.frame(value = c(1, NA_real_)),
      class = "data.frame",
      no_na = FALSE
    )
  )
  expect_invisible(
    call = .rba_args_na_check(
      arg1 = matrix(c(1, NA_real_), nrow = 1),
      class = "matrix",
      no_na = FALSE
    )
  )
})

test_that("no_na = FALSE preserves original length semantics", {

  expect_invisible(
    call = .rba_args_na_check(
      arg1 = c(NA_real_, 2),
      len = 2L,
      no_na = FALSE
    )
  )
  expect_error(
    object = .rba_args_na_check(
      arg1 = c(NA_real_, 2),
      len = 1L,
      no_na = FALSE
    ),
    regexp = "length `1`"
  )
})

test_that("save_to uses one shared missing-value sentinel contract", {

  save_to_cons <- list(
    arg = "save_to",
    class = "character",
    len = 1L,
    no_na = FALSE
  )

  expect_invisible(
    call = .rba_args_na_nested(
      cons = list(save_to_cons),
      save_to = NA
    )
  )
  expect_error(
    object = .rba_args_na_nested(
      cons = list(save_to_cons),
      save_to = c("first.txt", "second.txt")
    ),
    regexp = "length `1`"
  )
})

test_that("rbioapi options use the default missing-value contract", {

  missing_options <- list(
    timeout = NA_real_,
    dir_name = NA_character_,
    diagnostics = NA
  )

  for (option in names(missing_options)) {
    option_args <- missing_options[option]
    expect_error(
      object = do.call(.rba_args_na_nested, option_args),
      regexp = sprintf(
        "`%s` cannot contain `NA` or `NaN` values",
        option
      )
    )
  }

  expect_error(
    object = .rba_args_na_nested(timeout = c(1, 2)),
    regexp = "timeout should be of length `1`"
  )
})
