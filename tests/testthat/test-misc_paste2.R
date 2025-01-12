test_that(".paste2 works", {

  expect_identical(
    object = .paste2(c("first", "second", "third")),
    expected = "first, second and third"
  )

  expect_identical(
    object = .paste2(c("first", "second", "third"), last = " or "),
    expected = "first, second or third"
  )
  expect_identical(
    object = .paste2(c("first", "second", "third"), sep = " + "),
    expected = "first + second and third"
  )
  expect_identical(
    object = .paste2(c("first", "second", "third"), quote = "`"),
    expected = "`first`, `second` and `third`"
  )
  expect_identical(
    object = .paste2(c("first", "second", "third"), quote_all = "`"),
    expected = "`first, second and third`"
  )

  expect_identical(
    object = .paste2(
      c("first", "second", "third"),
      last = " or ",
      sep = " + ",
      quote = "-",
      quote_all = "_"
    ),
    expected = "_-first- + -second- or -third-_"
  )

})
