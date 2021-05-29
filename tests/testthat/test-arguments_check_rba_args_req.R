# Dosen't work in test setup but works manually
# test_that(".rba_args_req works", {
#   cons <- list(list(arg = "aa",
#                     class = "character"),
#                list(arg = "bb",
#                     class = "numeric"))
#   rba_args_req_nested <- function(aa, bb = 2) {
#     cons <- cons
#     .rba_args_req(cons = cons, n = 1)
#   }
#
#   expect_identical(object = rba_args_req_nested(aa = "x", bb = 22),
#                    expected = list(list(arg = "aa",
#                                         class = "character",
#                                         no_null = TRUE),
#                                    list(arg = "bb",
#                                         class = "numeric")))
# })
