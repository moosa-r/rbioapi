#' rbioapi: User-friendly interface to Biological Databases' APIs
#'
#' This package provides and easy, user-friendly and unified access to Biological databases' API.
#' STRING, Reactome, Uniprot, Pubmed
#'
#' @section STRING functions:
#' map, interactions, image
#'
#' @docType package
#' @name rbioapi
NULL

.onAttach = function(libname, pkgname) {
  packageStartupMessage("Welcome to rbioAPI.\r\n",
                        "This package provides an easy, user-friendly and ",
                        "unified access to Biological databases' API from R.\r\n",
                        "Due to nature of this package, some function could ",
                        "break after the databases changes in their API. ",
                        "if so, kindly report the broken function[s] to:\r\n",
                        "https://github.com/moosa-r/rbioapi ",
                        "or moosa.rezwani@gmail.com\r\n",
                        "Last API verbs check for currently supported Databases:\r\n",
                        "STRING: Aug 04 2020\r\n",
                        "Enrichr: Aug 04 2020"
  )
  ## save each database "base URL" as an option.
  options("rba_databases" = c("STRING",
                              "Enrichr"))
  options("rba_url_string" = c("STRING" = "https://string-db.org"))
  options("rba_url_enrichr" = c("Enrichr" = "https://amp.pharm.mssm.edu"))
  ## save user agent for api Calls as an option
  options("rba_ua" = "rbioapi R package")

}



# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.devtools <- list(
#     devtools.path = "~/R-dev",
#     devtools.install.args = "",
#     devtools.name = "Your name goes here",
#     devtools.desc.author = "First Last <first.last@example.com> [aut, cre]",
#     devtools.desc.license = "What license is it under?",
#     devtools.desc.suggests = NULL,
#     devtools.desc = list()
#   )
#   toset <- !(names(op.devtools) %in% names(op))
#   if(any(toset)) options(op.devtools[toset])
#
#   invisible()
# }

# .onUnload()
