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
  packageStartupMessage("Welcome to rbioAPI.\r\n....Fill this message later....")
  ##### provide the dates which you last updated and checked for a database API. ###
  ## save each database """"base url""" as an option.
  options("url_string" = "https://string-db.org")
  options("url_enrichr" = "https://amp.pharm.mssm.edu")
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
