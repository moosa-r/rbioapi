#' @title rbioapi: User-Friendly R Interface to Biologic Web Services' API
#'
#' @description Currently fully supports miEAA, PANTHER, Reactome, STRING,
#'   and UniProt! The goal of rbioapi is to provide a user-friendly and
#'   consistent interface to biological databases and services; It is
#'   designed in a way that insulates the user from technicalities when
#'   it comes to using API services and creates a unified and
#'   easy-to-implement tool to connect to biological databases and services.
#'   With rbioapi, You are not required to have any prior technical
#'   knowledge. Just fill in a function's arguments and the rest is handled
#'   for you. This an ongoing project. New databases and services will be
#'   implemented periodically to gradually make rbioapi more comprehensive.
#'   Feel free to suggest any databases or services you often use.
#'
#' @section Currently, rbioapi fully supports and Covers API resources of:
#' \itemize{
#'   \item MiEAA
#'   \item PANTHER
#'   \item Reactome
#'   \item STRING
#'   \item UniProt}
#'
#' @docType package
#' @name rbioapi
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  options(rba_timeout = 30,
          rba_dir_name = "rbioapi",
          rba_diagnostics = FALSE,
          rba_retry_max = 1,
          rba_progress = FALSE,
          rba_save_file = FALSE,
          rba_skip_error = FALSE,
          rba_user_agent = "rbioapi R package",
          rba_verbose = TRUE,
          rba_retry_wait = 10,
          rba_user_options = c(rba_timeout = "timeout",
                               rba_dir_name = "dir_name",
                               rba_diagnostics = "diagnostics",
                               rba_retry_max = "retry_max",
                               rba_progress = "progress",
                               rba_save_file = "save_file",
                               rba_skip_error = "skip_error",
                               rba_verbose = "verbose",
                               rba_retry_wait = "retry_wait")
  )
  invisible()
}
