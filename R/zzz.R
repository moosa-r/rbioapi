#' @title rbioapi: User-friendly R Interface to Biologic Web-Services' API
#'
#' @description The goal of rbioapi is to provide a user-friendly and unified
#'   interface to biological databases, to insulate the user from
#'   technicalities when it comes to using API services and to create a
#'   unified, easy-to-learn, and easy-to-implement way to connect to biologic
#'   databases.\cr
#'   With rbioapi, You are not required to have any technical knowledge. Just
#'   fill a function's arguments and the rest is handled for you.\cr
#'   This an ongoing project. New databases and services will be implemented
#'   periodically. Feel free to suggest any databases or services you often use.
#'
#' @section Currently rbioapi fully supports:
#' \enumerate{\item Enrichr \item MiEAA \item PANTHER \item Reactome
#'   \item STRING \item UniProt}
#'
#' @docType package
#' @name rbioapi
#' @keywords internal
"_PACKAGE"

.onLoad = function(libname, pkgname){
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
                               rba_retry_wait = "retry_wait"))
  invisible()
}
