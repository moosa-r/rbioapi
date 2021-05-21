#' @section Supported Database/Services:
#' \itemize{
#'   \item Enrichr
#'   \item JASPAR
#'   \item MiEAA
#'   \item PANTHER
#'   \item Reactome
#'   \item STRING
#'   \item UniProt
#'   }
#'
#' @docType package
#' @name rbioapi
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  options(rba_timeout = 90,
          rba_dir_name = "rbioapi",
          rba_diagnostics = FALSE,
          rba_retry_max = 1,
          rba_progress = FALSE,
          rba_save_file = FALSE,
          rba_skip_error = !interactive(),
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
