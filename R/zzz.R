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
          rba_retry_max = 0,
          rba_progress = FALSE,
          rba_save_file = FALSE,
          rba_skip_error = !interactive(),
          rba_user_agent = "rbioapi_R_package_<https://cran.r-project.org/package=rbioapi>",
          rba_verbose = TRUE,
          rba_retry_wait = 10,
          rba_user_options = c(rba_diagnostics = "diagnostics",
                               rba_dir_name = "dir_name",
                               rba_progress = "progress",
                               rba_retry_max = "retry_max",
                               rba_retry_wait = "retry_wait",
                               rba_save_file = "save_file",
                               rba_skip_error = "skip_error",
                               rba_timeout = "timeout",
                               rba_verbose = "verbose"),
          rba_user_options_allowed = c(rba_diagnostics = "Logical (TRUE/FALSE)",
                                       rba_dir_name = "Character",
                                       rba_progress = "Logical (TRUE/FALSE)",
                                       rba_retry_max = "Numeric (0 or greater)",
                                       rba_retry_wait = "Numeric (0 or greater)",
                                       rba_save_file = "Logical (TRUE/FALSE)",
                                       rba_skip_error = "Logical (TRUE/FALSE)",
                                       rba_timeout = "Numeric (0.1 or greater)",
                                       rba_verbose = "Logical (TRUE/FALSE)")
  )
  invisible()
}
