#' @section Supported Database/Services:
#' \itemize{
#'   \item Enrichr
#'   \item JASPAR
#'   \item miEAA
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

#' Default expressions for rbioapi options
#'
#' Provides the expressions used by \code{.onLoad()} to initialize package
#' options and by the \code{rba_options()} documentation to report their
#' default values.
#'
#' @keywords internal
#' @noRd
.rba_option_defaults <- alist(
  rba_diagnostics = FALSE,
  rba_dir_name = "rbioapi",
  rba_retry_max = 0,
  rba_retry_wait = 10,
  rba_progress = FALSE,
  rba_save_file = FALSE,
  rba_skip_error = !interactive(),
  rba_timeout = 90,
  rba_verbose = TRUE,
  rba_metadata = FALSE,
  rba_user_agent =
    "rbioapi_R_package_<https://cran.r-project.org/package=rbioapi>"
)

#' Accepted-value descriptions for rbioapi options
#'
#' Provides the descriptions displayed in the \code{allowed_value} column
#' returned by \code{rba_options()}.
#'
#' @keywords internal
#' @noRd
.rba_user_options_allowed <- c(
  rba_diagnostics = "Logical (TRUE/FALSE)",
  rba_dir_name = "Character",
  rba_retry_max = "Numeric (finite non-negative whole number)",
  rba_retry_wait = "Numeric (finite and non-negative)",
  rba_progress = "Logical (TRUE/FALSE)",
  rba_save_file = "Logical (TRUE/FALSE)",
  rba_skip_error = "Logical (TRUE/FALSE)",
  rba_timeout = "Numeric (0.001 to 3600, inclusive)",
  rba_verbose = "Logical (TRUE/FALSE)",
  rba_metadata = "Logical (TRUE/FALSE)"
)

.onLoad <- function(libname, pkgname) {
  options(
    c(
      # Evaluate the documented defaults when the package namespace loads.
      lapply(
        X = .rba_option_defaults,
        FUN = eval,
        envir = baseenv()
      ),
      # Derive the argument names used by rba_options() and .rba_ext_args().
      list(
        rba_user_options = stats::setNames(
          object = sub(
            pattern = "^rba_",
            replacement = "",
            x = names(x = .rba_user_options_allowed)
          ),
          nm = names(x = .rba_user_options_allowed)
        )
      )
    )
  )

  invisible()
}
