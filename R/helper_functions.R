#' Test connection with a rest server
#'
#' Internal helper function for rba_connection_test(). It will  make HTTP HEAD
#'   request to the given resource.
#'
#' @param url A URL to to resource being examined.
#' @param diagnostics logical: Generate diagnostics and detailed messages with
#'   internal information.
#'
#' @return An informative message with the result of HEAD request's success or
#'   failure.
#' @family internal_internet_connectivity
#' @noRd
.rba_api_check <- function(url, diagnostics = FALSE){
  request <- quote(
    httr::HEAD(
      url = url,
      httr::timeout(getOption("rba_timeout")),
      httr::user_agent(getOption("rba_user_agent")),
      if (diagnostics) httr::verbose()
    )
  )
  test_result <- try(httr::status_code(eval(request)), silent = !diagnostics)

  if (is.numeric(test_result)) {
    if (test_result == 200) {
      return(TRUE)
    } else {
      return(
        .rba_http_status(
          http_status = test_result,
          as_sentence = FALSE
        )
      )
    }
  } else {
    return(test_result)
  }
}

#' Test if the Supported Services Are Responding
#'
#' Run this function to test the internet connectivity of your device and the
#'   current status of the supported Services.
#'
#' This function attempts to send a simple query to the supported services.
#'   If the service successfully responded, you will be informed with a success
#'   message; If not, the content of the error will be reported to you.
#'   \cr Please run this function if you encounter any errors while using
#'   rbioapi. Also, if you need to contact support, kindly call this function
#'   with 'diagnostic = TRUE' and include the output messages in your support
#'   request.
#'
#' @param print_output (Logical) (default = TRUE) Send the tests' output
#'   to the console?
#' @param diagnostics (Logical) (default = FALSE) Show diagnostics and
#'   detailed messages with internal information.
#'
#' @return Connection test for the supported servers will be displayed
#'   in console and the results will be invisibly returned as a list.
#'
#' @examples
#' \donttest{
#' rba_connection_test()
#' }
#'
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_connection_test <- function(print_output = TRUE, diagnostics = FALSE) {
  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "print_output", class = "logical", len = 1L)
    )
  )

  # Set options
  if (is.null(diagnostics)) {
    diagnostics <- getOption("rba_diagnostics")
  }
  user_agent <- getOption("rba_user_agent")
  timeout <- getOption("rba_timeout")
  skip_error <- getOption("rba_skip_error")

  cat_if <- ifelse(
    test = isTRUE(print_output),
    yes = function(...) { cat(...) },
    no = function(...) { invisible() }
  )

  # start tests
  .msg(
    "Checking Your connection to the Databases currently supported by rbioapi:",
    cond = "print_output"
  )

  cat_if("--->>>", "Internet", ":\n")
  google <- try(
    httr::status_code(
      httr::HEAD(
        "https://google.com/",
        if (diagnostics) httr::verbose(),
        httr::user_agent(user_agent),
        httr::timeout(timeout)
      )
    ),
    silent = TRUE
  )

  if (google == 200) {
    cat_if("+++ Connected to the Internet.\n")
  } else {
    cat_if("!!!! No Internet Connection.\n")
    if (isTRUE(skip_error)) {
      return("Could not resolve `https://google.com`. Check Your internet Connection.")
    } else {
      stop(
        "Could not resolve `https://google.com`. Check Your internet Connection.",
        call. = diagnostics
      )
    }
  }

  tests <- .rba_stg("tests")
  output <- list()

  for (i in seq_along(tests)) {
    cat_if("--->>>", names(tests)[[i]], ":\n")
    output[[names(tests)[[i]]]] <- .rba_api_check(tests[[i]],
                                                  diagnostics = diagnostics)
    if (isTRUE(output[[names(tests)[[i]]]])) {
      cat_if("+++ The server is responding.\n")
    } else {
      cat_if("!!! failed with error:\n", output[[names(tests)[[i]]]])
    }
  }
  invisible(output)
}

#' Set rbioapi Global Options
#'
#' A safe way to change rbioapi's global options and behavior. see "arguments"
#'   section for available options.
#'   \cr Note that you are not limited to changing the options globally, you can
#'   include the option names and values in the '...' argument of any rbioapi
#'   function to alter the option(s) only in that function call;
#'   e.g. example_function(x, diagnostics = TRUE, timeout = 300).
#'   \cr Alternatively, you can call this function with no arguments, i.e.
#'   rba_options(), to retrieve a data frame of available rbioapi options and
#'   their current values.
#'
#'   Because this function validates your supplied changes, please
#'   \strong{\emph{only change rbioapi options using this function}} and avoid
#'   directly editing them.
#'
#' @param diagnostics (Logical) (default = FALSE) Show diagnostics and
#'   detailed messages with internal information.
#' @param dir_name (character) (default = "rbioapi") If the package needs to
#'   generate a file path to save the server's response, a directory with this
#'   name will be created in your working directory to save your files.
#' @param retry_max (Numeric) (default = 0) How many times should rbioapi
#'   retry in case of 5xx server responses, errors related to the server
#'   or no internet connectivity?
#' @param retry_wait (Numeric) (default = 10) Time in seconds to wait before
#'   next retry in case of internet connection or server problems.
#' @param progress (Logical) (default = FALSE) Should a progress bar be
#'   displayed?
#' @param save_file (Logical or character) (default = FALSE) Either:\itemize{
#'   \item TRUE: In this case, the raw server's response file will be
#'   automatically saved to a proper file path. use "dir_name" argument to
#'   change the file's parent directory.
#'   \item FALSE: (default) Do not automatically save server's response file.
#'   \item Character: (Only when changing the option via "..." in
#'   a functions call) A valid file path to save the server's response
#'   file to the function that you are calling.}
#' @param skip_error (Logical) (default = FALSE if R is in the interactive mode,
#'   TRUE otherwise) If TRUE, the code execution  will not be stopped in case
#'   of errors (anything but HTTP status 200 from the server); Instead the
#'   error message will be returned as the function's output. However, if FALSE,
#'   in case of any error, the code execution will be halted and an error
#'   message will be issued.
#' @param timeout (Numeric) (default = 90) The maximum time in seconds that
#'   you are willing to wait for a server response before giving up and
#'   stopping the function execution.
#' @param verbose (Logical) (Default = TRUE) Generate short informative
#'   messages.
#'
#' @return If called without any argument, a Data frame with available options
#'   and their information; If Called with an argument, will Return
#'   NULL but Alters that option globally.
#'
#' @examples
#' rba_options()
#' \dontrun{
#' rba_options(verbose = FALSE)
#' }
#' \dontrun{
#' rba_options(save_file = TRUE)
#' }
#' \dontrun{
#' rba_options(diagnostics = TRUE, progress = TRUE)
#' }
#'
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_options <- function(diagnostics = NULL,
                        dir_name = NULL,
                        retry_max = NULL,
                        retry_wait = NULL,
                        progress = NULL,
                        save_file = NULL,
                        skip_error = NULL,
                        timeout = NULL,
                        verbose = NULL) {

  .rba_args(
    cond = list(
      list(
        quote(is.character(save_file)),
        "As a global option, you can only set save_file to 'logical', not a file path."
      )
    )
  )

  ## if empty function was called, show the available options
  changes <- vapply(
    X = ls(),
    function(x) {
      x <- get(x)
      !(is.null(x) || is.na(x))
    },
    logical(1)
  )

  if (!any(changes)) {
    options_df <- data.frame(
      rbioapi_option = getOption("rba_user_options"),
      current_value = vapply(
        names(getOption("rba_user_options")),
        function(x) { as.character(getOption(x)) },
        character(1)
      ),
      allowed_value = getOption("rba_user_options_allowed"),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    return(options_df)
  } else {
    ## change the supplied options
    for (chng in names(changes[changes])) {
      chng_content <- get(chng)
      eval(
        parse(
          text = sprintf(
            ifelse(
              is.character(chng_content),
              yes = "options(%s = \"%s\")",
              no = "options(%s = %s)"
            ),
            paste0("rba_", chng),
            chng_content
          )
        )
      )
    }
    invisible()
  }
}

#' Iterate Over Paginated Calls
#'
#' @param input_call Call: Normalized call to evaluate.
#' @param page_arg Character: Name of the page argument.
#' @param pages Numeric vector: Page numbers to request.
#' @param eval_env Environment: Environment in which to evaluate the calls.
#' @param sleep_time Numeric: Seconds to wait between calls.
#' @param progress Logical: Display a progress bar?
#'
#' @return A named list containing one result per call.
#' @noRd
.rba_pages_do <- function(input_call,
                          page_arg,
                          pages,
                          eval_env,
                          sleep_time,
                          progress) {
  ## Initialize one progress bar for the complete operation
  if (isTRUE(progress)) {
    pb <- utils::txtProgressBar(
      min = 0,
      max = length(pages),
      style = 3
    )
    on.exit(close(pb), add = TRUE)
  }

  ## Preallocate named output
  output <- stats::setNames(
    vector(mode = "list", length = length(pages)),
    paste0(
      "page_",
      format(pages, scientific = FALSE, trim = TRUE)
    )
  )

  ## Evaluate one page at a time
  for (i in seq_along(pages)) {
    if (i > 1L) {
      Sys.sleep(sleep_time)
    }

    input_call[[page_arg]] <- pages[[i]]
    output[i] <- list(eval(input_call, envir = eval_env))

    if (isTRUE(progress)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  return(output)
}

#' Retrieve Multiple Pages of a Paginated Resource
#'
#' Evaluate a quoted call to an exported rbioapi function for multiple page
#'   numbers. Calls are made sequentially, and their results are returned in
#'   the requested order.
#'
#' Pagination can be specified in either of two ways. To request an inclusive
#'   range, set the named page argument in \code{input_call} to a character
#'   string of the form \code{"pages:start:end"}. Alternatively, omit the page
#'   argument from \code{input_call}, supply its exact name through
#'   \code{page_arg}, and supply the desired page numbers through \code{pages}.
#'   The range may run in either direction, and the two forms cannot be
#'   combined. The page argument must exactly match a formal function argument;
#'   partial and positional matching are not used for pagination.
#'
#' Page numbers must be unique positive whole numbers, and no more than 100
#'   pages can be requested in one call. \code{sleep_time} seconds are inserted
#'   between successive calls.
#'
#' The value of \code{skip_error} is passed to every page call, allowing
#'   rbioapi's standard error-handling mechanism to determine whether a failed
#'   request stops the operation. If \code{progress = TRUE}, one progress bar
#'   is displayed and
#'   \code{verbose = FALSE} and \code{progress = FALSE} are passed to the
#'   individual rbioapi calls.
#'
#' @param input_call Call: A quoted call to an exported API-endpoint-facing
#'   rbioapifunction. To request an inclusive range, set the called function's
#'   named page argument to a character string of the form
#'   \code{"pages:start:end"}. Alternatively, omit the page argument from
#'   \code{input_call}, supply its exact name through \code{page_arg}, and
#'   supply the desired page numbers through \code{pages}. These two forms
#'   cannot be combined.
#' @param page_arg Character: (optional) Exact name of the formal argument of
#'   the called rbioapi function that accepts the page number. Supply together
#'   with \code{pages} when the page argument is omitted from
#'   \code{input_call}.
#' @param pages Numeric vector: (optional) Unique positive whole page numbers
#'   in the order in which they should be requested. A maximum of 100 values
#'   can be supplied. Must be supplied together with \code{page_arg}.
#' @param sleep_time Numeric: (default = \code{2}) Number of seconds to wait
#'   between successive calls. Must be at least 2.
#' @param skip_error Logical: (default = \code{TRUE}) Continue the operation
#'   after an unsuccessful page call and return its error message as that
#'   page's result? This value is passed to every page call. If
#'   \code{input_call} already supplies \code{skip_error}, its value is
#'   overridden and a warning is issued.
#' @param progress Logical: (default = \code{FALSE}) Display one progress bar
#'   for the complete operation? When \code{TRUE}, verbose messages and
#'   progress bars from individual page calls are suppressed.
#' @param verbose Logical: (default = current \code{rba_verbose} option)
#'   Generate an informative message describing the complete operation?
#'
#' @return A named list containing one element per requested page. Element
#'   names have the form \code{page_<number>}.
#'
#' @examples
#' \donttest{
#' rba_pages(
#'   input_call = quote(
#'     rba_uniprot_taxonomy_name(
#'       name = "adenovirus",
#'       search_type = "contain",
#'       page_size = 20,
#'       page_number = "pages:1:3"
#'     )
#'   )
#' )
#' }
#' \donttest{
#' rba_pages(
#'   input_call = quote(
#'     rba_uniprot_taxonomy_name(
#'       name = "adenovirus",
#'       search_type = "contain",
#'       page_size = 20
#'     )
#'   ),
#'   page_arg = "page_number",
#'   pages = c(1, 3, 5)
#' )
#' }
#'
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_pages <- function(input_call,
                      page_arg = NULL,
                      pages = NULL,
                      sleep_time = 2,
                      skip_error = TRUE,
                      progress = FALSE,
                      verbose = getOption("rba_verbose")) {
  ## 1. Validate the main function arguments
  .rba_args(
    cons = list(
      list(arg = "page_arg", class = "character", len = 1L),
      list(
        arg = "pages", class = c("numeric", "integer"), min_len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "sleep_time", class = c("numeric", "integer"), len = 1L,
        min_val = 2, max_val = .Machine$double.xmax, no_null = TRUE
      )
    ),
    cond = list(
      list(
        quote(missing(input_call) || !is.call(input_call)),
        "`input_call` should be a quoted function call."
      ),
      list(
        quote(xor(is.null(page_arg), is.null(pages))),
        "`page_arg` and `pages` should be supplied together."
      ),
      list(
        quote(!is.null(pages) && anyDuplicated(pages) > 0L),
        "`pages` should contain unique values."
      )
    )
  )
  .rba_args(
    cons = list(
      list(
        arg = "skip_error", class = "logical", len = 1L, no_null = TRUE
      ),
      list(
        arg = "progress", class = "logical", len = 1L, no_null = TRUE
      ),
      list(
        arg = "verbose", class = "logical", len = 1L, no_null = TRUE
      )
    )
  )

  ## 2. Extract the function name from the quoted input call
  call_head <- input_call[[1L]]
  if (is.symbol(call_head) || is.call(call_head)) {
    head_parts <- as.character(call_head)
  } else {
    head_parts <- character()
  }
  function_name <- ifelse(
    is.symbol(call_head) ||
      (
        is.call(call_head) &&
          length(head_parts) == 3L &&
          identical(head_parts[1:2], c("::", "rbioapi"))
      ),
    utils::tail(head_parts, 1L),
    ""
  )

  ## 3. Stop early if target is not an exported rbioapi function
  .rba_args(
    cond = list(
      list(
        quote(
          !(function_name %in% setdiff(
            getNamespaceExports("rbioapi"),
            c("rba_connection_test", "rba_options", "rba_pages")
          ))
        ),
        "`input_call` should call an exported API-endpoint-facing rbioapi function."
      )
    )
  )

  ## 4. Extract the target function arguments
  input_call[[1L]] <- call(
    "::",
    as.name("rbioapi"),
    as.name(function_name)
  )
  call_args <- as.list(input_call)[-1L]
  range_args <- which(vapply(
    X = call_args,
    FUN = function(x) {
      is.character(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        startsWith(x, "pages:")
    },
    FUN.VALUE = logical(1)
  ))

  ## 5. Resolve and validate the pagination arguments
  range_mode <- is.null(page_arg)
  page_spec_valid <- ifelse(
    range_mode,
    length(range_args) == 1L && grepl(
      "^pages:[1-9][0-9]*:[1-9][0-9]*$",
      call_args[[range_args]]
    ),
    length(range_args) == 0L
  )

  page_limits <- numeric()
  page_count <- length(pages)
  if (range_mode && page_spec_valid) {
    page_arg <- names(call_args)[range_args]
    page_limits <- suppressWarnings(
      as.numeric(
        strsplit(call_args[[range_args]], ":", fixed = TRUE)[[1L]][-1L]
      )
    )
    page_count <- abs(diff(page_limits)) + 1
  }

  page_limits_finite <- all(is.finite(page_limits))
  page_arg_valid <- isTRUE(page_arg %in% setdiff(
    names(formals(getExportedValue("rbioapi", function_name))),
    "..."
  ))
  request_valid <-
    page_spec_valid &&
    page_arg_valid &&
    page_limits_finite &&
    isTRUE(page_count <= 100L)

  .rba_args(
    cond = list(
      list(
        quote(!page_spec_valid),
        ifelse(
          range_mode,
          "Supply one valid `\"pages:start:end\"` page-range specification, or use `page_arg` with `pages`.",
          "`page_arg` and `pages` cannot be combined with a `\"pages:start:end\"` page range."
        )
      ),
      list(
        quote(
          range_mode &&
            page_spec_valid &&
            !page_limits_finite
        ),
        "Invalid page range. Page numbers should be finite."
      ),
      list(
        quote(page_spec_valid && !page_arg_valid),
        sprintf(
          "`page_arg` should exactly match a formal argument of `%s`.",
          function_name
        )
      ),
      list(
        quote(
          page_spec_valid &&
            page_limits_finite &&
            isTRUE(page_count > 100L)
        ),
        "No more than 100 pages can be requested in one call."
      ),
      list(
        quote(
          request_valid &&
            utils::hasName(call_args, "skip_error")
        ),
        "`skip_error` in `input_call` was overridden by the `skip_error` argument of `rba_pages()`.",
        warn = TRUE
      ),
      list(
        quote(
          request_valid &&
            isTRUE(progress) &&
            any(c("verbose", "progress") %in% names(call_args))
        ),
        "Any `verbose` or `progress` setting in `input_call` was overridden because `progress = TRUE` in `rba_pages()`.",
        warn = TRUE
      )
    )
  )

  if (range_mode && request_valid) {
    pages <- seq.int(
      from = page_limits[[1L]],
      to = page_limits[[2L]]
    )
  }

  ## 6. Apply shared call options and run the internal page loop
  input_call[["skip_error"]] <- skip_error
  if (isTRUE(progress)) {
    input_call[["verbose"]] <- FALSE
    input_call[["progress"]] <- FALSE
  }

  .msg(
    "Retrieving %s page(s) using `%s()`.",
    page_count,
    function_name
  )

  output <- .rba_pages_do(
    input_call = input_call,
    page_arg = page_arg,
    pages = pages,
    eval_env = parent.frame(),
    sleep_time = sleep_time,
    progress = progress
  )
  return(output)
}
