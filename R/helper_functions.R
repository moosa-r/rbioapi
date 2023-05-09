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
  request <- quote(httr::HEAD(url = url,
                              httr::timeout(getOption("rba_timeout")),
                              httr::user_agent(getOption("rba_user_agent")),
                              if (diagnostics) httr::verbose()
  ))
  test_result <- try(httr::status_code(eval(request)),
                     silent = !diagnostics)

  if (is.numeric(test_result)) {
    if (test_result == 200) {
      return(TRUE)
    } else {
      return(.rba_http_status(test_result,
                              verbose = FALSE))
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
  # Set options
  if (is.null(diagnostics) || is.na(diagnostics) || !is.logical(diagnostics)) {
    diagnostics <- getOption("rba_diagnostics")
  }
  user_agent <- getOption("rba_user_agent")
  timeout <- getOption("rba_timeout")
  skip_error <- getOption("rba_skip_error")

  cat_if <- ifelse(test = isTRUE(print_output),
                   yes = function(...) {cat(...)},
                   no = function(...) {invisible()})
  # start tests
  .msg("Checking Your connection to the Databases currently supported by rbioapi:",
       cond = "print_output")

  cat_if("--->>>", "Internet", ":\n")
  google <- try(httr::status_code(httr::HEAD("https://google.com/",
                                             if (diagnostics) httr::verbose(),
                                             httr::user_agent(user_agent),
                                             httr::timeout(timeout)))
                , silent = TRUE)

  if (google == 200) {
    cat_if("+++ Connected to the Internet.\n")
  } else {
    cat_if("!!!! No Internet Connection.\n")
    if (isTRUE(skip_error)) {
      return("Could not resolve `https://google.com`. Check Your internet Connection.")
    } else {
      stop("Could not resolve `https://google.com`. Check Your internet Connection.",
           call. = diagnostics)
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
#' @param timeout (Numeric) (default = 30) The maximum time in seconds that
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
  .rba_args(cond = list(list(quote(is.character(save_file)),
                             "As a global option, you can only set save_file to 'logical', not a file path.")))
  ## if empty function was called, show the available options
  changes <- vapply(X = ls(),
                    function(x) {
                      x <- get(x)
                      !(is.null(x) || is.na(x))},
                    logical(1))
  if (!any(changes)) {
    options_df <- data.frame(rbioapi_option = getOption("rba_user_options"),
                             current_value = vapply(names(getOption("rba_user_options")),
                                                    function(x) {as.character(getOption(x))},
                                                    character(1)),
                             allowed_value = getOption("rba_user_options_allowed"),
                             stringsAsFactors = FALSE,
                             row.names = NULL)
    return(options_df)
  } else {
    ## change the supplied options
    for (chng in names(changes[changes])) {
      chng_content <- get(chng)
      eval(parse(text = sprintf(ifelse(is.character(chng_content),
                                       yes = "options(%s = \"%s\")",
                                       no = "options(%s = %s)"),
                                paste0("rba_", chng),
                                chng_content)))
    }
    invisible()
  }
}

#' Iterate over function calls
#'
#' This function accepts a list where each of its elements is a character
#'   vector which can be parsed and evaluated. currently, this is only used
#'   in rba_pages
#'
#' @param input_call a list that contains the calls.
#' @param pb_switch Display a progress bar?
#'
#' @return The evaluation results of each input call.
#' @noRd
.rba_pages_do <- function(input_call, pb_switch, sleep_time = 1) {
  if (pb_switch) {
    ## initiate progress bar
    pb <- utils::txtProgressBar(min = 0,
                                max = length(input_call),
                                style = 3)
    pb_val <- 0
  }
  #do the calls
  output <- lapply(X = input_call,
                   FUN = function(x){
                     Sys.sleep(sleep_time)
                     y <- eval(parse(text = x))
                     if (pb_switch) {
                       # advance the progress bar
                       pb_now <- get("pb_val", envir = parent.frame(2))
                       assign("pb_val", pb_now + 1, envir = parent.frame(2))
                       utils::setTxtProgressBar(pb, pb_now + 1)
                     }
                     return(y)
                   })
  if (pb_switch) {close(pb)}
  return(output)
}

#' Get Multiple Pages of a Paginated Resource
#'
#' Some resources return paginated results, meaning that you have to make
#'   separate calls for each page. Using this function, you can iterate over
#'   up to 100 pages. Just supply your rbioapi function and change to page
#'   argument to "pages:start_page:end_page", for example "pages:1:5".
#'
#'   To prevent flooding the server, there will be a 1 second delay between
#'   calls, also the user cannot iterate on more than 100 pages. The function
#'   will also override skip_error option and will always set it to TRUE.
#'   This means that in case of server response error (e.g. requesting pages
#'   that do not exist) an error message be returned to you instead of
#'   halting function's execution.
#'
#' @param input_call A quoted call. supply a regular rbioapi function call,
#'   but with two differences:\enumerate{
#'   \item: Wrap a quote() around it. meaning: quote(rba_example())
#'   \item: Set the argument that corresponds to the page number to
#'   "pages:start_page:end_page", for example "pages:1:5".}
#'   See the "examples" section to learn more.
#' @param ... Experimental internal options.
#'
#' @return A named list where each element corresponds to a request's page.
#'
#' @examples
#' \donttest{
#' rba_pages(input_call = quote(rba_uniprot_taxonomy(ids = 189831,
#'     hierarchy = "siblings",
#'     page_size = 50,
#'     page_number = "pages:1:5")))
#' }
#' \donttest{
#' rba_pages(input_call = quote(rba_uniprot_taxonomy_name(name = "adenovirus",
#'     field = "scientific",
#'     search_type = "contain",
#'     page_size = 200,
#'     page_number = "pages:1:5",
#'     verbose = FALSE)))
#' }
#' \donttest{
#' rba_pages(input_call = quote(rba_panther_info(what = "families",
#'     families_page = "pages:9:11")))
#' }
#'
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_pages <- function(input_call, ...){
  ## Internal options
  ext_args <- list(...)
  internal_opts <- list(verbose = TRUE,
                        sleep_time = 1,
                        page_check = TRUE,
                        add_skip_error = TRUE,
                        list_names = NA,
                        force_pb = NA)
  if (length(ext_args) > 0) {
    internal_opts[names(ext_args)] <- ext_args
  }
  verbose <- internal_opts$verbose

  ## Convert the input_call to character
  if (!inherits(input_call, "call")) {
    stop("The call should be wrapped in qoute()",
         call. = getOption("rba_diagnostics"))
  }

  input_call <- gsub(pattern = "\\s+",
                     replacement = " ",
                     x = paste0(deparse(input_call), collapse = ""))
  if (!grepl("^rba_.+\\(", input_call)) {
    stop("You should supply a rbioapi function.",
         call. = getOption("rba_diagnostics"))
  }

  ## Extract start and end pages
  start_page <- unlist(regmatches(input_call,
                                  gregexpr("(?<=\"pages:)\\d+(?=:\\d+\")",
                                           input_call, perl = TRUE)))
  end_page <- unlist(regmatches(input_call,
                                gregexpr("(?<=\\d:)\\d+(?=\")",
                                         input_call, perl = TRUE)))
  start_page <- as.integer(start_page)
  end_page <- as.integer(end_page)
  ## Check pages
  if (length(start_page) != 1 | length(end_page) != 1) {
    stop("The variable you want to paginate should be formatted as:",
         "`pages:start:end`.\nfor example: \"pages:1:5\".",
         call. = getOption("rba_diagnostics"))
  }

  if (isTRUE(internal_opts$page_check) && (end_page - start_page > 100)) {
    stop("The maximum pages you are allowed to iterate are 100 pages.",
         call. = getOption("rba_diagnostics"))
  }

  ## Only show progress bar if verbose, diagnostics and progress bar are off
  if (is.na(internal_opts$force_pb)) {
    verbose_on <-
      !grepl(",\\s*verbose\\s*=\\s*FALSE", input_call) &&
      (grepl(",\\s*verbose\\s*=\\s*TRUE", input_call) ||
         isTRUE(getOption("rba_verbose")))
    diagnostics_on <-
      !grepl(",\\s*diagnostics\\s*=\\s*FALSE", input_call) &&
      (grepl(",\\s*diagnostics\\s*=\\s*TRUE", input_call) ||
         isTRUE(getOption("rba_diagnostics")))
    progress_on <-
      !grepl(",\\s*progress\\s*=\\s*FALSE", input_call) &&
      (grepl(",\\s*progress\\s*=\\s*TRUE", input_call) ||
         isTRUE(getOption("rba_progress")))
    pb_switch <- sum(c(verbose_on, diagnostics_on, progress_on)) == 0
  } else {
    pb_switch <- isTRUE(internal_opts$force_pb)
  }

  ## Build the calls
  elements_seq <- seq.int(from = start_page, to = end_page,
                          by = ifelse(test = start_page > end_page,
                                      yes = -1L,
                                      no = 1L))
  # Add skip_error = TRUE and page numbers to the calls
  input_call <- gsub(",\\s*skip_error\\s*=\\s*(TRUE|FALSE)", "",
                     input_call, perl = TRUE)
  input_call <- sub(pattern = "\"pages:\\d+:\\d+\"",
                    replacement = ifelse(test = isFALSE(internal_opts$add_skip_error),
                                         yes = "%s",
                                         no = "%s, skip_error = TRUE"),
                    x = input_call,
                    perl = TRUE)

  input_call <- as.list(sprintf(input_call, elements_seq))

  # Name the list
  if (length(internal_opts$list_names) != length(input_call)) {
    names(input_call) <- paste0("page_", elements_seq)
  } else {
    names(input_call) <- internal_opts$list_names
  }

  ## Do the calls
  .msg("Iterating from page %s to page %s.", start_page, end_page)
  final_output <- .rba_pages_do(input_call,
                                pb_switch = pb_switch,
                                sleep_time = internal_opts$sleep_time)
  return(final_output)
}

