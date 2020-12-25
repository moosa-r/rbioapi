#' Test if the Supported Services Are Responding
#'
#' Run this function to test the internet connectivity of your device and the
#'   current status of the supported Services.
#'
#' This function attempts to send a simple query to the supported services.
#'   If the service successfully responded, you will be informed with a success
#'   message; If not, the content of the error will be reported to you.\cr
#'   Please run this function if you encounter any errors while using rbioapi.
#'   Also, if you need to contact support, kindly call this function with
#'   'diagnostic = TRUE' and include the output messages in your support
#'   request.
#' @param diagnostics (Logical) (default = FALSE) Show diagnostics and
#'   detailed messages with internal information.
#'
#' @return NULL, Connection test for the supported servers will be displayed
#'   in console
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_connection_test <- function(diagnostics = FALSE) {
  message("Checking Your connection to the Databases",
          " currently Supported by rbioapi:")

  urls <- list("STRING" = paste0(.rba_stg("string", "url"),
                                "/api/json/version"),
              "Enrichr" = paste0(.rba_stg("enrichr", "url"),
                                 "/Enrichr"),
              "Ensembl" = paste0(.rba_stg("ensembl", "url"),
                                 "/info/ping"),
              "MiEAA" = paste0(.rba_stg("mieaa", "url"),
                               "/mieaa2/api/"),
              "PANTHER" = paste0(.rba_stg("panther", "url"),
                                 "/services/api/panther"),
              "Reactome Content Service" = paste0(.rba_stg("reactome", "url"),
                                                  "/ContentService/data/database/name"),
              "Reactome Analysis Service" = paste0(.rba_stg("reactome", "url"),
                                                   "/AnalysisService/database/name"),
              "UniProt" = paste0(.rba_stg("uniprot", "url"),
                                 "/proteins/api/proteins/P25445")
  )

  cat("\U2022", "Internet", ":\r\n")
  google <- try(httr::status_code(httr::HEAD("https://www.google.com/",
                                             if (diagnostics) httr::verbose(),
                                             httr::user_agent(getOption("rba_user_agent")),
                                             httr::timeout(getOption("rba_timeout"))))
                , silent = TRUE)

  if (google == 200) {
    cat("\U2705 Connected to the Internet.\r\n")
  } else {
    cat("\U274C No Internet Connection.\r\n")
    stop("Could not resolve google.com", " . Check Your internet Connection.",
         call. = diagnostics)
  }

  for (i in seq_along(urls)) {
    cat("\U2022", names(urls)[[i]], ":\r\n")
    cat(.rba_api_check(urls[[i]], diagnostics = diagnostics), "\r\n")
  }
  invisible()
}

#' Set rbioapi Global Options
#'
#' A safe way to change rbioapi's global options and behavior. see "arguments"
#'   section for available options.\cr
#'   Note that you are not limited to changing the options globally, you can
#'   include the option names and values in the '...' argument of any rbioapi
#'   function to alter the option(s) only in that function call;
#'   e.g. example_function(x, diagnostics = TRUE, timeout = 300).\cr\cr
#'   Alternatively, you can call this function with no arguments, i.e.
#'   rba_options(), to retrieve a data frame of available rbioapi options and
#'   their current values.\cr
#'
#'   Because this function validates your provided changes, please
#'   \strong{\emph{only change rbioapi options using this function}} and avoid
#'   directly editing them.
#'
#' @param diagnostics (Logical) (default = FALSE) Show diagnostics and
#'   detailed messages with internal information.
#' @param dir_name (character) (default = "rbioapi") If the package needs to
#'   generate a file path to save the server's response, a directory with this
#'   name will be created in your working directory to save your files.
#' @param retry_max (Numeric) (default = 1) How many times should rbioapi
#'   retry in case of 5xx server responses, errors un-related to the server
#'   or no internet connectivity?
#' @param retry_wait (Numeric) (default = 10) Time in seconds to wait before
#'   next retry in case of internet connection or server problems.
#' @param progress (Logical) Should a progress bar be displayed? (default =
#'   FALSE)
#' @param save_file (Logical or character) (default = FALSE) Either:\itemize{
#'   \item TRUE: In this case, the raw server's response file will be
#'   automatically saved to a proper file path. use "dir_name" argument to
#'   change the file's parent directory.
#'   \item FALSE: (default) Do not automatically save server's response file.
#'   \item Character: (Only when changing the option via "..." argument in
#'   the exported functions) A valid file path to save the server's response
#'   file to the function that you are calling.}
#' @param skip_error (Logical) (default = FALSE) If TRUE, the code execution
#'   will not be stopped in case of errors (anything but HTTP status 200 from
#'   the server); Instead the error message will be returned as the function's
#'   output. Set this to TRUE if you don't want your script be interrupted by
#'   failed server responses.
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
#' \dontrun{rba_options(verbose = FALSE)}
#' \dontrun{rba_options(save_file = TRUE)}
#' \dontrun{rba_options(diagnostics = TRUE, progress = TRUE)}
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_options <- function(diagnostics = NA,
                        dir_name = NA,
                        retry_max = NA,
                        retry_wait = NA,
                        progress = NA,
                        save_file = NA,
                        skip_error = NA,
                        timeout = NA,
                        verbose = NA) {
  .rba_args(cond = list(list(quote(is.character(save_file)),
                             "As a global option, you can only set save_file to 'logical', not a file path.")))
  ## if empty function was called, show the available options
  changes <- vapply(ls(), function(x) {!is.na(get(x))}, logical(1))
  if (!any(changes)) {
    options_df <- data.frame(rbioapi_option = getOption("rba_user_options"),
                             current_value = vapply(names(getOption("rba_user_options")),
                                                    function(x) {as.character(getOption(x))},
                                                    character(1)),
                             value_class = vapply(names(getOption("rba_user_options")),
                                                  function(x) {class(getOption(x))},
                                                  character(1)),
                             stringsAsFactors = FALSE,
                             row.names = NULL)
    return(options_df)
  } else {
    ## change the provided options
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
#'   vector which can be parserd and evaluated. currently, this is only used
#'   in rba_pages
#'
#' @param input_call a list that contains the calls.
#' @param pb_switch Display a progress bar?
#'
#' @return The evaluation results of each input call.
#' @export
.rba_pages_do <- function(input_call, pb_switch) {
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
                     Sys.sleep(1)
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
#'   up to 100 pages. Just provide your rbioapi function and change to page
#'   argument to "pages:start_page:end_page", for example "pages:1:5".
#'
#'   To prevent flooding the server, there will be a 1 second delay between
#'   calls, also the user cannot iterate on more than 100 pages. The function
#'   will also override skip_error option and will always set it to TRUE.
#'   This means that in case of server response error (e.g. requesting pages
#'   that do not exist) an error message be returned to you instead of
#'   halting function's execution.
#'
#' @param input_call A quoted call. Provide a regular rbioapi function call,
#'   but with two differences:\enumerate{
#'   \item: Wrap a quote() around it. meaning: quote(rba_example())
#'   \item: Set the argument that corresponds to the page number to
#'   "pages:start_page:end_page", for example "pages:1:5".}\cr
#'   refer to the "examples" section to learn more.
#'
#' @return A named list where each element corresponds to a request's page.
#'
#' @examples
#' \dontrun{
#' rba_pages(input_call = quote(rba_uniprot_taxonomy(ids = 189831,
#'                                                      hierarchy = "siblings",
#'                                                      page_size = 50,
#'                                                      page_number = "pages:1:5"
#'                                                      )))
#' rba_pages(input_call = quote(rba_uniprot_taxonomy_name(name = "adenovirus",
#'                                                        field = "scientific",
#'                                                        search_type = "contain",
#'                                                        page_size = 200,
#'                                                        page_number = "pages:1:5",
#'                                                        verbose = FALSE)))
#' rba_pages(input_call = quote(rba_panther_info(what = "families",
#'                                               families_page = "pages:9:11")))
#' }
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_pages <- function(input_call){
  ## convert the input_call to character
  input_call <- as.character(substitute(input_call))
  if (input_call[[1]] != "quote") {
    stop("The call should be wrapped in qoute()",
         call. = getOption("rba_diagnostics"))
  }
  input_call <- input_call[[2]]
  if (!grepl("^rba_.+\\(", input_call)) {
    stop("You should provide a rbioapi function.",
         call. = getOption("rba_diagnostics"))
  }

  ## extract start and end pages
  start_page <- regmatches(input_call,
                           gregexpr("(?<=\"pages:)\\d+(?=:\\d+\")",
                                    input_call, perl = TRUE))[[1]]
  end_page <- regmatches(input_call,
                         gregexpr("(?<=\\d:)\\d+(?=\")",
                                  input_call, perl = TRUE))[[1]]
  ## check pages
  if (length(start_page) != 1 | length(end_page) != 1) {
    stop("The variable you want to paginate should be formatted as:",
         "`pages:start:end`.\r\nfor example: \"pages:1:5\".",
         call. = getOption("rba_diagnostics"))
  }
  start_page <- as.integer(start_page)
  end_page <- as.integer(end_page)
  if (end_page <= start_page) {
    stop("The starting page should be greater than the ending page.",
         call. = getOption("rba_diagnostics"))
  }
  if (end_page - start_page > 100) {
    stop("The maximum pages you are allowed to iterate are 100 pages.",
         call. = getOption("rba_diagnostics"))
  }

  ## only show progress bar if both verbose and diagnostics are off
  verbose_on <-
    !grepl(",\\s*verbose\\s*=\\s*FALSE", input_call) &&
    (grepl(",\\s*verbose\\s*=\\s*TRUE", input_call) ||
       isTRUE(getOption("rba_verbose")))
  diagnostics_on <-
    !grepl(",\\s*diagnostics\\s*=\\s*FALSE", input_call) &&
    (grepl(",\\s*diagnostics\\s*=\\s*TRUE", input_call) ||
       isTRUE(getOption("rba_diagnostics")))
  pb_switch <- !(verbose_on || diagnostics_on)

  ## build the calls
  # add skip_error = TRUE to the calls
  input_call <- gsub(",\\s*skip_error\\s*=\\s*(TRUE|FALSE)", "",
                     input_call,
                     perl = TRUE)
  input_call <- sub("\"pages:\\d+:\\d+\"", "%s, skip_error = TRUE",
                    input_call, perl = TRUE)

  input_call <- as.list(sprintf(input_call,
                                seq.int(from = start_page, to = end_page,
                                        by = 1)))
  names(input_call) <- paste0("page_",
                              seq.int(from = start_page, to = end_page, by = 1))

  ## Do the calls
  message("Iterating from page ", start_page, " to page ", end_page, ".")
  final_output <- .rba_pages_do(input_call,
                                pb_switch = pb_switch)
  return(final_output)
}

#' What to Cite?
#'
#' Since rbioapi will ultimately connects you other services, In addition to
#'  rbioapi , you should also cite those services if you have used rbioapi
#'  to publish a research paper.\cr
#'  This function will help you to learn which services you have used and
#'  refers you to the appropriate citation policy guide of that services.\cr
#'  Please note the Disclaimer in "Details" section!
#'
#'  Important Disclaimer: Please note that this function is only a starting
#'  place! To avoid any legal, ethical or professional problems, make sure to
#'  review citation policies and term of uses of any service/database/data
#'  you have used via rbioapi!
#'
#' @param ... You can provide one or more of the following:
#'   \enumerate{
#'   \item The name of the functions which you have used.
#'   \item The path to your script file(s).
#'   \item Working directory ( getwd() ) and/or any directories that contain your
#'   scripts.
#'   \item "rstudio_context" to scan your active source editor's context
#'   (only in Rtudio enviroment).
#'   }
#
#' @return NULL, messages about regard to neccassary citations will be
#'   displayed in the console.
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_citation <- function(...) {
  ## 1 Prepare input
  input <- as.character(substitute(list(...)))
  cat(sprintf("\r\n\ \U2022 To cite %s:\r\n \U2022\U2022 %s- %s\r\n",
              c("rbioapi", "R"), c(1,2), c(.rba_stg("citation", "rbioapi"),
                                           .rba_stg("citations", "r"))))
  if (length(input) == 1) {
    cat("\U2022 If you are not sure what services or databases to cite, you could call rba_citation() with:\r\n",
        "   1- The name of the functions which you have used.\r\n",
        "   2- The path to your script file(s).\r\n",
        "   3- Working directory [getwd()] and/or any directories that contain your scripts.\r\n",
        "   4- \"rstudio_context\" to scan your active source editor's context (only in Rtudio enviroment).\r\n")
  } else {
    ## check if the user provided directory paths
    input <- unique(input)
    input[input == "getwd()"] <- getwd()
    is_dir <- grepl("^[a-zA-z]:|^\\\\\\w|^/",
                    input, perl = TRUE) && !grepl("\\.\\w+$",
                                                  input, perl = TRUE)
    if (sum(is_dir) != 0) {
      read_func <- function(x) {
        tryCatch(list.files(x, full.names = TRUE, recursive = TRUE),
                 error = function(x){FALSE}, warning = function(x){invisible()})
      }
      input <- append(input[!is_dir], unlist(lapply(input[is_dir], read_func)))
    }
    ## check if the user provided file paths
    is_file <- grepl("\\.\\w+$", input, perl = TRUE)
    if (sum(is_file) != 0) {
      read_func <- function(x) {
        tryCatch(unique(scan(x, what = character(), sep = "\n", quiet = TRUE)),
                 error = function(x){FALSE}, warning = function(x){invisible()})
      }
      input <- unique(append(input[!is_file],
                             unlist(lapply(input[is_file], read_func))))
    }
    ## check if the user requested to scan rstudio
    if (Sys.getenv("RSTUDIO") == "1" && any(input == "rstudio_context")) {
      input <- append(input, as.character(rstudioapi::getSourceEditorContext()))}
    ## 2 Search patterns
    patt <- sprintf("(?<=\\brba_)(%s)(?=_\\w+?\\()",
                    paste(.rba_stg("db"), collapse = "|"))
    rba_used <- lapply(input, function(x){
      regmatches(x, regexpr(patt, x, perl = TRUE))})
    rba_used <- unique(unlist(rba_used))
    rba_cites <- unlist(lapply(rba_used, function(x){
      .rba_stg("citation", x) }))
    if (length(rba_used) != 0) {
      cat(sprintf("\r\n\ \U2022 To cite %s:\r\n \U2022\U2022 %s- %s\r\n",
                  rba_used, seq_along(rba_used) + 2, rba_cites))
    } else {
      cat("\r\n \U2022 Nothing was detected in your provided arguments.")
    }
  }
  invisible()
}
