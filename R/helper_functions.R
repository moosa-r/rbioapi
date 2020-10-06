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
#' @param diagnostics logical: Generate diagnostics and detailed messages with
#'   internal information.
#'
#' @return NULL
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_connection_test = function(diagnostics = FALSE) {
  message("Checking Your connection to the Databases",
          " currently Supported by rbioapi:")

  urls = list("STRING" = paste0(rba_ba_stg("string", "url"),
                                "/api/json/version"),
              "Enrichr" = paste0(rba_ba_stg("enrichr", "url"),
                                 "/Enrichr"),
              "Ensembl" = paste0(rba_ba_stg("ensembl", "url"),
                                 "/info/ping"),
              "Reactome Content Service" = paste0(rba_ba_stg("reactome", "url"),
                                                  "/ContentService/data/database/name"),
              "Reactome Analysis Service" = paste0(rba_ba_stg("reactome", "url"),
                                                   "/AnalysisService/database/name"),
              "UniProt" = paste0(rba_ba_stg("uniprot", "url"),
                                 "/proteins/api/proteins/P25445")
  )

  cat("\U2022", "Internet", ":\r\n")
  google = try(httr::status_code(httr::HEAD("https://www.google.com/",
                                            if (diagnostics) httr::verbose(),
                                            httr::user_agent(getOption("rba_user_agent")),
                                            httr::timeout(getOption("rba_client_timeout"))))
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
    cat(rba_ba_api_check(urls[[i]], diagnostics = diagnostics), "\r\n")
  }
  invisible()
}

#' Set rbioapi Global Options
#'
#' A Safe way to alter rbioapi's global options. Please refer to the
#'   Arguments section for a detailed description of Available options.\cr
#'   Alternatively, you can call this function with no arguments [ e.g.
#'   rba_options() ] to retrieve a table of available rbioapi options and their
#'   current values.\cr
#'   Note: Because this function validates your provided changes, Kindly
#'   \strong{\emph{only change rbioapi options using this function}} and avoid
#'   directly editing them.
#'
#' Note that you are not limited to changing the options globally, you could
#'   include the option names and values in the '...' argument of any rbioapi
#'   function to alter the option in that function's call only;
#'   e.x, example_function(x, y, diagnostics = TRUE).
#'
#' @param client_timeout numeric: The maximum time in seconds that you are
#'   willing to wait for a server response before giving up and stopping the
#'   function execution.
#' @param diagnostics logical: Generate diagnostics and detailed messages with
#'   internal information.
#' @param dir_name character: If the package needed to generate a file path to
#'   save the server's response, a directory with this name will be created in
#'   your working directory to save your files.
#' @param max_retries numeric: How many times should rbioapi retry in case of
#'   5xx server responses, errors or no internet connectivity?
#' @param wait_time numeric: Time in seconds to wait before each retry in case
#'   of internet connection lost or server problems.
#' @param progress_bar logical: Should a progress bar be displayed?
#' @param skip_error logical: If TRUE, the code execution will not be stopped
#'   in case of errors (anything but HTTP status 200 from the server); Instead
#'   the error message will be returned as the function's output. Set this
#'   to 'TRUE' if you are calling rbioapi function in batch batch mode or
#'   sourcing a script.
#' @param verbose logical: Generate informative messages.
#'
#' @return NULL
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_options = function(client_timeout = NA,
                       diagnostics = NA,
                       dir_name = NA,
                       max_retries = NA,
                       progress_bar = NA,
                       save_resp_file = NA,
                       skip_error = NA,
                       verbose = NA,
                       wait_time = NA) {
  rba_ba_args(cond = list(list(quote(is.character(save_resp_file)),
                               "As a global option, you can only set save_resp_file to 'logical', not a file path.")))
  ## if empty function was called, show the available options
  changes = vapply(ls(), function(x) {!is.na(get(x))}, logical(1))
  if (!any(changes)) {
    options_df = data.frame(rbioapi_option = getOption("rba_user_options"),
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
      chng_content = get(chng)
      eval(parse(text = sprintf(ifelse(is.character(chng_content),
                                       yes = "options(%s = \"%s\")",
                                       no = "options(%s = %s)"),
                                paste0("rba_", chng),
                                chng_content)))
    }
    invisible()
  }
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
#' @return NULL
#' @family "Helper functions"
#' @keywords Helper
#' @export
rba_citation = function(...) {
  ## 1 Prepare input
  input = as.character(substitute(list(...)))
  cat(sprintf("\r\n\ \U2022 To cite %s:\r\n \U2022\U2022 %s- %s\r\n",
              c("rbioapi", "R"), 1:2, c(rba_ba_stg("citation", "rbioapi"),
                                        rba_ba_stg("citations", "r"))))
  if (length(input) == 1) {
    cat("\U2022 If you are not sure what services or databases to cite, you could call rba_citation() with:\r\n",
        "   1- The name of the functions which you have used.\r\n",
        "   2- The path to your script file(s).\r\n",
        "   3- Working directory [getwd()] and/or any directories that contain your scripts.\r\n",
        "   4- \"rstudio_context\" to scan your active source editor's context (only in Rtudio enviroment).\r\n")
  } else {
    ## check if the user provided directory paths
    input = unique(input)
    input[input == "getwd()"] = getwd()
    is_dir = grepl("^[a-zA-z]:|^\\\\\\w|^/",
                   input, perl = TRUE) & !grepl("\\.\\w+$",
                                                input, perl = TRUE)
    if (sum(is_dir) != 0) {
      read_func = function(x) {
        tryCatch(list.files(x, full.names = TRUE, recursive = TRUE),
                 error = function(x){FALSE}, warning = function(x){invisible()})
      }
      input = append(input[!is_dir], unlist(lapply(input[is_dir], read_func)))
    }
    ## check if the user provided file paths
    is_file = grepl("\\.\\w+$", input, perl = TRUE)
    if (sum(is_file) != 0) {
      read_func = function(x) {
        tryCatch(unique(scan(x, what = character(), sep = "\n", quiet = TRUE)),
                 error = function(x){FALSE}, warning = function(x){invisible()})
      }
      input = unique(append(input[!is_file],
                            unlist(lapply(input[is_file], read_func))))
    }
    ## check if the user requested to scan rstudio
    if (Sys.getenv("RSTUDIO") == "1" && any(input == "rstudio_context")) {
      input = append(input, as.character(rstudioapi::getSourceEditorContext()))}
    ## 2 Search patterns
    patt = sprintf("(?<=\\brba_)(%s)(?=_\\w+?\\()",
                   paste(rba_ba_stg("db"), collapse = "|"))
    rba_used = lapply(input, function(x){
      regmatches(x, regexpr(patt, x, perl = TRUE))})
    rba_used = unique(unlist(rba_used))
    rba_cites = unlist(lapply(rba_used, function(x){
      rba_ba_stg("citation", x) }))
    if (length(rba_used) != 0) {
      cat(sprintf("\r\n\ \U2022 To cite %s:\r\n \U2022\U2022 %s- %s\r\n",
                  rba_used, seq_along(rba_used) + 2, rba_cites))
    } else {
      cat("\r\n \U2022 Nothing was detected in your provided arguments.")
    }
  }
  invisible()
}
