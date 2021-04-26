##### data containers #######################################################
#' Internal Data Container for rbioapi
#'
#' A central way to return information necessary for the internal functions to
#'   work.
#'
#' Consult the source codes to learn about supported arguments and data
#'   structure. it is straightforward and self-explanatory.
#'   Currently the first argument can be one of 'db', 'options', 'test',
#'   'citations' or a service name.
#'
#' @param ... A sequence of arguments in which the function will traverse across
#'    the defined data storage tree. Only the first arguments will be passed
#'    to match.arg().
#' @return Based on the called sequence of arguments, it could be any object
#'   type. but mostly, it will be of class character.
#' @family internal_data_container
#' @export
.rba_stg <- function(...){
  arg <- c(...)
  #possible arguments
  output <- switch(arg[[1]],
                   db = c("enrichr", "ensembl", "mieaa", "reactome",
                          "string", "uniprot"),
                   enrichr = switch(
                     arg[[2]],
                     name = "Enrichr",
                     url = "https://maayanlab.cloud",
                     pth = "Enrichr/",
                     ptn = "^(https?://)?(www\\.)?maayanlab\\.cloud/Enrichr/",
                     err_ptn = "^$"
                   ),
                   ensembl = switch(
                     arg[[2]],
                     name = "Ensembl",
                     url = "https://rest.ensembl.org",
                     ptn = "^(https?://)?(www\\.)?rest\\.ensembl\\.org/",
                     err_ptn = "^4\\d\\d$",
                     err_prs = list("json->list_simp",
                                    function(x) {x[["error"]][[1]]})
                   ),
                   mieaa = switch(
                     arg[[2]],
                     name = "MiEAA",
                     url = "https://ccb-compute2.cs.uni-saarland.de",
                     pth = "mieaa2/api/v1/",
                     ptn = "^(https?://)?(www\\.)?ccb-compute2\\.cs\\.uni-saarland\\.de/mieaa2/",
                     err_ptn = "^4\\d\\d$",
                     err_prs = list("json->chr")
                   ),
                   panther = switch(
                     arg[[2]],
                     name = "PANTHER",
                     url = "http://www.pantherdb.org",
                     pth = "services/oai/pantherdb/",
                     ptn = "^(https?://)?(www\\.)?pantherdb\\.org/services/",
                     err_ptn = "^4\\d\\d&",
                     err_prs = list("json->list_simp",
                                    function(x) {x$search$error})
                   ),
                   reactome = switch(
                     arg[[2]],
                     name = "Reactome",
                     url = "https://reactome.org",
                     pth = switch(match.arg(arg[[3]],
                                            c("analysis",
                                              "content")),
                                  analysis = "AnalysisService/",
                                  content = "ContentService/"),
                     ptn = "^(https?://)?(www\\.)?reactome\\.org/(?:AnalysisService|ContentService)/",
                     err_ptn = "^4\\d\\d$",
                     err_prs = list("json->list_simp",
                                    function(x) {x[["messages"]][[1]]})
                   ),
                   string = switch(
                     arg[[2]],
                     name = "STRING",
                     url = "https://version-11-0.string-db.org",
                     pth = "api/",
                     ptn = "^(http.?://).*string-db\\.org/api/",
                     err_ptn = "^4\\d\\d$",
                     err_prs = list("json->list_simp",
                                    function(x) {paste(x, collapse = "\r\n")})
                   ),
                   uniprot = switch(
                     arg[[2]],
                     name = "UniProt",
                     url = "https://www.ebi.ac.uk",
                     pth = "proteins/api/",
                     ptn = "^(https?://)?(www\\.)?ebi\\.ac\\.uk/proteins/api/",
                     err_prs = list("json->list_simp",
                                    function(x) {x[["errorMessage"]][[1]]}),
                     err_ptn = "^4\\d\\d$"
                   ),
                   options = switch(
                     as.character(length(arg)),
                     "1" = options()[grep("^rba_",
                                          names(options()))],
                     getOption(arg[[2]])),
                   citations = switch(
                     arg[[2]],
                     rbioapi = "** rbio api****",
                     r = "R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.",
                     enrichr = "https://maayanlab.cloud/Enrichr/help#terms",
                     ensembl = "***ensembl api papeer***",
                     reactome = "https://reactome.org/cite",
                     string = "Szklarczyk D, Gable AL, Lyon D, Junge A, Wyder S, Huerta-Cepas J, Simonovic M, Doncheva NT, Morris JH, Bork P, Jensen LJ, Mering CV. STRING v11: protein-protein association networks with increased coverage, supporting functional discovery in genome-wide experimental datasets. Nucleic Acids Res. 2019 Jan 8;47(D1):D607-D613. doi: 10.1093/nar/gky1131. PMID: 30476243; PMCID: PMC6323986.",
                     uniprot = "***uniprot api papeer***"),
                   tests = list("Enrichr" = paste0(.rba_stg("enrichr", "url"),
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
                                "STRING" = paste0(.rba_stg("string", "url"),
                                                  "/api/json/version"),
                                "UniProt" = paste0(.rba_stg("uniprot", "url"),
                                                   "/proteins/api/proteins/P25445")
                   )
  )
  return(output)
}

##### Internet connectivity ##################################################

#' Handle Situations with Connection or Server Problems
#'
#' When called, the function will test the Internet connection. Based on called
#'   arguments it will try suspend the execution of R codes and retry and test
#'   if necessary until the device is connected back to the internet.
#'
#' @param retry_max numeric: The maximum times to Retry the connection test.
#' @param retry_wait numeric: The value in seconds which will be passed to
#'   sys.sleep() between each connection test.
#' @param verbose logical: Generate informative messages.
#' @param diagnostics logical: Generate diagnostics and detailed messages with
#'   internal information.
#'
#' @return TRUE if connected to the internet, a character string if not.
#' @family internal_inernet_connectivity
#' @export
.rba_net_handle <- function(retry_max = 1,
                            retry_wait = 10,
                            verbose = FALSE,
                            diagnostics = FALSE) {
  if (isTRUE(diagnostics)) {message("Testing the internet connection.")}
  test_call <- quote(
    httr::status_code(httr::HEAD("https://www.google.com/",
                                 httr::timeout(getOption("rba_timeout")),
                                 if (diagnostics) httr::verbose())
    ))
  net_status <- try(eval(test_call), silent = TRUE)
  retry_count <- 0

  while (net_status != 200 && retry_count < retry_max) {
    retry_count <- retry_count + 1
    if (isTRUE(verbose)) {
      message(sprintf("No internet connection, waiting for %s seconds and retrying (retry count: %s/%s).",
                      retry_wait,
                      retry_count,
                      retry_max))
    }
    Sys.sleep(retry_wait)
    net_status <- try(eval(test_call), silent = TRUE)
  } #end of while

  if (net_status == 200) {
    if (isTRUE(diagnostics)) {message("Device is connected to the internet!")}
  } else {
    stop("No internet connection; Stopping code execution!",
         call. = diagnostics)
  } #end of if net_test
  return(net_status == 200)
}

#' Translate HTTP Status Code to Human-Readable Explanation
#'
#' It will make HTTP status more informative by trying to translate it to a
#'   human readable and informative text. this function will be called by
#'   rba_error_parser().
#'
#' @param http_status numeric: A given Standard HTTP status code.
#' @param verbose logical: Should the function return a sentence case?
#'
#' @return Character string. Returns the HTTP status code with it's class and
#'   possibly it's meaning.
#'
#' @references \href{https://www.iana.org/assignments/http-status-codes/}{IANA:
#'   Hypertext Transfer Protocol (HTTP) Status Code Registry}
#'
#' @family internal_inernet_connectivity
#' @export
.rba_http_status <- function(http_status, verbose = FALSE){
  #ref:
  http_status <- as.character(http_status)
  stopifnot(grepl("^[12345]\\d\\d$", http_status))

  resp <- switch(substr(http_status, 1, 1),
                 "1" = list(class = "Informational",
                            deff = switch(
                              http_status,
                              "100" = "Continue",
                              "101" = "Switching Protocols",
                              "102" = "Processing",
                              "103" = "Early Hints")),
                 "2" = list(class = "Success",
                            deff = switch(
                              http_status,
                              "200" = "OK",
                              "201" = "Created",
                              "202" = "Accepted",
                              "203" = "Non-Authoritative Information",
                              "204" = "No Content",
                              "205" = "Reset Content",
                              "206" = "Partial Content",
                              "207" = "Multi-Status",
                              "208" = "Already Reported",
                              "226" = "IM Used")),
                 "3" = list(class = "Redirection",
                            deff = switch(
                              http_status,
                              "300" = "Multiple Choices",
                              "301" = "Moved Permanently",
                              "302" = "Found",
                              "303" = "See Other",
                              "304" = "Not Modified",
                              "305" = "Use Proxy",
                              "306" = "Switch Proxy",
                              "307" = "Temporary Redirect",
                              "308" = "Permanent Redirect")),
                 "4" = list(class = "Redirection",
                            deff = switch(
                              http_status,
                              "400" = "Bad Request",
                              "401" = "Unauthorized",
                              "402" = "Payment Required",
                              "403" = "Forbidden",
                              "404" = "Not Found",
                              "405" = "Method Not Allowed",
                              "406" = "Not Acceptable",
                              "407" = "Proxy Authentication Required",
                              "408" = "Request Timeout",
                              "409" = "Conflict",
                              "410" = "Gone",
                              "411" = "Length Required",
                              "412" = "Precondition Failed",
                              "413" = "Payload Too Large",
                              "414" = "URI Too Long",
                              "415" = "Unsupported Media Type",
                              "416" = "Range Not Satisfiable",
                              "417" = "Expectation Failed",
                              "421" = "Misdirected Request",
                              "422" = "Unprocessable Entity",
                              "423" = "Locked",
                              "424" = "Failed Dependency",
                              "425" = "Too Early",
                              "426" = "Upgrade Required",
                              "428" = "Precondition Required",
                              "429" = "Too Many Requests",
                              "431" = "Request Header Fields Too Large",
                              "451" = "Unavailable For Legal Reasons")),
                 "5" = list(class = "Redirection",
                            deff = switch(
                              http_status,
                              "500" = "Internal Server Error",
                              "501" = "Not Implemented",
                              "502" = "Bad Gateway",
                              "503" = "Service Unavailable",
                              "504" = "Gateway Timeout",
                              "505" = "HTTP Version Not Supported",
                              "506" = "Variant Also Negotiates",
                              "507" = "Insufficient Storage",
                              "508" = "Loop Detected",
                              "510" = "Not Extended",
                              "511" = "Network Authentication Required"))
  )

  output <- ifelse(!is.null(resp$deff),
                   yes = sprintf("HTTP Status '%s' (%s: %s)",
                                 http_status, resp$class, resp$deff),
                   no = sprintf("HTTP Status '%s' (%s class)",
                                http_status, resp$class))
  if (isTRUE(verbose)) {
    output <- sprintf("The server returned %s.", output)
  }
  return(output)
}

##### API Calls ##################################################

#' Add Additional Parameters to API-Call's Body
#'
#' Evaluate the Expression presented in the input format and Builds a list which
#'  will serve as a query input for httr request.
#'
#' @param init list: initial default query parameters in the format of named
#'   list. provide list() if it is empty.
#' @param ... list: Additional queries to evaluate and possibly append to
#'   the initial parameters. formatted as lists with the following order:
#'   \enumerate{
#'   \item parameter's name based on the API documentation,
#'   \item An expression to be evaluated to either TRUE or FALSE,
#'   \item A value that should be appended to the list in case of the expression
#'   being TRUE.}
#'
#' @return Named list. with the formal API parameter's names as name and
#'   corresponding values.
#'
#' @family internal_api_calls
#' @export
.rba_query <- function(init, ...) {
  ## check the input method
  ext_par <- list(...)
  if (utils::hasName(ext_par, "extra_pars")) {
    ext_par <- ext_par$extra_pars
  }
  ## evaluate extra parameters
  ext_evl <- vapply(X = ext_par,
                    FUN = function(x) {
                      if (length(x[[2]]) > 1) {
                        warning("Internal Query Builder:\r\n",
                                x[[1]],
                                " has more than one element. Only the first element will be used.",
                                call. = FALSE)
                      }
                      if (isTRUE(x[[2]][[1]])) {
                        return(TRUE)
                      } else if (isFALSE(x[[2]][[1]])) {
                        return(FALSE)}
                      else {
                        warning("Internal Query Builder:\r\n The evaluation result of ",
                                x[[1]],
                                " is not TRUE or FALSE, thus skipping it.",
                                call. = FALSE)
                        return(FALSE)}
                    },
                    FUN.VALUE = logical(1))

  # extract extra parameters where theirs second element was TRUE
  ext_val <- lapply(ext_par[ext_evl], function(x) {x[[3]]})
  # set names to the extracted parameters
  if (length(ext_val) >= 1) {
    names(ext_val) <- vapply(ext_par[ext_evl],
                             function(x) {x[[1]]},
                             character(1))
    init <- append(init, ext_val)
  }
  return(init)
}

#' Build httr HTTP Query
#'
#' Converts package's exported functions input to a function call understandable
#'   by httr package.
#'
#' This is a convenient interface between rbioapi exported functions and httr
#'   package. Apart from producing a standard expression compatible with httr,
#'   it can resolve the case when multiple parsers or HTTP accept parameters are
#'   possible according to the end-user's inputs. Also, it will append
#'   'httr::write()', 'httr::progress' and 'httr::vebose()' based on the
#'   end-user's inputs.
#'   \cr There are two scenarios with providing accepted response and response
#'   parser arguments:
#'   \cr 1- If it is pre-defined and end-user's inputs will not affect the accepted
#'   and parser values, pass them as accept = x and parser = y.
#'   \cr 2- If these values should be chosen according to save_to argument, pass
#'   them as file_parser, file_accept, obj_parser and obj_accept. In this case,
#'   if save_to argument is a path or logical TRUE, the response will be saved
#'   to disk and file parser and accept will be chosen, if not, obj parser and
#'   accept will be chosen to build httr's function call.
#'
#' @param httr A HTTP verb's name. Can be one of 'get', 'post', 'head', 'put',
#'   'patch' or 'delete'.
#' @param url A URL to the HTTP resource being called.
#' @param path A path to the HTTP resource being called.
#' @param ... Additional arguments. 'save_to', 'accept', 'parser',
#'   'file_accept', 'obj_accept', 'file_parser' and 'obj_parser' will be
#'   processed. The rest will be passed to httr function's ... argument.
#'
#' @return a list with two elements: call, which is a standard httr function
#'  call and parser which is a character string that will be used later by other
#'  rbioapi internal functions.
#'
#' @family internal_api_calls
#' @export
.rba_httr <- function(httr,
                      url = NULL,
                      path = NULL,
                      ...) {
  ## assign global options
  diagnostics <- get0("diagnostics", envir = parent.frame(1),
                      ifnotfound = getOption("rba_diagnostics"))
  progress <- get0("progress", envir = parent.frame(1),
                   ifnotfound = getOption("rba_progress"))
  timeout <- get0("timeout", envir = parent.frame(1),
                  ifnotfound = getOption("rba_timeout"))
  ### 1 capture extra arguments
  # possible args: all args supported by httr +
  # args to this function: [file/obj_]accept, [file/obj_]parser, save_to
  ext_args <- list(...)

  ### 2 build main HTTP request (using httr)
  httr_call <- list(switch(httr,
                           "get" = quote(httr::GET),
                           "post" = quote(httr::POST),
                           "head" = quote(httr::HEAD),
                           "put" = quote(httr::PUT),
                           "delete" = quote(httr::DELETE),
                           "patch" = quote(httr::PATCH),
                           stop("internal error: what verb to use with httr?",
                                call. = TRUE)),
                    url = utils::URLencode(URL = url, repeated = FALSE),
                    path = utils::URLencode(URL = path, repeated = FALSE),
                    quote(httr::user_agent(getOption("rba_user_agent"))),
                    quote(httr::timeout(timeout))
  )
  if (isTRUE(diagnostics)) {
    httr_call <- append(httr_call, quote(httr::verbose()))
  }
  if (isTRUE(progress)) {
    httr_call <- append(httr_call, quote(httr::progress()))
  }

  ###  3 deal with extra arguments
  if (length(ext_args) >= 1) {
    ### 3.1 check if there is "save to file vs return R object" scenario
    if (sum(utils::hasName(ext_args, "save_to"),
            utils::hasName(ext_args, "file_accept"),
            utils::hasName(ext_args, "obj_accept")) == 3) {
      ## 3.1.a it was up to the  end-user to choose the response type
      if (isFALSE(ext_args$save_to)) {
        httr_call <- append(httr_call,
                            list(str2lang(sprintf("httr::accept(\"%s\")",
                                                  ext_args$obj_accept))))
        if (utils::hasName(ext_args, "obj_parser")) {parser <- ext_args$obj_parser}
      } else {
        httr_call <- append(httr_call,
                            list(str2lang(sprintf("httr::accept(\"%s\")",
                                                  ext_args$file_accept)),
                                 str2lang(sprintf("httr::write_disk(\"%s\", overwrite = TRUE)",
                                                  ext_args$save_to))
                            ))
        if (utils::hasName(ext_args, "file_parser")) {parser <- ext_args$file_parser}
      }
    } else {
      ## 3.1.b it was a pre-defined response type
      # accept header?
      if (utils::hasName(ext_args, "accept")) {
        httr_call <- append(httr_call,
                            list(str2lang(sprintf("httr::accept(\"%s\")",
                                                  ext_args$accept))))
      }
      # save to file?
      if (utils::hasName(ext_args, "save_to") && !isFALSE(ext_args$save_to)) {
        httr_call <- append(httr_call,
                            list(str2lang(sprintf("httr::write_disk(\"%s\", overwrite = TRUE)",
                                                  ext_args$save_to))))
      }
      # parser?
      if (utils::hasName(ext_args, "parser")) {
        parser <- ext_args$parser
      } else {
        parser <- NULL
      }
    }
    ### remove extra arguments that you don't want in httr function call
    ext_args <- ext_args[!grepl("^(?:accept|file_accept|obj_accept|save_to|\\w*parser)$",
                                names(ext_args))]
  } #end of if (length(ext_args...
  httr_call <- list(call = as.call(append(httr_call, ext_args)),
                    parser = parser)
  return(httr_call)
}

#' Internal function to make http request
#'
#' This function will be called by .rba_skeleton() and is the internal
#'   function which resides between making an httr function call using
#'   .rba_httr and evaluating that call to retrieve a response from the API
#'   server.
#'
#' In case of an error (anything other than status code 200), the function will
#'   perform extra steps according to the context:
#'   \cr 1- If it was not possible to establish a connection with the server,
#'   .rba_net_handle() will be called to handle the situation.
#'   \cr 2- If the server returned a status code 5xx, calling the server will be
#'   retried accordingly.
#'   \cr 3- if the server returned status code other than 200 or 5xx, the response
#'   and status code will be handed to rba_error_parser() to handle the
#'   situation.
#'
#' @param input_call A httr function call made  by .rba_httr().
#' @param skip_error logical: If TRUE, in case of an error HTTP status other
#'  than 200, instead of halting the code execution, the error message will be
#'  returned as the function's output.
#' @param retry_max numeric: A value to be passed to
#'   .rba_net_handle() retry_max argument.
#' @param retry_wait numeric: A value to be passed to
#'   .rba_net_handle() retry_wait argument.
#' @param verbose should the function generate informative messages?
#' @param diagnostics logical: Generate diagnostics and detailed messages with
#'   internal information.
#'
#' @return A raw server response in the format of httr's class "response". in
#'   the case of status code other than 200 and skip_error = TRUE, a character
#'   string with the pertinent error message.
#'
#' @family internal_api_calls
#' @export
.rba_api_call <- function(input_call,
                          skip_error = FALSE,
                          retry_max = 1,
                          retry_wait = 10,
                          verbose = TRUE,
                          diagnostics = FALSE) {
  ## 1 call API
  response <- try(eval(input_call, envir = parent.frame(n = 2)),
                  silent = !diagnostics)
  ## 2 check the internet connection & 5xx http status
  if (!inherits(response, "response") ||
      substr(response$status_code, 1, 1) == "5") {
    ## 2.1 there is an internet connection or server issue
    # wait for the internet connection
    net_connected <- .rba_net_handle(retry_max = retry_max,
                                     retry_wait = retry_wait,
                                     verbose = verbose,
                                     diagnostics = diagnostics)
    if (isTRUE(net_connected)) {
      ## 2.1.1 net_connection test is passed
      response <- try(eval(input_call, envir = parent.frame(n = 2)),
                      silent = !diagnostics)
    } else {
      ## 2.1.2 net_connection test is not passed
      stop("No internet connection; Stopping code execution!",
           call. = diagnostics)
    }
  } # end of step 2

  ## 3 Decide what to return
  if (!inherits(response, "response")) {
    ## 3.1 errors un-related to server's response
    error_message <- response
    if (isTRUE(skip_error)) {
      return(error_message, call. = diagnostics)
    } else {
      stop(error_message, call. = diagnostics)
    }
  } else if (substr(response$status_code, 1, 1) != "2") {
    ## 3.2 API call was not successful
    error_message <- .rba_error_parser(response = response, verbose = verbose)
    if (isTRUE(skip_error)) {
      return(error_message)
    } else {
      stop(error_message, call. = diagnostics)
    }
  } else {
    ## 3.3 Everything is OK (HTTP status == 200)
    return(response)
  }
}

#' A Wrapper for API Calling and Parsing the Response
#'
#' This function will be called at the last step of any exported function to
#'   call the server API using .rba_api_call() and parse the response using
#'   .rba_response_parser().
#'
#' The function will try to use the parser specified in the 'input_call' object,
#'   but if a parser value was provided with the 'response_parser' argument,
#'   it will have priority and will overwrite the input_call's parser input.
#'   \cr diagnostics, verbose, retry_max, retry_wait and skip_error variables
#'   \cr will be assigned and passed on to the subsequent executed calls.s
#'   \cr note that the function was much longer at the begging of this package
#'   development, hence the name 'skeleton'.
#'
#' @param input_call list: The exact output of .rba_httr()
#' @param response_parser A string vector corresponding to the pre-defined
#'   parser calls in .rba_response_parser() or an expression to be evaluated by
#'   .rba_response_parser().
#'
#' @return A parsed server Response which may be and R object of any class,
#'   depending on .rba_response_parser() output. In case of error and
#'   'skip_error = TRUE', the output will be the error message as a character
#'   string.
#'
#' @family internal_api_calls
#' @export
.rba_skeleton <- function(input_call,
                          response_parser = NULL) {
  ## 0 assign options variables
  diagnostics <- get0("diagnostics", envir = parent.frame(1),
                      ifnotfound = getOption("rba_diagnostics"))
  verbose <- get0("verbose", envir = parent.frame(1),
                  ifnotfound = getOption("rba_verbose"))
  retry_max <- get0("retry_max", envir = parent.frame(1),
                    ifnotfound = getOption("rba_retry_max"))
  retry_wait <- get0("retry_wait", envir = parent.frame(1),
                     ifnotfound = getOption("rba_retry_wait"))
  skip_error <- get0("skip_error", envir = parent.frame(1),
                     ifnotfound = getOption("rba_skip_error"))
  ## 1 Make API Call
  response <- .rba_api_call(input_call = input_call$call,
                            skip_error = skip_error,
                            retry_max = retry_max,
                            retry_wait = retry_wait,
                            verbose = verbose,
                            diagnostics = diagnostics)
  ## 2 Parse the the response if possible
  # Parser Provided via .rba_skeleton's 'response parser' argument will
  # override the 'parser' provided in input call
  if (!is.null(response_parser)) {
    parser_input <- response_parser
  } else {
    parser_input <- input_call$parser
  }

  if (inherits(response, "response") && !is.null(parser_input)) {
    final_output <- .rba_response_parser(response, parser_input)
  } else {
    final_output <- response
  }

  ## 3 Return the output
  return(final_output)
}

#### Check Arguments #######

#' Add rbioapi options to user's Arguments Check
#'
#' This function is an internal component of .rba_args(). It will
#'   add user-defiended rbioapi options variables (provided by the "..."
#'   arguments in the exported function call) to .rba_args's cond and cons.
#'
#' The aim of this function is to eliminate the need
#'   to write explicit options arguments checking when writing the exported
#'   functions. Without this, the developer was forced to repeatably include
#'   every rbioapi options arguments in argument checking segment of each
#'   exported function.
#'
#' @param cons Constrains to be evaluated.
#' @param cond Conditions to be evaluated.
#' @param what what to build? cond or cons?
#'
#' @return NULL. If The arguments check failed, the code execution will be
#' halted or a warning will be issued.
#'
#' @family internal_arguments_check
#' @export
.rba_args_opts <- function(cons = NULL, cond = NULL, what) {
  if (what == "cons") {
    ext_cons <- list(timeout = list(arg = "timeout",
                                    class = "numeric",
                                    len = 1,
                                    ran = c(0.001, 3600)),
                     dir_name = list(arg = "dir_name",
                                     class = "character",
                                     len = 1),
                     diagnostics = list(arg = "diagnostics",
                                        class = "logical",
                                        len = 1),
                     retry_max = list(arg = "retry_max",
                                      class = "numeric",
                                      len = 1),
                     progress = list(arg = "progress",
                                     class = "logical",
                                     len = 1),
                     save_file = list(arg = "save_file",
                                      class = c("logical",
                                                "character"),
                                      len = 1),
                     skip_error = list(arg = "skip_error",
                                       class = "logical",
                                       len = 1),
                     verbose = list(arg = "verbose",
                                    class = "logical",
                                    len = 1),
                     retry_wait = list(arg = "retry_wait",
                                       class = "numeric",
                                       len = 1,
                                       min_val = 1))
    cons <- append(ext_cons[names(ext_cons) %in% ls(envir = parent.frame(2))],
                   cons)
    return(cons)
  } else if (what == "cond") {
    ext_cond <- list(dir_name = list(quote(grepl("[\\\\/:\"*?<>|]+", dir_name, perl = TRUE)),
                                     "Invalid dir_name. Directory name cannot include these characters: \\/?%*:|<>"),
                     save_file = list(quote(!is.logical(save_file) &&
                                              !grepl("^[a-zA-z]:|^\\\\\\w|^/|\\w+\\.\\w+$",
                                                     save_file)),
                                      "Invalid save_file. You should set it to 'logical' or 'a valid file path'."))
    cond <- append(ext_cond[names(ext_cond) %in% ls(envir = parent.frame(2))],
                   cond)
    return(cond)
  } else {
    stop("Internal error")
  }
}

#' Check If A cons Element Follows A Constrain Type
#'
#' This function will take a single element from the .rba_args()'s
#'    cons argument and a single constrain type and checks if it is TRUE.
#'
#' @param cons_i element i from .rba_args()'s cons argument.
#' @param what what constrain to check? it should be one of the possible cons
#'  types defined in .rba_args()'s documentations.
#'
#' @return Logical. TRUE if element i is correct with regard to the constrain
#'   "what"; FALSE otherwise.
#'
#' @family internal_arguments_check
#' @export
.rba_args_cons_chk <- function(cons_i, what) {
  if (any(!is.na(cons_i[["evl_arg"]]))) {
    output <- all(switch(what,
                         "class" = class(cons_i[["evl_arg"]]) %in% cons_i[["class"]],
                         "val" = all(cons_i[["evl_arg"]] %in% cons_i[["val"]]),
                         "ran" = all(cons_i[["evl_arg"]] >= cons_i[["ran"]][[1]],
                                     cons_i[["evl_arg"]] <= cons_i[["ran"]][[2]]),
                         "len" = length(cons_i[["evl_arg"]]) == cons_i[["len"]],
                         "min_len" = length(cons_i[["evl_arg"]]) >= cons_i[["min_len"]],
                         "max_len" = length(cons_i[["evl_arg"]]) <= cons_i[["max_len"]],
                         "min_val" = cons_i[["evl_arg"]] >= cons_i[["min_val"]],
                         "max_val" = cons_i[["evl_arg"]] <= cons_i[["max_val"]],
                         "regex" = grepl(pattern = cons_i[["regex"]],
                                         x = cons_i[["evl_arg"]],
                                         ignore.case = FALSE, perl = TRUE),
                         stop("internal Error, constrian is not defiend: ", what)))
    return(output)
  } else {
    return(TRUE)
  }
}

#' Produce Error Message If an Element doesn't Follow a constrain
#'
#' In case of Constrain Error (i.e. a FALSE returned by
#'   .rba_args_cons_chk()), this function will produce a related error
#'   message.
#'
#' @param cons_i element i from .rba_args()'s cons argument.
#' @param what what constrain produced the error? it should be one of the
#'  possible cons types defined in .rba_args()'s documentations.
#'
#' @return A character string.
#'
#' @family internal_arguments_check
#' @export
.rba_args_cons_msg <- function(cons_i, what) {
  switch(what,
         "class" = sprintf("Invalid Argument; %s should be of class `%s`.\r\n\t(Your provided argument is \"%s\".)",
                           cons_i[["arg"]],
                           .paste2(cons_i[["class"]], last = " or ",
                                   quote = "\""),
                           class(cons_i[["evl_arg"]])),
         "val" = sprintf("Invalid Argument; %s should be either `%s`.\r\n\t(Your provided argument is `%s`.)",
                         cons_i[["arg"]],
                         .paste2(cons_i[["val"]], last = " or ",
                                 quote = "\""),
                         cons_i[["evl_arg"]]),
         "ran" = sprintf("Invalid Argument; %s should be `from %s to %s`.\r\n\t(Your provided argument is `%s`.)",
                         cons_i[["arg"]],
                         cons_i[["ran"]][[1]],
                         cons_i[["ran"]][[2]],
                         cons_i[["evl_arg"]]),
         "len" = sprintf("Invalid Argument; %s should be of length `%s`.\r\n\t(Your provided argument's length is `%s`.)",
                         cons_i[["arg"]],
                         cons_i[["len"]],
                         length(cons_i[["evl_arg"]])),
         "min_len" = sprintf("Invalid Argument; %s should be of minimum length `%s`.\r\n\t(Your provided argument's length is `%s`.)",
                             cons_i[["arg"]],
                             cons_i[["min_len"]],
                             length(cons_i[["evl_arg"]])),
         "max_len" = sprintf("Invalid Argument: %s should be of maximum length `%s`.\r\n\t(Your provided argument's length is `%s`.)",
                             cons_i[["arg"]],
                             cons_i[["max_len"]],
                             length(cons_i[["evl_arg"]])),
         "min_val" = sprintf("Invalid Argument: %s should be equal to or greater than `%s`.\r\n\t(Your provided argument is `%s`.)",
                             cons_i[["arg"]],
                             cons_i[["min_val"]],
                             cons_i[["evl_arg"]]),
         "max_val" = sprintf("Invalid Argument: %s should be equal to or less than `%s`.\r\n\t(Your provided argument is `%s`.)",
                             cons_i[["arg"]],
                             cons_i[["max_val"]],
                             cons_i[["evl_arg"]]),
         "regex" = sprintf("Invalid Argument: %s do not have a valid format.\r\n\t(It should match regex pattern: %s ).",
                           cons_i[["arg"]],
                           cons_i[["regex"]])
  )
}

#' A wrapper to Iterate Constrain Types on a cons' Element
#'
#' Iterates .rba_args_cons_chk() on every defined constrain
#'   for element i of a cons element. and produce an error message if necessary.
#'
#' @param cons_i element i from .rba_args()'s cons argument.
#'
#' @return A character vector with containing the error message for failed
#'   constrains, NA otherwise.
#'
#' @family internal_arguments_check
#' @export
.rba_args_cons_wrp <- function(cons_i) {
  all_cons <- setdiff(names(cons_i), c("arg", "class", "evl_arg"))
  cons_i_errs <- lapply(all_cons,
                        function(x){
                          if (.rba_args_cons_chk(cons_i = cons_i, what = x)) {
                            return(NA)
                          } else {
                            return(.rba_args_cons_msg(cons_i = cons_i, what = x))
                          }
                        })

  cons_i_errs <- unlist(cons_i_errs[which(!is.na(cons_i_errs))])
  return(cons_i_errs)
}


#' Produce Error Message If an Element Doesn't Follow a Constrain
#'
#' In case of Condition Error (i.e. a TRUE returned by evaluating the
#'  defined conditions in cond), this function will produce  a list with:
#'  1- messages that could be used as error or warning, 2- an element named
#'  "warn" that if FALSE, .rba_args() will stop the code
#'  execution with message as error, or if TRUE, issues a warning with that
#'  message.
#'
#' @param cond_i element i from .rba_args()'s cond argument.
#'
#' @return A list containing the messages and warn element to
#'   determine the behaviour of .rba_args().
#'
#' @family A list containing the messages and warn element to
#'   determine the behaviour of .rba_args().
#'
#' @export
.rba_args_cond <- function(cond_i) {
  if (is.call(cond_i[[1]])) {
    cond_i_1 <- eval(cond_i[[1]], envir = parent.frame(3))
  } else if (is.character(cond_i[[1]])) {
    cond_i_1 <- eval(parse(text = cond_i[[1]]), envir = parent.frame(3))
  } else {
    stop("Internal error, the first element in the condition sublist",
         "should be either a charachter or quoted call!", call. = TRUE)
  }
  ## Create an Error message
  if (isTRUE(cond_i_1)) {
    err_obj <- switch(as.character(length(cond_i)),
                      "2" = {
                        if (is.character(cond_i[[2]])) {
                          list(msg = cond_i[[2]],
                               warn = FALSE)
                        } else {
                          list(msg = sprintf("Argument's conditions are not satisfied; `%s` is TRUE.",
                                             as.character(enquote(cond_i[[1]]))[[2]]),
                               warn = isTRUE(cond_i[[2]]))
                        }},
                      "3" = list(msg = cond_i[[2]],
                                 warn = isTRUE(cond_i[[3]])),
                      "1" = list(msg = sprintf("Argument's conditions are not satisfied; `%s` is TRUE.",
                                               as.character(enquote(cond_i[[1]]))[[2]]),
                                 warn = FALSE),
                      stop("Internal error, invalid condition: ",
                           enquote(cond_i[[1]])[[2]], call. = FALSE)
    )
    return(err_obj)
  } else {
    return(NA)}
}

#' Internal user's Arguments Check
#'
#' This function provide a flexible, yet powerful and vigorous arguments check
#'   mechanisms. It can check many properties of input variables and also,
#'   check if a condition holds TRUE.
#'
#' cons Should be a list, and each element of that list should correspond to one
#'   input argument and be a lists with the following format:
#'   \cr list(arg = argument name as character string, constrain type =
#'   constrain value)
#'   e.g. list(arg = "species", class = c("character", "numeric"))
#'   \cr cond should be a list. and each element of that list, should correspond
#'   to one condition. the condition should be a quoted expression (or a
#'   character string), which could be evaluated (or parsed and evaluated) to a
#'   logical TRUE/FALSE object. If that expression is TRUE after the evaluation,
#'   the code execution will be halted (or warning will be issued if
#'   cond_warning = TURE or the last element of coniditon sub-list is
#'   "warn = TRUE ), optionally with a pre-defined error message.
#'   \cr cond's elements possible formats: \enumerate{
#'   \item list(quote(conditional expression))
#'   \item list(quote(conditional expression), "error message if expression
#'   is TRUE")
#'   \item list(quote(conditional expression), "warning message if expression
#'   is TRUE", warn = TRUE)
#'   \item list(quote(conditional expression), warn = TRUE)
#'   }
#'
#' @param cons Define Constrains for input arguments. Currently they may be:
#'   \cr 'class', 'val', 'ran', 'min_val', 'max_val', 'len', 'min_len',
#'   'max_len' and/or 'regex'.
#' @param cond Expression which will be evaluated to TRUE or FALSE.
#' @param cond_warning Should the function produce warning instead of stopping
#'   code execution? alternatively, you could include an element to
#'   any condition sub-list as "warn = TRUE", to only produce warning message
#'   for that condition only.
#'
#' @return NULL. if The arguments check failed, the code execution will be
#'  halted or a warning will be issued.
#'
#' @family internal_arguments_check
#' @export
.rba_args <- function(cons = NULL,
                      cond = NULL,
                      cond_warning = FALSE){
  ### 0 set diagnostics
  diagnostics <- get0("diagnostics", envir = parent.frame())
  if (is.null(diagnostics) || is.na(diagnostics) || !is.logical(diagnostics)) {
    diagnostics <- getOption("rba_diagnostics")
  }
  ### 1.1 append extra arguments which occurs in most functions:
  cons <- .rba_args_opts(cons = cons, what = "cons")
  cond <- .rba_args_opts(cond = cond, what = "cond")

  ### 2 Check Arguments
  errors <- c()
  ## 2.1 check if the provided object can be evaluated
  cons <- lapply(X = cons,
                 FUN = function(cons_i){
                   cons_i[["evl_arg"]] <- try(expr = get(x = cons_i[["arg"]],
                                                         envir = parent.frame(3)),
                                              silent = TRUE)
                   return(cons_i)
                 })
  cons_not_exist <- vapply(X = cons,
                           FUN = function(x) {
                             inherits(x[["evl_arg"]], "try-error")
                           },
                           FUN.VALUE = logical(1))

  if (any(cons_not_exist)) { # some object didn't exist!
    #generate errors
    errors <- append(errors,
                     vapply(X = cons[cons_not_exist],
                            FUN = function(x){
                              regmatches(x, regexpr("(?<= : (\\\n  ){1}).*(?=\\\n)",
                                                    x, perl = TRUE))},
                            FUN.VALUE = character(1)
                     ))
    #remove from cons
    cons <- cons[!cons_not_exist]
  }
  ## 2.2 check class
  class_errs <- lapply(cons,
                       function(x) {
                         if (.rba_args_cons_chk(cons_i = x, what = "class")) {
                           return(NA)
                         } else {
                           return(.rba_args_cons_msg(cons_i = x,
                                                     what = "class"))
                         }
                       })

  if (any(!is.na(class_errs))) {
    errors <- append(errors, unlist(class_errs[!is.na(class_errs)]))
    cons <- cons[is.na(class_errs)] # remove elements with wrong class
  }
  ## 2.3 check other constrains if their class is correct
  other_errs <- lapply(cons, .rba_args_cons_wrp)
  if (any(!is.na(other_errs))) {errors <- append(errors, unlist(other_errs))}
  ## 2.4 Take actions for the errors
  if (length(errors) == 1) {
    stop(errors, call. = diagnostics)
  } else if (length(errors) > 1) {
    error_message <- paste0("\r\n", seq_along(errors), "- ", errors)
    stop(sprintf("The following `%s Errors` was raised during your provided arguments check:",
                 length(errors)),
         error_message,
         call. = diagnostics)
  }

  ### 3 Check relationship between arguments
  if (!is.null(cond)) {
    ## 3.1 check if all conditions are satisfied
    cond_err <- lapply(X = cond, .rba_args_cond)
    cond_err <- cond_err[!is.na(cond_err)]
    if (length(cond_err) > 0) {
      ## 3.2 Generate error message(s) if any
      cond_msg <- NULL
      if (length(cond_err) == 1) {
        cond_msg <- cond_err[[1]][["msg"]]
      } else if (length(cond_err) > 1) {
        cond_msg <- paste0("\r\n", seq_along(cond_err), "- ",
                           vapply(X = cond_err,
                                  FUN = function(x){
                                    x[["msg"]]
                                  },
                                  FUN.VALUE = character(1)),
                           collapse = "")
        cond_msg <- sprintf("The following `%s Conditional issues` were found during your provided arguments check:%s",
                            length(cond_msg),
                            cond_msg)
      }
      ## 3.3 Take actions for the errors
      if (cond_warning == TRUE || all(vapply(X = cond_err,
                                             FUN = function(x){
                                               x[["warn"]]
                                             },
                                             FUN.VALUE = logical(1)))) {
        warning(cond_msg, call. = diagnostics)
      } else {
        stop(cond_msg, call. = diagnostics)}
    }
  }

  invisible()
}

#### Response Parsers ####

#' Parse API Response
#'
#' Using the input provided as 'parser' argument, this function will parse the
#'   response from a REST API into appropriate R objects.
#'
#' The function will be called within .rba_skeleton subsequent of a
#'   server response with HTTP status code 200.
#'   \cr each parser  could be either a single-argument function or
#'   one of the following character strings that will be internally converted
#'   to a proper function:
#'   "json->df", "json->df_no_flat", "json->list_simp", "json->list",
#'   "json->chr", text->chr", "text->df", "tsv->df".
#'   \cr if you provide more than one parser, the parsers will be sequentially
#'   applied to the response (i.e. response %>% parser1 %>% parser2 %>% ...)
#'
#' @param response An httr response object.
#' @param parsers Response parsers, a single value or a vector. Each element
#'   should be either a function with a single argument or a character string.
#'
#' @return A valid R object, depends on the parsers which have been used.
#'
#' @family internal_response_parser
#' @export
.rba_response_parser <- function(response, parsers) {
  if (!is.vector(parsers)) { parsers <- list(parsers)}
  parsers <- sapply(X = parsers,
                    FUN = function(parser){
                      #create a parser if not provided
                      if (!is.function(parser)) {
                        parser <- switch(
                          parser,
                          "json->df" = function(x) {
                            data.frame(jsonlite::fromJSON(httr::content(x,
                                                                        as = "text",
                                                                        encoding = "UTF-8"),
                                                          flatten = TRUE),
                                       stringsAsFactors = FALSE)
                          },
                          "json->df_no_flat" = function(x) {
                            data.frame(jsonlite::fromJSON(httr::content(x,
                                                                        as = "text",
                                                                        encoding = "UTF-8"),
                                                          flatten = FALSE),
                                       stringsAsFactors = FALSE)
                          },
                          "json->list_simp" = function(x) {
                            as.list(jsonlite::fromJSON(httr::content(x,
                                                                     as = "text",
                                                                     encoding = "UTF-8"),
                                                       simplifyVector = TRUE))
                          },
                          "json->list" = function(x) {
                            as.list(jsonlite::fromJSON(httr::content(x,
                                                                     as = "text",
                                                                     encoding = "UTF-8"),
                                                       simplifyVector = FALSE))
                          },
                          "json->chr" = function(x) {
                            as.character(jsonlite::fromJSON(httr::content(x,
                                                                          as = "text",
                                                                          encoding = "UTF-8")))
                          },
                          "text->chr" = function(x) {
                            as.character(httr::content(x,
                                                       as = "text",
                                                       encoding = "UTF-8"))
                          },
                          "text->df" = function(x) {
                            utils::read.table(text = httr::content(x,
                                                                   type = "text/plain",
                                                                   as = "text",
                                                                   encoding = "UTF-8"),
                                              header = FALSE,
                                              stringsAsFactors = FALSE)
                          },
                          "tsv->df" = function(x) {
                            as.character(httr::content(x,
                                                       as = "text",
                                                       encoding = "UTF-8"))
                          },
                          stop("Internal Error: Specify a valid parser name or provide a function!",
                               call. = TRUE)
                        )
                      }
                      return(parser)
                    })

  # sequentially handle the response to the parsers
  for (parser in seq_along(parsers)) {
    response <- do.call(what = parsers[[parser]], args = list(response))
  }
  return(response)
}

#' Parse Appropriate, Server-aware Error Message
#'
#' In case of server response with status code other than 200, this function
#'   will be called from .rba_api_call() and tries to parse the informative
#'   error message which returned by the server as an error message.
#'
#' This function will detect the responded server based on "ptn" values stored
#'   in .rba_stg(). and if that particular servers error format was defined
#'   under "err", the response will be parsed using "err_prs" argument and will
#'   be converted to a character string using "err_prs2" value. (all in
#'   .rba_stg()). if the server was not identified, or the server was not
#'   recorded to have a defined error response, this function will only return
#'   the translation of HTTP status code, using .rba_http_status().
#'
#' @param response a formal api server response, with the class 'response'
#'   from httr package.
#' @param verbose Should the function generate informative messages?
#'
#' @return Character string that contains A server-specific error message or if
#'   not, a human-understandable explanation of the returned HTTP status code.
#'
#' @family internal_response_parser
#' @export
.rba_error_parser <- function(response,
                              verbose = FALSE) {
  ## detect the database name
  dbs <- vapply(X = .rba_stg("db"),
                FUN = function(db) {
                  grepl(.rba_stg(db, "ptn"), response$url,
                        perl = TRUE, ignore.case = TRUE)},
                FUN.VALUE = logical(1)
  )
  db <- names(dbs)[dbs]
  ## parse the error
  if (length(db) == 1 &&
      grepl(.rba_stg(db, "err_ptn"), response$status_code)) {
    ## The API server returns an error string for this status code
    error_message <- tryCatch({
      sprintf(
        "%s server returned \"%s\".\r\n  With this error message:\r\n  \"%s\"",
        .rba_stg(db, "name"),
        .rba_http_status(http_status = response$status_code,
                         verbose = FALSE),
        .rba_response_parser(response = response,
                             parsers = .rba_stg(db, "err_prs"))
      )},
      error = function(e) {
        .rba_http_status(http_status = response$status_code,
                         verbose = verbose)
      })
  } else {
    ## The API server returns only status code with no error string
    error_message <- .rba_http_status(http_status = response$status_code,
                                      verbose = verbose)
  }
  return(error_message)
}
#### Miscellaneous ####
#' Smarter messaging system
#'
#' This function is a more versatile version of message(), and makes the
#'   package's messaging system more minimal to code.
#'
#' By default, the 'fmt' and ... will be passed to sprintf() and the results
#'   will be issued as a message. but, if 'sprintf = FALSE' or, the 'fmt'
#'   argument's string input didn't contain "%s", the function will pass the
#'   the inputs to paste().
#'
#' @param fmt passed to 'fmt' arguments in sprintf() or as the first argument of
#'   paste(), depending on the situation.
#' @param sprintf logical: should the 'fmt' and '...' be passed to sprintf if
#'   possible? set to 'FALSE' to force passing 'fmt' and '...' to paste.
#' @param cond A variable name to be evaluated, and only produce the message
#'   if that variable is 'TRUE'. note: the variable should be of class 'logical'.
#' @param sep,collapse to be passed to paste() if being called.
#' @param ... will be passed to '...' argument of the function sprintf() or
#'   paste().
#'
#' @return NULL, a message will be diplayed if verbose = TRUE
#'
#' @family internal_misc
#' @export
.msg <- function(fmt, ..., sprintf = TRUE, cond = "verbose",
                 sep = "", collapse = NULL) {
  if (isTRUE(get0(cond, envir = parent.frame(1), ifnotfound = FALSE))) {
    message(ifelse(isTRUE(sprintf) &&
                     is.character(fmt) &&
                     grepl("%s", fmt, fixed = TRUE),
                   yes = sprintf(fmt, ...),
                   no = paste(fmt, ..., sep = sep, collapse = collapse)),
            appendLF = TRUE)
  }
  invisible()
}

#' Grammatically Correct Pasting
#'
#' This function will append every element by comma and the last element by
#'   'and'/'or', just like natural and correct english sentence.
#'
#' @param ... words to be appended together.
#' @param last (default: "AND") The separator between the last two words.
#' @param sep The separator between every words except the last two.
#' @param quote Should every word be quoted between a character?
#' @param quote_all Should the final result be quoted between a character?
#'
#' @return A character string of appended words, in a natural english way.
#'
#' @family internal_misc
#' @export
.paste2 <- function(...,
                    last = " and ", sep = ", ",
                    quote = NA, quote_all = NA) {
  input <- c(...)
  len <- length(input)
  if (!is.na(quote)) {
    input <- sprintf("%s%s%s", quote, input, quote)
  }
  if (len > 1) {
    input <- paste(paste0(input[-len], collapse = sep),
                   input[len],
                   sep = last)
  }
  if (!is.na(quote_all)) {
    input <- sprintf("%s%s%s", quote_all, input, quote_all)
  }
  return(input)
}

#' Validate the Provided File Path or Create One
#'
#' Based on the 'save_to' argument, this function will handle different
#'   scenarios for the provided file path. see details for more information.
#'
#' 1- If 'save_to = FALSE': the function will return "FALSE" and no path will be
#'   generated.
#'   \cr 2- If 'save_to = character string': The function will validate the
#'   input, if it is a valid file path, the content of 'save_to' will be
#'   returned. Otherwise, if the provided input is not valid, scenario 3 will be
#'   executed.
#'   \cr 3- If 'save_to = TRUE': A file path will be generated and returned
#'   based on 'dir_name' and 'file' inputs.
#'   \cr Also, in scenario 3, the function will check if any file currently
#'   exists under the generated path. if so, a numeral suffix will be added to
#'   the generated file name in order to prevent over-writing of existing files.
#'
#'
#' @param file A template for the file name and file extension. in form of a
#'   character string: "file_name.file_extension"
#' @param dir_name A directory which will be created in the working environment
#'   as a parent directory of the file.
#' @param save_to logical or character: It is the main switch that dictate the
#'   function's execution. see details.
#'
#' @return FALSE if no file path should be generated or a character string
#'   which is a file path.
#'
#' @family internal_misc
#' @export
.rba_file <- function(file,
                      save_to = NA,
                      dir_name = NA) {
  if (is.na(save_to)) {
    save_to <- get0(x = "save_file",
                    ifnotfound = FALSE,
                    envir = parent.frame(1))
    }
  if (!isFALSE(save_to)) {
    ## 1 file path will be generated unless save_to == FALSE
    # set values
    diagnostics <- get0("diagnostics", envir = parent.frame(1),
                        ifnotfound = getOption("rba_diagnostics"))
    verbose <- get0("verbose", envir = parent.frame(1),
                    ifnotfound = getOption("rba_verbose"))
    # set defaults
    def_file_ext <- regmatches(file, regexpr("(?<=\\.)\\w+?$",
                                             file, perl = TRUE))
    def_file_name <- regmatches(file,
                                regexpr(sprintf("^.*(?=\\.%s$)", def_file_ext),
                                        file, perl = TRUE))
    ## File path is in "save_to", if not in "file = file_name.file_ext"
    if (is.character(save_to)) {
      # 2a the user provided a file path, just check if it is valid
      if (!grepl("^[a-zA-z]:|^\\\\\\w|^/|^\\w+\\.\\w+$", save_to)) {
        ## 2a.1 not a valid file path!
        warning(sprintf("\"%s\" is not a valid file path. Ignored that.",
                        save_to),
                call. = diagnostics)
        save_to <- TRUE
      } else {
        ## 2a.2 the provided file path is valid
        ## 2a.2.1 Does the path end to a directory or file?
        if (!grepl("/$", save_to, perl = TRUE) &&
            grepl("\\S+\\.\\S*", basename(save_to), perl = TRUE)) {
          # 2a.2.1a it's file!
          overwrite <- TRUE
          # extract the file name and extension
          file_ext <- regmatches(basename(save_to),
                                 regexpr("(?<=\\.)\\w+?$",
                                         basename(save_to), perl = TRUE))
          file_name <- regmatches(basename(save_to),
                                  regexpr(sprintf("^.*(?=\\.%s$)", file_ext),
                                          basename(save_to), perl = TRUE))
          # 2a.3 Check if the path and extension agree
          if (!grepl(def_file_ext, file_ext, ignore.case = TRUE)) {
            warning(sprintf("The Response file's type (\"%s\") does not match the extension of your provided file path(\"%s\").",
                            def_file_ext, basename(save_to)),
                    call. = diagnostics)
          }
        } else {
          #2a.2.1b it's directory
          overwrite <- FALSE
          ## append the default file name to the directory path
          file_ext <- def_file_ext
          file_name <- def_file_name
          save_to <- file.path(sub("/$", "", save_to),
                               paste0(file_name, ".", file_ext))
        }
      }
    }
    if (isTRUE(save_to)) {
      ## 2b User didn't provide a file path, use defaults
      overwrite <- FALSE
      ## 2b.1 extract the default file name and extension
      file_ext <- def_file_ext
      file_name <- def_file_name
      ## 2b.2 set directory name
      dir_name <- ifelse(is.na(dir_name),
                         yes = get0("dir_name", envir = parent.frame(1),
                                    ifnotfound = getOption("rba_dir_name")),
                         no = dir_name)
      ## 2b.3 set file path
      save_to <- file.path(getwd(), dir_name, paste0(file_name, ".", file_ext))
    } # end of if is.character(save_to)

    ## 3 now that you have a file path...
    ## 3.1 check if a file doesn't exist with this path
    if (isFALSE(overwrite) && file.exists(save_to)) {
      ## add an incremented file
      exst_files <- list.files(path = dirname(save_to),
                               pattern = sprintf("(^%s)(_\\d+)*(\\.%s$)",
                                                 file_name, file_ext),
                               full.names = FALSE)
      incrt <- regmatches(exst_files,
                          regexpr(sprintf("(?<=^%s_)(\\d+)*(?=\\.%s)",
                                          file_name, file_ext),
                                  exst_files, perl = TRUE))
      if (length(incrt) == 0) {
        incrt <- 1
      } else {incrt <- max(as.numeric(incrt)) + 1}
      save_to <- file.path(dirname(save_to),
                           paste0(file_name, "_", incrt, ".", file_ext))
    } else {
      ## 3.2 file doesn't exist. create the directory just in case
      ### 4 create the directory
      dir.create(dirname(save_to),
                 showWarnings = FALSE,
                 recursive = TRUE)
    }
    .msg("Saving the server response to: \"%s\"", save_to)
  } # end if !isFALSE(save_to)
  return(save_to)
}

#### Options ####
#' Temporary Change rbioapi Options During a Function Call
#'
#' The '...' argument of any exported function will be passed to this function.
#'   It will temporary alter the standard rbioapi options during the caller
#'   function execution.
#'
#' The availabe rbioapi options will be retrieved from
#'   getOption("rba_user_options"). If the name of parameter in '...' is a
#'   standrad rbioapi option, the content of that option will be checked and
#'   in case that the content is valid, the caller function's environment will
#'   be altered in response to the change.
#'   \cr Also the function will ignore any arguments which is not standard and
#'   issues an informative warning for the user.
#'
#' @param ... Extra arguments that were provided in the endpoints functions.
#' @param ignore_save if the function has a dedicated file saving argument,
#'   set this to TRUE.
#'
#' @return NULL, if arguments check failed, code execution will be stopped.
#'   otherwise, nothing will be returned nor displayed.
#'
#' @family internal_options
#' @export
.rba_ext_args <- function(..., ignore_save = FALSE) {
  ext_args <- list(...)
  rba_opts <- getOption("rba_user_options") #available options for the end-users
  if (length(ext_args) > 0) { #user provided something in ...
    unnamed_args <- which(names(ext_args) == "" | is.na(names(ext_args)))
    invalid_args <- setdiff(names(ext_args[-unnamed_args]), rba_opts)
    if (length(c(unnamed_args, invalid_args)) > 0) {
      warning(sprintf("invalid rbioapi options were ignored:%s%s",
                      ifelse(length(unnamed_args) != 0,
                             yes = sprintf("\r\n- %s unnamed argument(s).",
                                           length(unnamed_args)),
                             no = ""),
                      ifelse(length(invalid_args) != 0,
                             yes = sprintf("\r\n- %s",
                                           .paste2(invalid_args,
                                                   last = " and ",
                                                   quote = "\"")),
                             no = "")
      ), call. = FALSE)
      ext_args <- ext_args[-c(unnamed_args,
                              which(names(ext_args) %in% invalid_args))]
    }
    if (isTRUE(ignore_save) && utils::hasName(ext_args, "save_file")) {
      warning("This function has a dedicated file-saving argument, ",
              "'save_file' option was ignored.",
              call. = FALSE)
      rba_opts <- rba_opts[names(rba_opts) != "rba_save_file"]
    }
  } #end of if (length(ext_args) > 0)
  # create option variables
  for (opt in rba_opts) {
    assign(x = opt,
           value = ifelse(utils::hasName(ext_args, opt),
                          yes = ext_args[[opt]],
                          no = getOption(paste0("rba_", opt))),
           envir = parent.frame(1))
  }

  invisible()
}
