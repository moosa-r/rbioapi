##### data containers #######################################################
#' Data container for supported databases
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ba_stg = function(...){
  arg = c(...)
  #possible arguments
  arg_1 = c("db", "enrichr", "ensembl", "reactome", "string", "uniprot", "options", "citations")
  output = switch(match.arg(arg[[1]], arg_1),
                  db = c("enrichr", "ensembl", "reactome", "string", "uniprot"),
                  enrichr = switch(arg[[2]],
                                   name = "Enrichr",
                                   url = "https://amp.pharm.mssm.edu",
                                   pth = "Enrichr/",
                                   ptn = "(http.*://)*amp.pharm.mssm.edu/Enrichr/\\w+.*"),
                  ensembl = switch(arg[[2]],
                                   name = "Ensembl",
                                   url = "https://rest.ensembl.org",
                                   ptn = "(http.*://)*rest.ensembl.org/\\w+.*",
                                   err = c("400","404"),
                                   err_prs = "json->list",
                                   err_fun = function(x) {x[["error"]][[1]]}),
                  reactome = switch(arg[[2]],
                                    name = "Reactome",
                                    url = "https://reactome.org",
                                    pth = switch(match.arg(arg[[3]],
                                                           c("analysis",
                                                             "content")),
                                                 analysis = "AnalysisService/",
                                                 content = "ContentService/"),
                                    ptn = "(http.*://)*reactome.org/(?:AnalysisService|ContentService)/\\w+.*"),
                  string = switch(arg[[2]],
                                  name = "STRING",
                                  url = "https://string-db.org",
                                  pth = "api/",
                                  ptn = "(http.*://)*string-db.org/api/\\w+.*"),
                  uniprot = switch(arg[[2]],
                                   name = "UniProt",
                                   url = "https://www.ebi.ac.uk",
                                   pth = "proteins/api/",
                                   ptn = "(http.*://)*ebi.ac.uk/proteins/api/\\w+.*",
                                   err = c("400", "404"),
                                   err_prs = "json->list",
                                   err_fun = function(x) {x[["errorMessage"]][[1]]}),
                  options = switch(as.character(length(arg)),
                                   "1" = options()[grep("^rba_",
                                                        names(options()))],
                                   getOption(arg[[2]])),
                  citations = switch(arg[[2]],
                                     rbioapi = "Moosa Rezwani (NA). rbioapi: User-friendly interface from R to Biological Databases' APIs. R package version 0.4.0. https://github.com/moosa-r/rbioapi",
                                     r = "R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.",
                                     enrichr = "***enrichr api papeer***",
                                     ensembl = "***ensembl api papeer***",
                                     reactome = "***reactome api papeer***",
                                     string = "***string api papeer***",
                                     uniprot = "***uniprot api papeer***")
                  )
  return(output)
}

##### Internet connectivity ##################################################
#' Handle situations when internet connection is disturbed or http statis 5xx
#' @description changed name from rba_ba_internet_handler
#' @param retry_max
#' @param wait_time
#' @param verbose
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ba_net_handle = function(retry_max = 1,
                             wait_time = 10,
                             verbose = FALSE,
                             diagnostics = FALSE) {
  if (diagnostics == TRUE) {message("Testing the internet connection.")}
  test_call = quote(httr::status_code(httr::HEAD("https://www.google.com/",
                                                 httr::timeout(getOption("rba_client_timeout")),
                                                 if (diagnostics) httr::verbose()
  )))
  net_status = try(eval(test_call),
                   silent = TRUE)
  retry_count = 0

  while (net_status != 200 & retry_count < retry_max) {
    retry_count = retry_count + 1
    if (verbose == TRUE) {
      message(sprintf("No internet connection, waiting for %s seconds and retrying (retry count:  %s/%s)",
                      wait_time,
                      retry_count,
                      retry_max))
    }
    Sys.sleep(wait_time)
    net_status = try(eval(test_call),
                     silent = TRUE)
  } #end of while

  if (net_status == 200) {
    if (diagnostics == TRUE) {message("Device is connected to the internet!")}
  } else {
    stop("No internet connection! Terminating code excutation!",
         call. = diagnostics)
  } #end of if net_test
  return(net_status == 200)
}

#' Test connection with a rest server
#'
#' @param name
#' @param url
#'
#' @return
#' @export
#'
#' @examples
rba_ba_api_check = function(url, diagnostics = FALSE){
  request = quote(httr::HEAD(url = url,
                             httr::timeout(getOption("rba_client_timeout")),
                             httr::user_agent(getOption("rba_user_agent")),
                             if (diagnostics) httr::verbose()
  ))
  test_result = try(httr::status_code(eval(request)),
                    silent = !diagnostics)

  if (is.numeric(test_result)) {
    if (test_result == 200) {
      return("\U2705 The Server is Respoding.")
    } else {
      return(paste("\U274C",
                   rba_ba_http_status(test_result,
                                      verbose = FALSE)))
    }
  } else {
    return(paste("\U274C", test_result))
  }
}

#' Translate HTTP status code to human readable explanation
#' @description changed name from rba_ba_translate
#' @param http_status
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
rba_ba_http_status = function(http_status, verbose = FALSE){
  #ref: iana.org/assignments/http-status-codes/http-status-codes.xhtml
  http_status = as.character(http_status)
  stopifnot(grepl("^[12345]\\d\\d$", http_status))

  resp = switch(substr(http_status, 1, 1),
                "1" = list(class = "Informational",
                           deff = switch(http_status,
                                         "100" = "Continue",
                                         "101" = "Switching Protocols",
                                         "102" = "Processing",
                                         "103" = "Early Hints")),
                "2" = list(class = "Success",
                           deff = switch(http_status,
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
                           deff = switch(http_status,
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
                           deff = switch(http_status,
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
                           deff = switch(http_status,
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

  output = sprintf("HTTP Status '%s'", http_status)
  if (!is.null(resp$deff)) {
    output = paste0(output, sprintf(" (%s: %s)", resp$class, resp$deff))
  } else {
    output = paste0(output, sprintf(" (%s class)", resp$class))
  }
  if (verbose == TRUE) {
    output = sprintf("The server returned %s.", output)
  }
  return(output)
}

##### API Calls ##################################################

#' Add additional parameters to API call's body
#' @description changed the name from rba_ba_body_add_pars
#' the format shoud be 1- init which is a named list, followed by optional
#' lists with the following order: argument name, a condition which may be TRUE,
#' a value that should be appended to the list
#' @param additional_pars
#' @param call_body
#'
#' @return
#' @export
#'
#' @examples
rba_ba_query = function(init, ...) {
  ## check the input method
  ext_par = list(...)
  if (utils::hasName(ext_par, "extra_pars")) {
    ext_par = ext_par$extra_pars
  }
  ## evaluate extra parameters
  for(i in seq_along(ext_par)){
    # check if the condition has more than 1 element
    if (length(ext_par[[i]][[2]]) > 1) {
      warning("Internal Query Builder:\r\n",
              ext_par[[i]][[1]], " has more than one element. ",
              "only the first element will be used.",
              call. = FALSE, immediate. = TRUE)
    }
    # only proceed if the condition is indeed logical
    if (!is.logical(ext_par[[i]][[2]])) {
      warning("Internal Query Builder:\r\n",
              "The evaluation result of ",
              ext_par[[i]][[1]], " is not logical, thus skipping that.",
              call. = FALSE, immediate. = TRUE)
    } else {
      if (ext_par[[i]][[2]][[1]] == TRUE) {
        init[[ext_par[[i]][[1]]]] = ext_par[[i]][[3]]
      }
    }
  } # end of for(i ...
  return(init)
}

#' Build httr HTTP queries
#'
#' @param httr
#' @param url
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ba_httr = function(httr,
                       url = NULL,
                       path = NULL,
                       ...) {
  ## assign global options
  diagnostics = get0("diagnostics", envir = parent.frame(1),
                     ifnotfound = getOption("rba_diagnostics"))
  progress_bar = get0("progress_bar", envir = parent.frame(1),
                      ifnotfound = getOption("rba_progress_bar"))
  client_timeout = get0("client_timeout", envir = parent.frame(1),
                        ifnotfound = getOption("rba_client_timeout"))
  ### 1 capture extra arguments
  # possible args: all args supported by httr +
  # args to this function: [file/obj_]accept, [file/obj_]parser, save_to
  ext_args = list(...)

  ### 2 build main HTTP request (using httr)
  httr_call = list(switch(httr,
                          "get" = quote(httr::GET),
                          "post" = quote(httr::POST),
                          "head" = quote(httr::HEAD),
                          "put" = quote(httr::PUT),
                          "delete" = quote(httr::DELETE),
                          "patch" = quote(httr::PATCH),
                          stop("internal error: what verb to use with httr?",
                               call. = TRUE)),
                   url = as.character(url),
                   path = as.character(path),
                   quote(httr::user_agent(getOption("rba_user_agent"))),
                   quote(httr::timeout(client_timeout))
  )
  if (diagnostics == TRUE) {
    httr_call = append(httr_call, quote(httr::verbose()))
  }
  if (progress_bar == TRUE) {
    httr_call = append(httr_call, quote(httr::progress()))
  }

  ###  3 deal with extra arguments
  if (length(ext_args) >= 1) {
    ### 3.1 check if there is "save to file vs return R object" scenario
    if (sum(utils::hasName(ext_args, "save_to"),
            utils::hasName(ext_args, "file_accept"),
            utils::hasName(ext_args, "obj_accept")) == 3) {
      ## 3.1.a it was up to the  end-user to choose the response type
      if (ext_args$save_to == FALSE) {
        httr_call = append(httr_call,
                           list(str2lang(sprintf("httr::accept(\"%s\")",
                                                 ext_args$obj_accept))))
        if (utils::hasName(ext_args, "obj_parser")) {parser = ext_args$obj_parser}
      } else {
        httr_call = append(httr_call,
                           list(str2lang(sprintf("httr::accept(\"%s\")",
                                                 ext_args$file_accept)),
                                str2lang(sprintf("httr::write_disk(\"%s\", overwrite = TRUE)",
                                                 ext_args$save_to))
                           ))
        if (utils::hasName(ext_args, "file_parser")) {parser = ext_args$file_parser}
      }
    } else {
      ## 3.1.b it was a pre-defined response type
      # accept header?
      if (utils::hasName(ext_args, "accept")) {
        httr_call = append(httr_call,
                           list(str2lang(sprintf("httr::accept(\"%s\")",
                                                 ext_args$accept))))
      }
      # save to file?
      if (utils::hasName(ext_args, "save_to") && ext_args$save_to != FALSE) {
        httr_call = append(httr_call,
                           list(str2lang(sprintf("httr::write_disk(\"%s\", overwrite = TRUE)",
                                                 ext_args$save_to))))
      }
      # parser?
      if (utils::hasName(ext_args, "parser")) {
        parser = ext_args$parser
      } else {
        parser = NULL
      }
    }
    ### remove extra arguments that you don't want in httr function call
    ext_args = ext_args[!grepl("^(?:accept|file_accept|obj_accept|save_to|\\w*parser)$",
                               names(ext_args))]
  } #end of if (length(ext_args...
  httr_call = list(call = as.call(append(httr_call, ext_args)),
                   parser = parser)
  return(httr_call)
}

#' Internal function to make http request
#'
#' @param skip_error
#' @param no_interet_retry_max
#' @param no_internet_wait_time
#' @param verbose
#' @param diagnostics
#' @param input_call
#'
#' @return
#' @export
#'
#' @examples
rba_ba_api_call = function(input_call,
                           skip_error = FALSE,
                           no_interet_retry_max = 1,
                           no_internet_wait_time = 10,
                           verbose = TRUE,
                           diagnostics = FALSE) {
  ## 1 call API
  response = try(eval(input_call, envir = parent.frame(n = 2)),
                 silent = !diagnostics)
  ## 2 check the internet connection & 5xx http status
  if (class(response) != "response" ||
      substr(response$status_code, 1, 1) == "5") {
    ## 2.1 there is an internet connection or server issue
    # wait for the internet connection
    net_connected = rba_ba_net_handle(retry_max = no_interet_retry_max,
                                      wait_time = no_internet_wait_time,
                                      verbose = verbose,
                                      diagnostics = diagnostics)
    if (net_connected == TRUE) {
      ## 2.1.1 net_connection test is passed
      response = try(eval(input_call, envir = parent.frame(n = 2)),
                     silent = !diagnostics)
    } else {
      ## 2.1.2 net_connection test is not passed
      stop("No internet connection! Terminating code excutation!",
           call. = diagnostics)
    }
  } # end of step 2

  ## 3 Decide what to return
  if (class(response) != "response") {
    ## 3.1 errors un-related to server's response
    error_message = response
    if (skip_error == TRUE) {
      return(error_message, call. = diagnostics)
    } else {
      stop(error_message, call. = diagnostics)
    }
  } else if (as.character(response$status_code) != "200") {
    ## 3.2 API call was not successful
    error_message = rba_ba_error_parser(response = response, verbose = verbose)
    if (skip_error == TRUE) {
      return(error_message)
    } else {
      stop(error_message, call. = diagnostics)
    }
  } else {
    ## 3.3 Everything is OK (HTTP status == 200)
    return(response)
  }
}

#' General skeleton for all functions in the package
#'
#' @param no_interet_retry_max
#' @param no_internet_wait_time
#' @param response_parser
#' @param skip_error
#' @param input_call
#'
#' @return
#' @export
#'
#' @examples
rba_ba_skeleton = function(input_call,
                           response_parser = NULL) {
  ## 0 assign options variables
  diagnostics = get0("diagnostics", envir = parent.frame(1),
                     ifnotfound = getOption("rba_diagnostics"))
  verbose = get0("verbose", envir = parent.frame(1),
                 ifnotfound = getOption("rba_verbose"))
  max_retries = get0("max_retries", envir = parent.frame(1),
                     ifnotfound = getOption("rba_max_retries"))
  wait_time = get0("wait_time", envir = parent.frame(1),
                   ifnotfound = getOption("rba_wait_time"))
  skip_error = get0("skip_error", envir = parent.frame(1),
                    ifnotfound = getOption("rba_skip_error"))
  ## 1 Make API Call
  response = rba_ba_api_call(input_call = input_call$call,
                             skip_error = skip_error,
                             no_interet_retry_max = max_retries,
                             no_internet_wait_time = wait_time,
                             verbose = verbose,
                             diagnostics = diagnostics)
  ## 2 Parse the the response if possible
  # Parser Provided via rba_ba_skeleton's 'response parser' argument will
  # override the 'parser' provided in input call
  if (!is.null(response_parser)) {
    parser_input = response_parser
  } else {
    parser_input = input_call$parser
  }

  if (class(response) == "response" && !is.null(parser_input)) {
    final_output = rba_ba_response_parser(parser = parser_input)
  } else {
    final_output = response
  }

  ## 3 Return the output
  return(final_output)
}


#### Check Arguments #######

#' Check provided Arguments
#'
#' @param cons
#' @param cond_warning
#' @param cond
#'
#' @return
#' @export
#'
#' @examples
rba_ba_args = function(cons = NULL,
                       cond = NULL,
                       cond_warning = FALSE){
  # per each argument,the function input "cons" should be a named
  # sub-list with one of these members:
  # arg, class, val, range, min_val, max_val, len, min_len, max_len,
  # example:
  # list(arg = progress_bar, name = "progress_bar", class = "logical")

  # "cond" should be sub-list containing an expression to be evaluated
  # and an optional error message.
  # example:
  # list(list(quote(genes < background),
  #           "Provided genes length cannot be greater than the background")
  #      )

  ### 0 set diagnostics
  diagnostics = get0("diagnostics",
                     envir = parent.frame())
  if (is.null(diagnostics) || is.na(diagnostics) || !is.logical(diagnostics)) {
    diagnostics = getOption("rba_diagnostics")
  }

  ### 1 append extra arguments which occurs in most functions:
  ## all available options to the users
  ext_cons = list(client_timeout = list(arg = "client_timeout",
                                        class = "numeric",
                                        len = 1,
                                        min_val = 0.1),
                  dir_name = list(arg = "dir_name",
                                  class = "character",
                                  len = 1),
                  diagnostics = list(arg = "diagnostics",
                                     class = "logical",
                                     len = 1),
                  max_retries = list(arg = "max_retries",
                                     class = "numeric",
                                     len = 1),
                  progress_bar = list(arg = "progress_bar",
                                      class = "logical",
                                      len = 1),
                  save_resp_file = list(arg = "save_resp_file",
                                        class = c("logical",
                                                  "character"),
                                        len = 1),
                  skip_error = list(arg = "skip_error",
                                    class = "logical",
                                    len = 1),
                  verbose = list(arg = "verbose",
                                 class = "logical",
                                 len = 1),
                  wait_time = list(arg = "wait_time",
                                   class = "numeric",
                                   len = 1,
                                   min_val = 1))
  ext_cond = list(dir_name = list(quote(grepl("[\\\\/:\"*?<>|]+", dir_name, perl = TRUE)),
                                  "Invalid dir_name. directory name can not include these characters: \\/?%*:|<>"),
                  save_resp_file = list(quote(!is.logical(save_resp_file) &&
                                                !grepl("^[a-zA-z]:|^\\\\\\w|^/|\\w+\\.\\w+$",
                                                       save_resp_file)),
                                        "Invalid save_resp_file. you should set this argument as logical or a valid file path."))
  rba_opts = getOption("rba_user_options")
  stopifnot(setequal(rba_opts, names(ext_cons)))

  ## only keep the provided options (e.g. extra arguments)
  exist_opts = rba_opts[which(rba_opts %in% ls(envir = parent.frame(1)))]
  ext_cond = unname(ext_cond[names(ext_cond) %in% exist_opts])
  ext_cons = unname(ext_cons[exist_opts])
  ## append
  cons = append(ext_cons, cons)
  cond = append(ext_cond, cond)
  ### 2 Check arguments
  errors = c()
  ## 2.1 check for errors
  for (i in seq_along(cons)){
    # evaluate the i'th argument's constrains sub-list
    cons_i = cons[[i]]
    arg_name = cons_i[["arg"]]

    arg = try(eval(parse(text = arg_name), envir = parent.frame(1)),
              silent = TRUE)
    if (class(arg) == "try-error") {
      ## try to prettify the error message
      pretty_error = regmatches(arg,
                                regexpr("(?<= : (\\\n  ){1}).*(?=\\\n)",
                                        arg, perl = TRUE))
      errors = append(errors,
                      ifelse(length(pretty_error) == 0, # no regex match!
                             arg,
                             pretty_error))
    } else {
      # only check if the provided argument is not NA or Null
      if (!all(is.na(arg)) & !all(is.null(arg))) {
        # check class
        if (utils::hasName(cons_i, "class") &&
            !class(arg) %in% cons_i[["class"]]) {
          errors = append(errors,
                          sprintf("Invalid Argument; %s should be of class \"%s\".\r\n\t(Your provided argument is \"%s\".)",
                                  arg_name,
                                  paste0(cons_i[["class"]], collapse = " or "),
                                  class(arg)))
        } else {
          ## only continue checking if the class is correct
          # check for allowed values
          if (utils::hasName(cons_i, "val") &&
              !all(arg %in% cons_i[["val"]])) {
            errors = append(errors,
                            sprintf("Invalid Argument; %s should be either `%s`.\r\n\t(Your provided argument is `%s`.)",
                                    arg_name,
                                    paste0(cons_i[["val"]], collapse = " or "),
                                    arg))
          }
          # check for allowed range
          if (utils::hasName(cons_i, "ran") &&
              !all(arg >= cons_i[["ran"]][[1]] &
                   arg <= cons_i[["ran"]][[2]])) {
            errors = append(errors,
                            sprintf("Invalid Argument; %s should be `from %s to %s`.\r\n\t(Your provided argument is `%s`.)",
                                    arg_name,
                                    cons_i[["ran"]][[1]],
                                    cons_i[["ran"]][[2]],
                                    arg))
          }
          # check length
          if (utils::hasName(cons_i, "len") &&
              length(arg) != cons_i[["len"]]) {
            errors = append(errors,
                            sprintf("Invalid Argument; %s should be of length `%s`.\r\n\t(Your provided argument's length is `%s`.)",
                                    arg_name,
                                    cons_i[["len"]],
                                    length(arg)))
          }
          # check minimum length
          if (utils::hasName(cons_i, "min_len") &&
              length(arg) < cons_i[["min_len"]]) {
            errors = append(errors,
                            sprintf("Invalid Argument; %s should be of minimum length `%s`.\r\n\t(Your provided argument's length is `%s`.)",
                                    arg_name,
                                    cons_i[["min_len"]],
                                    length(arg)))
          }
          # check maximum length
          if (utils::hasName(cons_i, "max_len") &&
              length(arg) > cons_i[["max_len"]]) {
            errors = append(errors,
                            sprintf("Invalid Argument: %s should be of maximum length `%s`.\r\n\t(Your provided argument's length is `%s`.)",
                                    arg_name,
                                    cons_i[["max_len"]],
                                    length(arg)))
          }
          # check minimum value
          if (utils::hasName(cons_i, "min_val")
              && arg < cons_i[["min_val"]]) {
            errors = append(errors,
                            sprintf("Invalid Argument: %s should be equal to or greater than `%s`.\r\n\t(Your provided argument is `%s`.)",
                                    arg_name,
                                    cons_i[["min_val"]],
                                    arg))
          }
          # check maximum value
          if (utils::hasName(cons_i, "max_val")
              && arg > cons_i[["max_val"]]) {
            errors = append(errors,
                            sprintf("Invalid Argument: %s should be equal to or less than `%s`.\r\n\t(Your provided argument is `%s`.)",
                                    arg_name,
                                    cons_i[["max_val"]],
                                    arg))
          }
        } # end of if (utils::hasName(cons_i, "class") &&...
      } # end of if (!all(is.na(arg)) & !all(is.null(arg)))
    } #end of if (class(arg) == "try-error")
  } # end of for (i in seq_along(cons))
  ## 2.2 take actions for the errors
  if (length(errors) == 1) {
    stop(errors, call. = diagnostics)
  } else if (length(errors) > 1) {
    error_message = paste0("\r\n", seq_along(errors), "- ", errors)
    stop(sprintf("The following `%s Errors` was raised during your provided argument's check:",
                 length(errors)),
         error_message,
         call. = diagnostics)
  }

  ### Check relationship between arguments
  if (!all(is.null(cond))) {
    cond_errors = c()
    for (i in seq_along(cond)) {
      cond_i = cond[[i]]
      # 3.1.1 evaluate the expression
      if (is.call(cond_i[[1]])) {
        cond_i_1 = eval(cond_i[[1]], envir = parent.frame(1))
      } else if (is.character(cond_i[[1]])) {
        cond_i_1 = eval(parse(text = cond_i[[1]]), envir = parent.frame(1))
      } else {
        stop("Internal error, the first element in the condition sublist",
             "should be either a charachter or quoted call!", call. = TRUE)
      }
      # 3.1.2 check if the expression is TRUE
      if (cond_i_1 == TRUE){
        #add the error message if existed
        cond_i_error = ifelse(length(cond_i) > 1,
                              yes = cond_i[[2]],
                              no = sprintf("Argument's conditions are not satisfied; `%s` is TRUE.",
                                           as.character(cond_i[[1]])))
        cond_errors = append(cond_errors, cond_i_error)
      }
    } #end of for (i in seq_along(cond))

    # 3.2 produce the message
    if (length(cond_errors) > 0) {
      if (length(cond_errors) == 1) {
        cond_message = cond_errors
      } else if (length(cond_errors) > 1) {
        cond_message = paste0("\r\n", seq_along(cond_errors), "- ", cond_errors, collapse = "")
        cond_message = sprintf("The following `%s Conditional Errors` was raised during your provided argument's check:%s",
                               length(cond_message),
                               cond_message)
      }
      # 3.3 stop or warn!
      if (cond_warning == TRUE) {
        warning(cond_message, call. = diagnostics)
      } else { stop(cond_message, call. = diagnostics) }
    }
  } # end of if (!all(is.null(cond)))
  invisible()
}

#### Response Parsers ####

#' Parse API Response
#'
#' @param parser
#'
#' @return
#' @export
#'
#' @examples
rba_ba_response_parser = function(parser) {
  #create a parser if not provided
  if (!is.call(parser)) {
    parser = switch(parser,
                    "json->df" = quote(data.frame(jsonlite::fromJSON(httr::content(response,
                                                                                   as = "text",
                                                                                   encoding = "UTF-8"),
                                                                     flatten = TRUE),
                                                  stringsAsFactors = FALSE)),
                    "json->list" = quote(as.list(jsonlite::fromJSON(httr::content(response,
                                                                                  as = "text",
                                                                                  encoding = "UTF-8"),
                                                                    simplifyVector = TRUE))),
                    "json->list_no_simp" = quote(as.list(jsonlite::fromJSON(httr::content(response,
                                                                                          as = "text",
                                                                                          encoding = "UTF-8"),
                                                                            simplifyVector = FALSE))),
                    "json->chr" = quote(as.character(jsonlite::fromJSON(httr::content(response,
                                                                                      as = "text",
                                                                                      encoding = "UTF-8")))),
                    "text->chr" = quote(as.character(httr::content(response,
                                                                   as = "text",
                                                                   encoding = "UTF-8"))),
                    "text->df" = quote(read.table(text = httr::content(response,
                                                                       type = "text/plain",
                                                                       as = "text",
                                                                       encoding = "UTF-8"),
                                                  header = FALSE,
                                                  stringsAsFactors = FALSE)),
                    "tsv->df" = quote(as.character(httr::content(response,
                                                                 as = "text",
                                                                 encoding = "UTF-8"))),
                    stop("Internal Error: Specify the valid parser expression!", call. = TRUE)
    )
  }
  # parse the response
  output = eval(parser, envir = parent.frame(1))
  return(output)
}

#' Try to Parse an appropriate error response
#'
#' @param response
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
rba_ba_error_parser = function(response,
                               verbose = verbose) {
  ## detect the database name
  db_found = FALSE
  for (db in rba_ba_stg("db")) {
    if (grepl(rba_ba_stg(db, "ptn"), response$url)) {
      db_found = TRUE
      break
    }
  }
  if (db_found == TRUE &&
      as.character(response$status_code) %in% rba_ba_stg(db, "err")) {
    ## The API server returns an error string for this status code
    server_error = rba_ba_response_parser(rba_ba_stg(db, "err_prs"))
    server_error = rba_ba_stg(db, "err_fun")(server_error)
    error_message = sprintf("%s server returned \"%s\".\r\n  with this error message:\r\n  \"%s\"",
                            rba_ba_stg(db, "name"),
                            rba_ba_http_status(response$status_code,
                                               verbose = FALSE),
                            server_error)
  } else {
    ## The API server returns only status code with no error string
    error_message = rba_ba_http_status(response$status_code,
                                       verbose = verbose)
  }
  return(error_message)
}
#### Miscellaneous ####
#' Alternative messaging system
#'
#' @param ...
#' @param fmt
#' @param sprintf
#' @param cond
#' @param sep
#' @param collapse
#'
#' @return
#' @export
#'
#' @examples
v_msg = function(fmt, ..., sprintf = TRUE, cond = "verbose", sep = "", collapse = NULL) {
  if (get0(cond, envir = parent.frame(1), ifnotfound = FALSE) == TRUE) {
    message(ifelse(sprintf == TRUE && is.character(fmt) && grepl("%s", fmt),
                   yes = sprintf(fmt, ...),
                   no = paste(fmt, ..., sep = sep, collapse = collapse)),
            appendLF = TRUE)
  }
  invisible()
}

#' check provided file address or create one
#'
#' @param dir_name
#' @param save_to
#' @param file
#'
#' @return
#' @export
#'
#' @examples
rba_ba_file = function(file,
                       save_to = NA,
                       dir_name = NA) {
  if (is.na(save_to)) {save_to = get0(x = "save_resp_file",
                                      ifnotfound = FALSE,
                                      envir = parent.frame(1))}
  if (save_to != FALSE) {
    ## 1 file path will be generated unless save_to == FALSE
    # set values
    diagnostics = get0("diagnostics", envir = parent.frame(1),
                       ifnotfound = getOption("rba_diagnostics"))
    verbose = get0("verbose", envir = parent.frame(1),
                   ifnotfound = getOption("rba_verbose"))
    # set defaults
    def_file_ext = regmatches(file, regexpr("(?<=\\.)\\w+?$",
                                            file, perl = TRUE))
    def_file_name = regmatches(file,
                               regexpr(sprintf("^.*(?=\\.%s$)", def_file_ext),
                                       file, perl = TRUE))
    ## File path is in "save_to", if not in "file = file_name.file_ext"
    if (is.character(save_to)) {
      # 2a the user provided a file path, just check if it is valid
      if (!grepl("^[a-zA-z]:|^\\\\\\w|^/|\\w+\\.\\w+$", save_to)) {
        ## 2a.1 not a valid file path!
        warning(sprintf("\"%s\" is not a valid file path. Ignored that.",
                        save_to))
        save_to = TRUE
      } else {
        ## 2a.2 the provided file path is valid
        overwrite = TRUE
        # extract the file name and extension
        file_ext = regmatches(basename(save_to),
                              regexpr("(?<=\\.)\\w+?$",
                                      basename(save_to), perl = TRUE))
        file_name = regmatches(basename(save_to),
                               regexpr(sprintf("^.*(?=\\.%s$)", file_ext),
                                       basename(save_to), perl = TRUE))
        # 2a.3 Check if the path and extension agree
        if (!grepl(def_file_ext, file_ext, ignore.case = TRUE)) {
          warning(sprintf("Your requested file format (\"%s\") does not match your provided file address's extention(\"%s\").",
                          def_file_ext, basename(save_to)),
                  call. = diagnostics)
        }
      }
    } else if (save_to == TRUE){
      ## 2b User didn't provide a file path, use defaults
      overwrite = FALSE
      ## 2b.1 extract the default file name and extension
      file_ext = def_file_ext
      file_name = def_file_name
      ## 2b.2 set directory name
      dir_name = ifelse(is.na(dir_name),
                        yes = get0("dir_name", envir = parent.frame(1),
                                   ifnotfound = getOption("rba_dir_name")),
                        no = dir_name)
      ## 2b.3 set file path
      save_to = file.path(getwd(), dir_name, paste0(file_name, ".", file_ext))
    } # end of if is.character(save_to)

    ## 3 now that you have a file path...
    ## 3.1 check if a file doesn't exist with this path
    if (overwrite == FALSE &&
        file.exists(save_to)) {
      ## add an incremented file
      exst_files = list.files(path = dirname(save_to),
                              pattern = sprintf("(^%s)(_\\d+)*(\\.%s$)",
                                                file_name, file_ext))
      incrt = regmatches(exst_files,
                         regexpr(sprintf("(?<=^%s_)(\\d+)*(?=\\.%s)",
                                         file_name, file_ext),
                                 exst_files, perl = TRUE))
      if (length(incrt) == 0) { incrt = 1
      } else { incrt = max(as.numeric(incrt)) + 1 }
      save_to = file.path(getwd(), dir_name,
                          paste0(file_name, "_", incrt, ".", file_ext))
    } else {
      ## 3.2 file doesn't exist. create the directory just in case
      ### 4 create the directory
      dir.create(dirname(save_to),
                 showWarnings = diagnostics,
                 recursive = TRUE)
    }
    if (verbose == TRUE) {message(sprintf("Saving the server response to: \"%s\"",
                                          save_to))}
  } # end if save_to != FALSE
  return(save_to)
}

#### Options ####
#' Temporary change a rbioapi option during a function call
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ba_ext_args = function(...) {
  ext_args = list(...)
  rba_opts = getOption("rba_user_options") # available options for the end-users
  if (length(ext_args) > 0) {
    # did the user provided non valid arguments?
    non_valid = setdiff(names(ext_args), rba_opts)
    if (is.null(names(ext_args)) | any(non_valid == "")) {
      warning("You provided unnamed extra arguments, thus were ignored.",
              call. = FALSE)
      non_valid = non_valid[which(non_valid != "")]
    }
    if (length(non_valid) > 0) {
      warning(sprintf("`%s` are not valid rbioapi options, thus were ignored.\r\n",
                      paste(non_valid, collapse = " & "),
                      call. = FALSE))
    }
  }
  # create the objects in the calling function's environment
  for (opt in rba_opts) {
    assign(x = opt,
           value = ifelse(utils::hasName(ext_args, opt),
                          yes = ext_args[[opt]],
                          no = getOption(paste0("rba_", opt))),
           envir = parent.frame(1))
  }
  invisible()
}
