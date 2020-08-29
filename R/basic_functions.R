##### Internet connectivity ##################################################
#' Handle situations when internet connection is disturbed
#'
#' @param retry_max
#' @param wait_time
#' @param verbose
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ba_internet_handler = function(retry_max = 1,
                                   wait_time = 10,
                                   verbose = FALSE,
                                   diagnostics = FALSE) {
  if (verbose == TRUE) {message("Testing the internet connection.")}

  net_status = try(httr::status_code(httr::HEAD("https://www.google.com/",
                                                if (diagnostics)
                                                  httr::verbose())),
                   silent = TRUE)



  retry_count = 0
  while (net_status != 200 & retry_count < retry_max) {
    retry_count = retry_count + 1

    if (verbose == TRUE) {message("No internet connection, waiting for ",
                                  wait_time,
                                  " seconds and retrying (retry count: ",
                                  retry_count, "/",
                                  retry_max, ")")}

    Sys.sleep(wait_time)
    net_status = try(httr::status_code(httr::HEAD("https://www.google.com/",
                                                  if (diagnostics)
                                                    httr::verbose())),
                     silent = TRUE)

  } #end of while

  if (net_status == 200) {
    if (verbose == TRUE) {message("Device is connected to the internet!")}
  } else {
    stop("No internet connection! Terminating code excutation!",
         call. = diagnostics)
  } #end of if net_test
  return(net_status == 200)
} # end of function

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
  if (diagnostics == TRUE){
    test_result = try(httr::status_code(httr::HEAD(url,
                                                   httr::user_agent(getOption("rba_ua")),
                                                   httr::verbose())),
                      silent = !diagnostics)
  } else {
    test_result = try(httr::status_code(httr::HEAD(url,
                                                   httr::user_agent(getOption("rba_ua"))
    )),
    silent = !diagnostics)
  }
  if (is.numeric(test_result)) {
    if (test_result == 200) {
      return("\U2705 The Server is Respoding.")
    } else {
      return(paste("\U274C",
                   rba_ba_translate(test_result,
                                    verbose = FALSE)))
    }

  } else {
    return(paste("\U274C", test_result))
  }
}

#' Test if The supported servers are Responding
#'
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_connection_test = function(diagnostics = FALSE) {
  message("Checking Your connection to the Databases",
          " Currently Supported by rbioapi:")

  urls = list("STRING" = paste0(getOption("rba_url_string"),
                                "/api/json/version"),
              "Enrichr" = paste0(getOption("rba_url_enrichr"),
                                 "/Enrichr"),
              "Ensembl" = paste0(getOption("rba_url_ensembl"),
                                 "/info/ping"),
              "Reactome_ContentService" = paste0(getOption("rba_url_reactome"),
                                                 "/ContentService/data/database/name"),
              "Reactome_AnalysisService" = paste0(getOption("rba_url_reactome"),
                                                  "/AnalysisService/database/name"),
              "UniProt" = paste0(getOption("rba_url_uniprot"),
                                                  "/proteins/api/proteins/P25445")
  )

  cat("-", "Internet", ":\r\n")
  google = try(httr::status_code(httr::HEAD("https://www.google.com/",
                                            if (diagnostics) httr::verbose(),
                                            httr::user_agent(getOption("rba_ua"))
  )
  ), silent = TRUE)

  if (google == 200) {
    cat("\U2705 Connected to the Internet.\r\n")
  } else {
    cat("\U274C No Internet Connection.\r\n")
    stop("Could not resolve google.com", " . Check Your internet Connection.",
         call. = diagnostics)
  }

  for (i in seq_along(urls)) {
    cat("-", names(urls)[[i]], ":\r\n")
    cat(rba_ba_api_check(urls[[i]], diagnostics = diagnostics), "\r\n")
  }
  invisible()

}

#' Translate HTTP status code to human readable explanation
#'
#' @param http_status
#'
#' @return
#' @export
#'
#' @examples
rba_ba_translate = function(http_status, verbose = TRUE){
  #source: Wikipedia (https://en.wikipedia.org/wiki/List_of_HTTP_status_codes)
  #1xx Informational response
  #2xx Success
  #3xx Redirection
  #4xx Client errors
  #5xx Server errors
  status_dictionary = c("100" = "Continue",
                        "101" = "Switching Protocols",
                        "102" = "Processing",
                        "103" = "Early Hints",

                        "200" = "OK",
                        "201" = "Created",
                        "202" = "Accepted",
                        "203" = "Non-Authoritative Information",
                        "204" = "No Content",
                        "205" = "Reset Content",
                        "206" = "Partial Content",
                        "207" = "Multi-Status",
                        "208" = "Already Reported",
                        "226" = "IM Used",

                        "300" = "Multiple Choices",
                        "301" = "Moved Permanently",
                        "302" = "Found",
                        "303" = "See Other",
                        "304" = "Not Modified",
                        "305" = "Use Proxy",
                        "306" = "Switch Proxy",
                        "307" = "Temporary Redirect",
                        "308" = "Permanent Redirect",

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
                        "418" = "I'm a teapot",
                        "421" = "Misdirected Request",
                        "422" = "Unprocessable Entity",
                        "423" = "Locked",
                        "424" = "Failed Dependency",
                        "425" = "Too Early",
                        "426" = "Upgrade Required",
                        "428" = "Precondition Required",
                        "429" = "Too Many Requests",
                        "431" = "Request Header Fields Too Large",
                        "451" = "Unavailable For Legal Reasons",

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
                        "511" = "Network Authentication Required"
  )

  if (verbose == TRUE) {
    if (as.character(http_status) %in% names(status_dictionary)){
      output = paste0("The server returned HTTP status ",
                      http_status, " (",
                      status_dictionary[as.character(http_status)], ")")

    } else {
      output = paste0("The server returned HTTP status ", http_status)
    }
  } else {
    if (as.character(http_status) %in% names(status_dictionary)){
      output = paste0("HTTP status ",
                      http_status, " (",
                      status_dictionary[as.character(http_status)], ")")

    } else {
      output = paste0("HTTP status ", http_status)
    }
  }
  return(output)
} # end of function

##### API Calls ##################################################

#' Parse API Response
#'
#' @param type
#' @param parser
#'
#' @return
#' @export
#'
#' @examples
rba_ba_response_parser = function(type = NA, parser = NULL) {
  #create a parser if not provided
  if (is.null(parser)) {
    if (type == "json->df") {
      parser = quote(data.frame(jsonlite::fromJSON(httr::content(response,
                                                                 as = "text",
                                                                 encoding = "UTF-8"),
                                                   flatten = TRUE),
                                stringsAsFactors = FALSE))
    } else if (type == "json->list") {
      parser = quote(as.list(jsonlite::fromJSON(httr::content(response,
                                                              as = "text",
                                                              encoding = "UTF-8"),
                                                simplifyVector = TRUE)
      ))
    } else if (type == "json->list_no_simp") {
      parser = quote(as.list(jsonlite::fromJSON(httr::content(response,
                                                              as = "text",
                                                              encoding = "UTF-8"),
                                                simplifyVector = FALSE)
      ))
    } else if (type == "json->chr") {
      parser = quote(as.character(jsonlite::fromJSON(httr::content(response,
                                                                   as = "text",
                                                                   encoding = "UTF-8")
      )))
    } else if (type == "text->chr") {
      parser = quote(as.character(httr::content(response,
                                                as = "text",
                                                encoding = "UTF-8")
      ))
    } else if (type == "text->df") {
      parser = quote(read.table(text = httr::content(response,
                                                     type = "text/plain",
                                                     as = "text",
                                                     encoding = "UTF-8"),
                                header = FALSE,
                                stringsAsFactors = FALSE)
      )
    } else if (type == "tsv->df") {
      parser = quote(as.character(httr::content(response,
                                                as = "text",
                                                encoding = "UTF-8")
      ))
    } else {
      stop("Internal Error: Specify the parser expression!", call. = TRUE)
    }
  }

  # parse the response
  output = eval(parser, envir = parent.frame())
  return(output)
}

#' Create/check provided file address
#'
#' @param file_ext
#' @param file_name
#' @param dir_name
#' @param save_to
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ba_file_path = function(file_ext,
                            file_name = NA,
                            dir_name = NA,
                            save_to = NA,
                            verbose = TRUE,
                            diagnostics = FALSE) {
  ## only create file path if now save_to argument was provided
  if (is.na(save_to)){
    file_adrs = paste0(file_name, ".", file_ext)
    save_to = file.path(getwd(), dir_name, file_adrs)
    message("No save path was provided.",
            " Saving to:\r\n", save_to)
  } else {
    if (!grepl(pattern = paste0("\\.", file_ext, "$"),
               x = save_to, ignore.case = TRUE)) {
      warning("Your requested file format (", file_ext, ")",
              " does not match your provided file address's extention.",
              " (", basename(save_to), ")",
              call. = diagnostics)
    }
    if (verbose == TRUE) {
      message("Saving to:\r\n", save_to)
    }
  } # end of if is.na(save_to)
  dir.create(dirname(save_to), showWarnings = diagnostics, recursive = TRUE)
  return(save_to)
}
#' Internal function to make http request
#'
#' @param call_function
#' @param skip_error
#' @param no_interet_retry_max
#' @param no_internet_wait_time
#' @param verbose
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ba_api_call = function(call_function,
                           skip_error = FALSE,
                           no_interet_retry_max = 1,
                           no_internet_wait_time = 10,
                           verbose = TRUE,
                           diagnostics = FALSE) {
  ## 1 call API
  response = try(eval(call_function, envir = parent.frame(n = 2)),
                 silent = !diagnostics)
  ## 2 check the internet connection & 5xx http status
  if (class(response) != "response" ||
      any(grep("^5\\d\\d", response$status_code))) {
    ## 2.1 there is an internet connection or server issue
    # wait for the internet connection
    net_connected = rba_ba_internet_handler(retry_max = no_interet_retry_max,
                                            wait_time = no_internet_wait_time,
                                            verbose = verbose,
                                            diagnostics = diagnostics)
    if (net_connected == TRUE) {
      ## 2.1.1 net_connection test is passed
      response = try(eval(call_function, envir = parent.frame(n = 2)),
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
    stop(response, call. = diagnostics)
  } else if (as.character(response$status_code) != "200") {
    ## 3.2 API call was not successful
    if (skip_error == TRUE) {
      return(rba_ba_translate(response$status_code, verbose = FALSE))
    } else {
      stop(rba_ba_translate(response$status_code, verbose = TRUE),
           call. = diagnostics)
    }
  } else {
    ## 3.3 Everything is OK (HTTP status == 200)
    return(response)
  }

}

#' General skeleton for all functions in the package
#'
#' @param call_function
#' @param no_interet_retry_max
#' @param no_internet_wait_time
#' @param verbose
#' @param response_parser
#' @param diagnostics
#' @param parser_type
#' @param user_agent
#' @param progress_bar
#'
#' @return
#' @export
#'
#' @examples
rba_ba_skeletion = function(call_function,
                            batch_mode = FALSE,
                            response_parser = NULL,
                            parser_type = NA,
                            skip_error = FALSE,
                            user_agent = FALSE,
                            progress_bar = FALSE,
                            verbose = TRUE,
                            diagnostics = FALSE,
                            no_interet_retry_max = getOption("max_retry"),
                            no_internet_wait_time = getOption("wait_time")) {
  ## 1 Build API Call expression
  call_function = as.list(call_function)
  if (diagnostics == TRUE) {
    call_function = append(call_function,
                           quote(httr::verbose()))
  }
  if (progress_bar == TRUE) {
    call_function = append(call_function,
                           quote(httr::progress()))
  }
  if (user_agent == TRUE) {
    call_function = append(call_function,
                           quote(httr::user_agent(getOption("rba_ua"))))
  }
  call_function = as.call(call_function)

  ## 2 Make API Call
  response = rba_ba_api_call(call_function = call_function,
                             skip_error = skip_error,
                             no_interet_retry_max = no_interet_retry_max,
                             no_internet_wait_time = no_internet_wait_time,
                             verbose = verbose,
                             diagnostics = diagnostics)

  ## 3 Parse the the response if possible
  if (class(response) == "response") {
    final_output = rba_ba_response_parser(type = parser_type,
                                          parser = response_parser)
  } else {
    final_output = response
  }

  ## 4 Return the output
  return(final_output)
} # end of function

#' A wrapper for General skeleton for long inputs
#'
#' @return
#' @export
#'
#' @examples
rba_ba_multi_batch = function(){
  ############ TODO

} # end of function

#' Add additional parameters to API call's body
#'
#' @param additional_pars
#' @param call_body
#'
#' @return
#' @export
#'
#' @examples
rba_ba_body_add_pars = function(call_body, additional_pars) {
  for(i in seq_along(additional_pars)){
    if (additional_pars[[i]][[1]] == TRUE) {
      call_body = append(call_body, additional_pars[[i]][[2]])
    }
  }
  return(call_body)
}

#### Check Arguments #######

#' Check provided Arguments
#'
#' @param cons
#' @param cond
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ba_args = function(cons = NULL,
                       cond = NULL,
                       cond_warning = FALSE,
                       diagnostics = FALSE){
  ## per each argument,the function input "cons" should be a named
  # sub-list with one of these members:
  # arg, name, class, val, range, min_val, max_val, len, min_len, max_len,
  ## example:
  # list(arg = progress_bar, name = "progress_bar", class = "logical")

  ### "cond" should be sub-list containing an expression to be evaluated
  ### and an optional error message.
  # example:
  # list(list(quote(genes < background),
  #           "Provided genes length cannot be greater than the background")
  #      )

  ### 1 append extra arguments which occurs in most functions:
  if (exists("verbose", envir = parent.frame())) {
    cons = append(cons, list(list(arg = eval(parse(text = "verbose"),
                                             envir = parent.frame()),
                                  name = "verbose",
                                  class = "logical",
                                  len = 1)))
  }
  if (exists("progress_bar", envir = parent.frame())) {
    cons = append(cons, list(list(arg = eval(parse(text = "progress_bar"),
                                             envir = parent.frame()),
                                  name = "progress_bar",
                                  class = "logical",
                                  len = 1)))
  }
  if (exists("diagnostics", envir = parent.frame())) {
    cons = append(cons, list(list(arg = eval(parse(text = "diagnostics"),
                                             envir = parent.frame()),
                                  name = "diagnostics",
                                  class = "logical",
                                  len = 1)))
  }
  ### 2 Check  arguments
  for (i in seq_along(cons)){
    # only check if the provided argument is not NA or Null
    if (!is.na(cons[[i]][["arg"]][[1]]) & !is.null(cons[[i]][["arg"]][[1]])) {
      # check class
      if (!is.null(cons[[i]][["class"]]) &&
          !class(cons[[i]][["arg"]]) %in% cons[[i]][["class"]]) {
        stop("Invalid Argument: ", cons[[i]][["name"]],
             " should be of class '", paste0(cons[[i]][["class"]],
                                             collapse = " or "),
             "'\r\n(Your provided argument is '",
             class(cons[[i]][["arg"]]), "')\r\n", call. = diagnostics)
      }

      # check for allowed values
      if (!is.null(cons[[i]][["val"]]) &&
          !all(cons[[i]][["arg"]] %in% cons[[i]][["val"]])) {
        stop("Invalid Argument: ", cons[[i]][["name"]],
             " should be either '", paste0(cons[[i]][["val"]],
                                           collapse = " or "),
             "'\r\n(Your provided argument is '",
             cons[[i]][["arg"]], "')\r\n", call. = diagnostics)
      }

      # check for allowed range
      if (!is.null(cons[[i]][["ran"]]) &&
          !all(cons[[i]][["arg"]] >= cons[[i]][["ran"]][[1]] &
               cons[[i]][["arg"]] <= cons[[i]][["ran"]][[2]])) {
        stop("Invalid Argument: ", cons[[i]][["name"]],
             " should be from '", cons[[i]][["ran"]][[1]],
             "' to '", cons[[i]][["ran"]][[2]],
             "'\r\n(Your provided argument is '",
             cons[[i]][["arg"]], "')\r\n", call. = diagnostics)
      }

      # check length
      if (!is.null(cons[[i]][["len"]]) &&
          length(cons[[i]][["arg"]]) != cons[[i]][["len"]]) {
        stop("Invalid Argument: ", cons[[i]][["name"]],
             " should of length ", cons[[i]][["len"]],
             "\r\n(Your provided argument's length is '",
             length(cons[[i]][["arg"]]), "')\r\n", call. = diagnostics)
      }

      # check minimum length
      if (!is.null(cons[[i]][["min_len"]]) &&
          length(cons[[i]][["arg"]]) < cons[[i]][["min_len"]]) {
        stop("Invalid Argument: ", cons[[i]][["name"]],
             " should of minimum length ", cons[[i]][["min_len"]],
             "\r\n(Your provided argument's length is '",
             length(cons[[i]][["arg"]]), "')\r\n", call. = diagnostics)
      }

      # check maximum length
      if (!is.null(cons[[i]][["max_len"]]) &&
          length(cons[[i]][["arg"]]) > cons[[i]][["max_len"]]) {
        stop("Invalid Argument: ", cons[[i]][["name"]],
             " should of maximum length ", cons[[i]][["max_len"]],
             "\r\n(Your provided argument's length is '",
             length(cons[[i]][["arg"]]), "')\r\n", call. = diagnostics)
      }

      # check minimum value
      if (!is.null(cons[[i]][["min_val"]])
          && cons[[i]][["arg"]] < cons[[i]][["min_val"]]) {
        stop("Invalid Argument: ", cons[[i]][["name"]],
             " should be equal or greather than ",
             cons[[i]][["min_val"]],
             "\r\n(Your provided argument is '",
             cons[[i]][["arg"]], "')\r\n", call. = diagnostics)
      }

      # check maximum value
      if (!is.null(cons[[i]][["max_value"]])
          && cons[[i]][["arg"]] > cons[[i]][["max_value"]]) {
        stop("Invalid Argument: ", cons[[i]][["name"]],
             " should be equal or less than ",
             cons[[i]][["max_value"]],
             "\r\n(Your provided argument is '",
             cons[[i]][["arg"]], "')\r\n", call. = diagnostics)
      }


    }
  }

  ###  3 Check relationship between arguments
  if(!is.null(cond[[1]])){
    for (i in seq_along(cond)){
      # check if the expression is TRUE
      if (eval(cond[[i]][[1]], envir = parent.frame()) == TRUE){
        # throw an error / warning message
        if (!is.null(cond[[i]][[2]])) {
          if (cond_warning == TRUE) {
            warning(cond[[i]][[2]], call. = diagnostics)
          } else {
            stop(cond[[i]][[2]], call. = diagnostics)
          }
        } else {
          if (cond_warning == TRUE) {
            warning("Argument's conditions are not satisfied:\r\n'",
                    as.character(cond[[i]]), "' is TRUE.\r\n",
                    call. = diagnostics)
          } else {
            stop("Argument's conditions are not satisfied:\r\n'",
                 as.character(cond[[i]]), "' is TRUE.\r\n",
                 call. = diagnostics)
          }
        }
      }
    }
  }



  return(TRUE)
}
