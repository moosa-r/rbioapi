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
  if (verbose == TRUE) {message("Testing the internet connection.\r\n")}

  net_status = httr::status_code(httr::HEAD("https://www.google.com/",
                                            if (diagnostics) httr::verbose()
  ))


  retry_count = 0
  while (net_status != 200 & retry_count < retry_max) {
    retry_count = retry_count + 1

    if (verbose == TRUE) {message("No internet connection, waiting for ",
                                  wait_time,
                                  " seconds and retrying (retry count: ",
                                  retry_count, "/",
                                  retry_max, ")\r\n")}

    Sys.sleep(wait_time)
    net_status = httr::status_code(httr::HEAD("https://www.google.com/",
                                              if (diagnostics) httr::verbose()
    ))

  } #end of while

  if (net_status == 200) {
    if (verbose == TRUE) {message("Device is connected to the internet!\r\n")}
  } else {
    stop("NO internet connection! Terminating Code excutation!\r\n",
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
                      silent = TRUE)
  } else {
    test_result = try(httr::status_code(httr::HEAD(url,
                                                   httr::user_agent(getOption("rba_ua"))
    )),
    silent = TRUE)
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
  message("Checking Your connection to the Databases Currently Supported by rbioapi:")

  urls = list("STRING" = paste0(getOption("rba_url_string"), "/api/json/version"),
              "Enrichr" = paste0(getOption("rba_url_enrichr"), "/Enrichr")
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
    stop("Could not resolve google.com", " . Check Your internet Connection.", call. = diagnostics)
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
                                                flatten = TRUE)))
    } else {
      stop("Internal Error: Specify the parser expression!", call. = TRUE)
    }
  }
  # parse the response
  output = eval(parser, envir = parent.frame())
  return(output)
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
                            response_parser = NULL,
                            parser_type = NA,
                            user_agent = FALSE,
                            progress_bar = FALSE,
                            verbose = FALSE,
                            diagnostics = FALSE,
                            no_interet_retry_max = 1,
                            no_internet_wait_time = 10) {
  ## 1 Make API Call
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

  response = eval(call_function, envir = parent.frame())

  ## 2 Check the API call's response
  if (as.character(response$status) != "200"){
    ## check if there is an internet connection
    net_connected = rba_ba_internet_handler(retry_max = no_interet_retry_max,
                                            wait_time = no_internet_wait_time,
                                            verbose = verbose,
                                            diagnostics = diagnostics)
    if (net_connected == TRUE) {
      ## the problem is not related to the internet connectivity
      stop(rba_ba_translate(response$status_code), call. = diagnostics)
    } else {
      ## the system is connected to the internet now, retrying
      response = eval(call_function, envir = parent.frame())
      if (as.character(response$status_code) != "200") {
        stop(rba_ba_translate(response$status_code), call. = diagnostics)
      }
    }
  } else {
    ## 3 everything is OK (http status == 200)
    # parse the request's output to a suitable format
    final_output = rba_ba_response_parser(type = parser_type,
                                          parser = response_parser)
    return(final_output)
  }
} # end of function

#' A wrapper for General skeleton for long inputs
#'
#' @return
#' @export
#'
#' @examples
rba_ba_multi_batch = function(){
  ############ to do

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
rba_ba_arguments_check = function(cons = NULL,
                                  cond = NULL,
                                  diagnostics = FALSE){
  ### per each argument,the function input "cons" should be a named
  # sub-list with one of these members:
  # arg,
  # name,
  # class,
  # val
  # len
  # min_len,
  # max_len,
  # min_val,
  # max_val

  ### "cond" should be an expression to be evaluated
  ###  Check  arguments ----
  for (i in seq_along(cons)){
    # only check if the provided argument is not NA or Null
    if (!is.na(cons[[i]][["arg"]][[1]]) & !is.null(cons[[i]][["arg"]][[1]])) {
      # check class
      if (!is.null(cons[[i]][["class"]]) &&
          !class(cons[[i]][["arg"]]) %in% cons[[i]][["class"]]) {
        stop("\r\nInvalid Argument: ", cons[[i]][["name"]],
             " should be of class '", paste0(cons[[i]][["class"]],
                                             collapse = " or "),
             "'\r\n(Your provided argument is '",
             class(cons[[i]][["arg"]]), "')\r\n", call. = diagnostics)
      }

      # check for allowed values
      if (!is.null(cons[[i]][["val"]]) &&
          !all(cons[[i]][["arg"]] %in% cons[[i]][["val"]])) {
        stop("\r\nInvalid Argument: ", cons[[i]][["name"]],
             " should be either '", paste0(cons[[i]][["val"]],
                                           collapse = " or "),
             "'\r\n(Your provided argument is '",
             cons[[i]][["arg"]], "')\r\n", call. = diagnostics)
      }

      # check  length
      if (!is.null(cons[[i]][["len"]]) &&
          length(cons[[i]][["arg"]]) != cons[[i]][["len"]]) {
        stop("\r\nInvalid Argument: ", cons[[i]][["name"]],
             " should of length ", cons[[i]][["len"]],
             "\r\n(Your provided argument's length is '",
             length(cons[[i]][["arg"]]), "')\r\n", call. = diagnostics)
      }

      # check minimum length
      if (!is.null(cons[[i]][["min_len"]]) &&
          length(cons[[i]][["arg"]]) < cons[[i]][["min_len"]]) {
        stop("\r\nInvalid Argument: ", cons[[i]][["name"]],
             " should of minimum length ", cons[[i]][["min_len"]],
             "\r\n(Your provided argument's length is '",
             length(cons[[i]][["arg"]]), "')\r\n", call. = diagnostics)
      }

      # check maximum length
      if (!is.null(cons[[i]][["max_len"]]) &&
          length(cons[[i]][["arg"]]) > cons[[i]][["max_len"]]) {
        stop("\r\nInvalid Argument: ", cons[[i]][["name"]],
             " should of maximum length ", cons[[i]][["max_len"]],
             "\r\n(Your provided argument's length is '",
             length(cons[[i]][["arg"]]), "')\r\n", call. = diagnostics)
      }

      # check minimum value
      if (!is.null(cons[[i]][["min_val"]])
          && cons[[i]][["arg"]] < cons[[i]][["min_val"]]) {
        stop("\r\nInvalid Argument: ", cons[[i]][["name"]],
             " should be equal or greather than ",
             cons[[i]][["min_val"]],
             "\r\n(Your provided argument is '",
             cons[[i]][["arg"]], "')\r\n", call. = diagnostics)
      }

      # check maximum value
      if (!is.null(cons[[i]][["max_value"]])
          && cons[[i]][["arg"]] > cons[[i]][["max_value"]]) {
        stop("\r\nInvalid Argument: ", cons[[i]][["name"]],
             " should be equal or less than ",
             cons[[i]][["max_value"]],
             "\r\n(Your provided argument is '",
             cons[[i]][["arg"]], "')\r\n", call. = diagnostics)
      }


    }
  }

  ###  Check relationship between arguments ----
  if(!is.null(cond[[1]])){
    for (i in seq_along(cond)){
      if (!eval(cond[[i]], envir = parent.frame())){
        stop("\r\nArgument's conditions are not satisfied:\r\n'",
             as.character(cond[[i]]), "' is FALSE.\r\n",
             call. = diagnostics)
      }
    }
  }



  return(TRUE)
}
