##### data containers #######################################################
#' Internal Data Container for rbioapi
#'
#' Retrieve shared service metadata, current package options, configured service
#'   keys, or connection-test URLs. The first key selects "db", "options",
#'   "tests", or a configured service key; additional keys select values within
#'   that entry.
#'
#' Used throughout API-facing functions to retrieve service URLs and resource
#'   paths, by rba_connection_test() to obtain test URLs, and by
#'   .rba_error_parser() to select service-specific error rules.
#'
#' @param ... Character: One to three exact keys identifying a stored value.
#' @return The selected value.
#' @family internal_data_container
#' @noRd
.rba_stg <- function(...){

  supplied_arg <- c(...)
  arg_n <- length(supplied_arg)

  # Pad a separate dispatch vector for safe indexing
  arg <- c(
    supplied_arg,
    rep(NA_character_, max(0L, 3L - arg_n))
  )

  # Possible arguments
  output <- switch(
    arg[[1]],

    db = c("enrichr", "ensembl", "jaspar", "mieaa", "reactome", "panther", "string", "uniprot"),

    enrichr = switch(
      arg[[2]],
      name = "Enrichr",
      url = "https://maayanlab.cloud",
      pth = switch(
        arg[[3]],
        human = "Enrichr/",
        fly = "FlyEnrichr/",
        yeast = "YeastEnrichr/",
        worm = "WormEnrichr/",
        fish = "FishEnrichr/",
        speedrichr = "speedrichr/api/"
      ),
      ptn = "^(https?://)?(www\\.)?maayanlab\\.cloud/(.*Enrichr|speedrichr)/",
      err_ptn = "^4\\d\\d$",
      err_prs = list(
        "text->chr",
        function(x) {
          regmatches(
            x,
            regexec(
              "(?is)class=\"title\"[^>]*>\\s*Error\\s*</div\\s*>\\s*<div[^>]*>\\s*(.*?)\\s*</div\\s*>",
              x,
              perl = TRUE
            )
          )[[1]][[2]]
        }
      )
    ),

    ensembl = switch(
      arg[[2]],
      name = "Ensembl",
      url = "https://rest.ensembl.org",
      ptn = "^(https?://)?(www\\.)?rest\\.ensembl\\.org/",
      err_ptn = "^4\\d\\d$",
      err_prs = list(
        "json->list_simp",
        function(x) { x[["error"]] }
      )
    ),

    jaspar = switch(
      arg[[2]],
      name = "JASPAR",
      url = "https://jaspar.elixir.no/",
      pth = "api/v1/",
      ptn = "^(https?://)?(www\\.)?jaspar\\.elixir\\.no/api/",
      err_ptn = "^4\\d\\d$",
      err_prs = list(
        "json->list_simp",
        function(x) { x[["detail"]] }
      )
    ),

    mieaa = switch(
      arg[[2]],
      name = "miEAA",
      url = "https://ccb-compute2.cs.uni-saarland.de",
      pth = "mieaa/api/v1/",
      ptn = "^(https?://)?(www\\.)?ccb-compute2\\.cs\\.uni-saarland\\.de/mieaa/",
      err_ptn = "^4\\d\\d$",
      err_prs = list(
        "json->list_simp",
        function(x) {
          paste(
            unlist(x, recursive = TRUE, use.names = FALSE),
            collapse = "\n"
          )
        }
      )
    ),

    panther = switch(
      arg[[2]],
      name = "PANTHER",
      url = "https://www.pantherdb.org",
      pth = "services/oai/pantherdb/",
      ptn = "^(https?://)?(www\\.)?pantherdb\\.org/services/",
      err_ptn = "^4\\d\\d$",
      err_prs = list(
        "json->list_simp",
        function(x) { x$search$error }
      )
    ),

    reactome = switch(
      arg[[2]],
      name = "Reactome",
      url = "https://reactome.org",
      pth = switch(
        arg[[3]],
        analysis = "AnalysisService/",
        content = "ContentService/"
      ),
      ptn = "^(https?://)?(www\\.)?reactome\\.org/(?:AnalysisService|ContentService)/",
      err_ptn = "^[45]\\d\\d$",
      err_prs = list(
        "json->list_simp",
        function(x) { paste(x[["messages"]], collapse = "\n") }
      )
    ),

    string = switch(
      arg[[2]],
      name = "STRING",
      url = "https://version-12-0.string-db.org",
      pth = "api/",
      ptn = "^(http.?://).*string-db\\.org/api/",
      err_ptn = "^4\\d\\d$",
      err_prs = list(
        "json->list_simp",
        function(x) {
          error_message <- paste(
            x[["ErrorMessage"]],
            collapse = "\n"
          )
          error_message <- gsub(
            "<br\\b[^>]*>",
            "\n",
            error_message,
            ignore.case = TRUE,
            perl = TRUE
          )
          gsub("&nbsp;", " ", error_message, fixed = TRUE)
        }
      )
    ),

    uniprot = switch(
      arg[[2]],
      name = "UniProt",
      url = "https://www.ebi.ac.uk",
      pth = "proteins/api/",
      ptn = "^(https?://)?(www\\.)?ebi\\.ac\\.uk/proteins/api/",
      err_ptn = "^[45]\\d\\d$",
      err_prs = list(
        "json->list_simp",
        function(x) { paste(x[["errorMessage"]], collapse = "\n") }
      )
    ),

    options = switch(
      arg_n,
      `1` = options()[grep("^rba_", names(options()))],
      getOption(arg[[2]])
    ),

    tests = list(
      "Enrichr" = paste0(.rba_stg("enrichr", "url"), "/Enrichr/"),
      "Ensembl" = paste0(.rba_stg("ensembl", "url"), "/info/ping"),
      "JASPAR" = paste0(.rba_stg("jaspar", "url"), "api/v1/live-api/"),
      "miEAA" = paste0(.rba_stg("mieaa", "url"), "/mieaa/api/"),
      "PANTHER" = paste0(.rba_stg("panther", "url"), "/services/oai/pantherdb/supportedgenomes"),
      "Reactome Content Service" = paste0(.rba_stg("reactome", "url"), "/ContentService/data/database/name"),
      "Reactome Analysis Service" = paste0(.rba_stg("reactome", "url"), "/AnalysisService/database/name"),
      "STRING" = paste0(.rba_stg("string", "url"), "/api/json/version"),
      "UniProt" = paste0(.rba_stg("uniprot", "url"), "/proteins/api/proteins/P25445")
    )
  )

  if (is.null(output)) {
    traversal <- if (arg_n == 0L) {
      "<empty>"
    } else {
      paste(sprintf("`%s`", supplied_arg), collapse = " -> ")
    }

    stop(
      "Internal Error; .rba_stg was called with wrong parameters.\n",
      "Traversal: ", traversal,
      call. = TRUE
    )
  }

  return(output)
}

##### Internet connectivity ##################################################

#' Check Connectivity Before Retrying an API Request
#'
#' Send an HTTP HEAD request to Google to test whether the device can reach the
#'   internet. If the first attempt fails, retry the connectivity test up to
#'   retry_max times and wait retry_wait seconds between attempts.
#'
#' Used by .rba_api_call() after a request fails or returns a 5xx response. The
#'   result determines whether the original API request is retried or reported
#'   as a connectivity or server failure.
#'
#' @param retry_max Numeric: (default = 0) Maximum retries after the first
#'   connection test. Must be a finite non-negative whole number.
#' @param retry_wait Numeric: (default = 10) Seconds to wait between retries.
#'   Must be finite and non-negative.
#' @param verbose Logical: (default = FALSE) Show a message before each retry.
#' @param diagnostics Logical: (default = FALSE) Show httr diagnostics and the
#'   final connection status.
#' @param skip_error Logical: (default = TRUE) Currently unused; included
#'   because .rba_api_call() passes the same argument.
#'
#' @return TRUE if any connectivity test returns HTTP 200; FALSE otherwise.
#' @family internal_internet_connectivity
#' @noRd
.rba_net_handle <- function(retry_max = 0,
                            retry_wait = 10,
                            verbose = FALSE,
                            diagnostics = FALSE,
                            skip_error = TRUE) {
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
    return(TRUE)
  } else {
    if (isTRUE(diagnostics)) {message("No internet connection!")}
    return(FALSE)
  } #end of if net_test
}

#' Describe an HTTP Status Code
#'
#' Look up the status class and known meaning of an HTTP status code.
#'
#' Used by .rba_api_check() to describe unsuccessful connection tests and by
#'   .rba_error_parser() to add status context to service error messages.
#'
#' @param http_status Character or Numeric: A single three-digit HTTP status
#'   code from 100 through 599.
#' @param as_sentence Logical: (default = FALSE) Format the description as a
#'   complete sentence.
#'
#' @return A single character string with the status code, HTTP class, and
#'   known meaning, optionally formatted as a sentence.
#'
#' @references \href{https://www.iana.org/assignments/http-status-codes/}{IANA:
#'   Hypertext Transfer Protocol (HTTP) Status Code Registry}
#'
#' @family internal_internet_connectivity
#' @noRd
.rba_http_status <- function(http_status, as_sentence = FALSE){
  #ref:
  if (
    !is.atomic(http_status) ||
    length(http_status) != 1L ||
    is.na(http_status) ||
    !grepl("^[12345]\\d\\d$", http_status)
  ) {
    stop(
      "Internal Error; `http_status` should be a single three-digit ",
      "HTTP status code.",
      call. = TRUE
    )
  }
  http_status <- as.character(http_status)

  resp <- switch(
    substr(http_status, 1, 1),
    "1" = list(
      class = "Informational",
      meaning = switch(
        http_status,
        "100" = "Continue",
        "101" = "Switching Protocols",
        "102" = "Processing",
        "103" = "Early Hints")
    ),
    "2" = list(
      class = "Success",
      meaning = switch(
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
        "226" = "IM Used")
    ),
    "3" = list(
      class = "Redirection",
      meaning = switch(
        http_status,
        "300" = "Multiple Choices",
        "301" = "Moved Permanently",
        "302" = "Found",
        "303" = "See Other",
        "304" = "Not Modified",
        "305" = "Use Proxy",
        "306" = "Unused",
        "307" = "Temporary Redirect",
        "308" = "Permanent Redirect")
    ),
    "4" = list(
      class = "Client Error",
      meaning = switch(
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
        "413" = "Content Too Large",
        "414" = "URI Too Long",
        "415" = "Unsupported Media Type",
        "416" = "Range Not Satisfiable",
        "417" = "Expectation Failed",
        "418" = "Unused",
        "421" = "Misdirected Request",
        "422" = "Unprocessable Content",
        "423" = "Locked",
        "424" = "Failed Dependency",
        "425" = "Too Early",
        "426" = "Upgrade Required",
        "428" = "Precondition Required",
        "429" = "Too Many Requests",
        "431" = "Request Header Fields Too Large",
        "451" = "Unavailable For Legal Reasons")
    ),
    "5" = list(
      class = "Server Error",
      meaning = switch(
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
        "510" = "Not Extended (Obsoleted)",
        "511" = "Network Authentication Required")
    )
  )

  output <- if (is.null(resp$meaning)) {
    sprintf("HTTP Status '%s' (%s class)", http_status, resp$class)
  } else {
    sprintf("HTTP Status '%s' (%s: %s)", http_status, resp$class, resp$meaning)
  }

  if (isTRUE(as_sentence)) {
    output <- sprintf("The server returned %s.", output)
  }

  return(output)
}

##### API Calls ##################################################

#' Build Optional Parameters for an API Query
#'
#' Add optional parameters to a base query when their conditions are TRUE. Each
#'   definition keeps the API parameter name, inclusion condition, and value
#'   together, so endpoint code does not need to build optional entries by hand.
#'
#' Used by API-facing functions across the supported services to combine
#'   required query values with endpoint-specific optional parameters before
#'   constructing the request.
#'
#' @param init List: Base query as a named list. Use list() when no base
#'   parameters are needed.
#' @param ... List: (optional) Three-element parameter definitions: the API
#'   parameter name, a TRUE or FALSE condition, and the value to add. Multiple
#'   definitions may instead be supplied in a named extra_pars list.
#'
#' @return The completed query, with selected optional parameters appended to
#'   init.
#'
#' @family internal_api_calls
#' @noRd
.rba_query <- function(init, ...) {
  ## check the input method
  ext_par <- list(...)
  if (utils::hasName(ext_par, "extra_pars")) {
    ext_par <- ext_par$extra_pars
  }
  ## evaluate extra parameters
  ext_evl <- vapply(
    X = ext_par,
    FUN = function(x) {

      if (length(x[[2]]) > 1) {
        warning(
          "Internal Query Builder:\n",
          x[[1]],
          " has more than one element. Only the first element will be used.",
          call. = FALSE
        )
        x[[2]] <- x[[2]][[1]]
      }

      if (isTRUE(x[[2]])) {
        return(TRUE)
      } else if (isFALSE(x[[2]])) {
        return(FALSE)
      } else {
        warning(
          "Internal Query Builder:\n The evaluation result of ",
          x[[1]],
          " is not TRUE or FALSE, thus skipping it.",
          call. = FALSE
        )
        return(FALSE)}
    },
    FUN.VALUE = logical(1)
  )

  # extract extra parameters where theirs second element was TRUE
  ext_val <- lapply(ext_par[ext_evl], function(x) { x[[3]] })
  # set names to the extracted parameters
  if (length(ext_val) >= 1) {
    names(ext_val) <- vapply(
      ext_par[ext_evl],
      function(x) { x[[1]] },
      character(1)
    )
    init <- append(init, ext_val)
  }
  return(init)
}

#' Attach Request Metadata to an rbioapi Result
#'
#' Attach recorded request metadata to a non-NULL rbioapi result.
#'
#' Used by .rba_skeleton() to attach records from one request path and by
#'   .rba_metadata_aggregate() to attach records combined across a multi-step
#'   workflow.
#'
#' @param result Any: rbioapi result that should receive the metadata.
#' @param requests List: Request records collected by .rba_api_call().
#'
#' @return result with request metadata attached, or result unchanged when it
#'   is NULL or requests is empty.
#' @noRd
.rba_metadata_attach <- function(result, requests) {
  if (is.null(result) || !length(requests)) {
    return(result)
  }

  attr(result, "rbioapi_metadata") <- structure(
    list(
      rbioapi_version = as.character(utils::packageVersion("rbioapi")),
      requests = requests
    ),
    class = "rba_metadata"
  )
  return(result)
}

#' Combine Request Metadata Across Workflow Steps
#'
#' Collect request records from intermediate results and final_object in
#'   execution order. If a non-data-frame list has no metadata of its own,
#'   inspect its elements. Before attaching the combined history to a list
#'   final_object, remove metadata from its elements.
#'
#' Used by rba_enrichr_gene_sets(), rba_enrichr_enrich(), rba_enrichr(),
#'   rba_mieaa_enrich_submit(), rba_mieaa_enrich(), and
#'   rba_reactome_pathways_events() so each multi-request workflow returns one
#'   ordered history rather than metadata split across intermediate results.
#'
#' @param ... Any: (optional) Intermediate rbioapi results, in execution order.
#' @param final_object Any: Object that should receive the combined records.
#'
#' @return final_object with combined request metadata attached, or unchanged
#'   when no request records are found.
#' @noRd
.rba_metadata_aggregate <- function(..., final_object) {
  if (is.null(final_object)) {
    return(final_object)
  }

  requests <- list()
  for (object in append(list(...), list(final_object))) {
    metadata <- attr(object, "rbioapi_metadata", exact = TRUE)
    if (!is.null(metadata)) {
      requests <- append(requests, metadata$requests)
    } else if (is.list(object) && !is.data.frame(object)) {
      for (element in object) {
        metadata <- attr(element, "rbioapi_metadata", exact = TRUE)
        if (!is.null(metadata)) {
          requests <- append(requests, metadata$requests)
        }
      }
    }
  }

  if (!length(requests)) {
    return(final_object)
  }

  if (is.list(final_object) && !is.data.frame(final_object)) {
    for (i in seq_along(final_object)) {
      attr(final_object[[i]], "rbioapi_metadata") <- NULL
    }
  }

  return(.rba_metadata_attach(final_object, requests))
}

#' Build httr Calls for rbioapi Requests
#'
#' Convert request components into an unevaluated httr call, adding the package
#'   user agent and timeout together with diagnostics, progress, accepted
#'   response type, and disk writing when requested. Use accept and parser when
#'   file and R outputs share a format; otherwise, select the file_* or obj_*
#'   values according to save_to.
#'
#' Used by API-facing functions to construct requests consistently. They pass
#'   its request call and parser to .rba_skeleton() for execution and response
#'   handling.
#'
#' @param httr Character: HTTP method name in lowercase. Accepted values are
#'   "get", "post", "head", "put", "patch", and "delete".
#' @param url Character: (optional) Base URL for the requested resource.
#' @param path Character: (default = "") Resource path passed to the httr
#'   method.
#' @param ... Any: (optional) Request arguments and the save_to, accept, parser,
#'   file_accept, obj_accept, file_parser, and obj_parser controls. All other
#'   values are passed to the selected httr method.
#'
#' @return A list with the unevaluated request in call and the selected parser
#'   specification in parser.
#'
#' @family internal_api_calls
#' @noRd
.rba_httr <- function(httr,
                      url = NULL,
                      path = "",
                      ...) {
  ## assign global options
  diagnostics <- get0("diagnostics", envir = parent.frame(1), ifnotfound = getOption("rba_diagnostics"))
  progress <- get0("progress", envir = parent.frame(1), ifnotfound = getOption("rba_progress"))
  timeout <- get0("timeout", envir = parent.frame(1), ifnotfound = getOption("rba_timeout"))

  ### 1 capture extra arguments
  # possible args: all args supported by httr +
  # args to this function: [file/obj_]accept, [file/obj_]parser, save_to
  ext_args <- list(...)

  ### 2 build main HTTP request (using httr)
  httr_call <- list(
    switch(
      httr,
      "get" = quote(httr::GET),
      "post" = quote(httr::POST),
      "head" = quote(httr::HEAD),
      "put" = quote(httr::PUT),
      "delete" = quote(httr::DELETE),
      "patch" = quote(httr::PATCH),
      stop("Internal Error; what verb to use with httr?", call. = TRUE)
    ),
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
        httr_call <- append(
          httr_call,
          list(
            as.call(list(quote(httr::accept), ext_args$obj_accept))
          )
        )
        if (utils::hasName(ext_args, "obj_parser")) {parser <- ext_args$obj_parser}
      } else {
        httr_call <- append(
          httr_call,
          list(
            as.call(list(quote(httr::accept), ext_args$file_accept)
            ),
            as.call(
              list(
                quote(httr::write_disk),
                ext_args$save_to,
                overwrite = TRUE
              )
            )
          )
        )
        if (utils::hasName(ext_args, "file_parser")) {parser <- ext_args$file_parser}
      }

    } else {

      ## 3.1.b it was a pre-defined response type
      # accept header?
      if (utils::hasName(ext_args, "accept")) {
        httr_call <- append(
          httr_call,
          list(
            as.call(list(quote(httr::accept), ext_args$accept)
            )
          )
        )
      }
      # save to file?
      if (utils::hasName(ext_args, "save_to") && !isFALSE(ext_args$save_to)) {
        httr_call <- append(
          httr_call,
          list(
            as.call(
              list(
                quote(httr::write_disk),
                ext_args$save_to,
                overwrite = TRUE
              )
            )
          )
        )
      }
      # parser?
      if (utils::hasName(ext_args, "parser")) {
        parser <- ext_args$parser
      } else {
        parser <- function(x) { x }
      }

    }

    ### remove extra arguments that you don't want in httr function call
    ext_args <- ext_args[!grepl("^(?:accept|file_accept|obj_accept|save_to|\\w*parser)$",
                                names(ext_args))]

  } else {

    parser <- function(x) { x }

  } #end of if (length(ext_args...

  httr_call <- list(
    call = as.call(append(httr_call, ext_args)),
    parser = parser
  )

  return(httr_call)
}

#' Execute an API Request and Handle Failures
#'
#' Evaluate a request call from .rba_httr(). If it does not return an HTTP
#'   response, or returns one with a 5xx status, check connectivity and retry
#'   the request once when connected. Pass any remaining non-2xx response to
#'   .rba_error_parser(). When metadata is TRUE, record every completed HTTP
#'   response, including the response from a retry.
#'
#' Used only by .rba_skeleton(), which supplies the per-call option values and
#'   then parses a successful response or returns the handled failure.
#'
#' @param input_call Call: Unevaluated httr request call from .rba_httr().
#' @param skip_error Logical: (default = TRUE) Return request errors and
#'   non-2xx responses as error messages instead of stopping.
#' @param retry_max Numeric: (default = 0) Maximum retries after the first
#'   connectivity test. Must be a finite non-negative whole number.
#' @param retry_wait Numeric: (default = 10) Seconds between connectivity test
#'   retries. Must be finite and non-negative.
#' @param verbose Logical: (default = TRUE) Show connectivity retry messages.
#' @param diagnostics Logical: (default = FALSE) Show httr diagnostics and
#'   include calls in error messages.
#' @param metadata Logical: (default = FALSE) Record each completed HTTP
#'   response.
#'
#' @return A list with the HTTP response or error message in result and recorded
#'   request metadata in requests.
#'
#' @family internal_api_calls
#' @noRd
.rba_api_call <- function(input_call,
                          skip_error = TRUE,
                          retry_max = 0,
                          retry_wait = 10,
                          verbose = TRUE,
                          diagnostics = FALSE,
                          metadata = FALSE) {
  requests <- list()

  ## 1 call API
  response <- try(
    eval(input_call, envir = parent.frame(n = 2)),
    silent = !diagnostics
  )
  if (isTRUE(metadata) && inherits(response, "response")) {
    requests[[length(requests) + 1L]] <- list(
      timestamp = response$date,
      call = input_call,
      response = response,
      parsers = list()
    )
  }

  ## 2 check the internet connection & 5xx http status
  if (!inherits(response, "response") ||
      substr(response$status_code, 1, 1) == "5") {

    ## 2.1 there is an internet connection or server issue
    # wait for the internet connection
    net_connected <- .rba_net_handle(
      retry_max = retry_max,
      retry_wait = retry_wait,
      verbose = verbose,
      diagnostics = diagnostics,
      skip_error = skip_error
    )
    if (isTRUE(net_connected)) {
      ## 2.1.1 net_connection test is passed
      response <- try(
        eval(input_call, envir = parent.frame(n = 2)),
        silent = !diagnostics
      )
      if (isTRUE(metadata) && inherits(response, "response")) {
        requests[[length(requests) + 1L]] <- list(
          timestamp = response$date,
          call = input_call,
          response = response,
          parsers = list()
        )
      }
    }

  } # end of step 2

  ## 3 Decide what to return
  if (!inherits(response, "response")) {

    ## 3.1 errors un-related to server's response
    error_message <- ifelse(
      test = net_connected,
      yes = as.character(response),
      no = "No internet connection. Stopping code execution!"
    )

    if (isFALSE(diagnostics)) {
      error_message <- gsub(
        pattern = "(^Error in .*?\\(.*?\\) :\\s*)|(\\s*$)",
        replacement = "",
        x = error_message,
        perl = TRUE
      )
    }

    # stop or return error?
    if (isTRUE(skip_error)) {
      return(list(result = error_message, requests = requests))
    } else {
      stop(error_message, call. = diagnostics)
    }

  } else if (substr(response$status_code, 1, 1) != "2") {

    ## 3.2 API call was not successful
    error_output <- .rba_error_parser(response = response)
    if (isTRUE(metadata) && length(requests)) {
      requests[[length(requests)]]$parsers <- error_output$parsers_invoked
    }
    if (isTRUE(skip_error)) {
      return(list(result = error_output$result, requests = requests))
    } else {
      stop(error_output$result, call. = diagnostics)
    }

  } else {

    ## 3.3 Everything is OK (HTTP status is 2xx)
    return(list(result = response, requests = requests))

  }
}

#' Mark API Errors Returned with HTTP 2xx
#'
#' Some APIs include an error message in a successful HTTP response. Mark that
#'   message so the parser sequence stops and .rba_skeleton() can apply
#'   skip_error.
#'
#' Used by .rba_panther_check_response(), rba_panther_info(), and
#'   rba_panther_genome() when an HTTP 200 body reports an error or a requested
#'   page falls outside the available range. .rba_response_parser() recognizes
#'   the added class and stops the remaining parser sequence.
#'
#' @param message Character: A single non-missing, non-empty character string.
#'
#' @return The message with classes rba_api_error and character.
#'
#' @family internal_response_parser
#' @noRd
.rba_api_error <- function(message) {
  stopifnot(
    is.character(message),
    length(message) == 1L,
    !is.na(message),
    nzchar(message)
  )

  return(
    structure(
      unname(message),
      class = c("rba_api_error", "character")
    )
  )
}

#' Execute and Parse an rbioapi Request
#'
#' Complete an API request by passing input_call$call to .rba_api_call() and
#'   applying .rba_response_parser() to a successful response.
#'   response_parser takes precedence over input_call$parser. diagnostics,
#'   metadata, verbose, retry_max, retry_wait, and skip_error are read from the
#'   calling function or the package options.
#'
#' Used as the final request step by API-facing functions. All supported
#'   services therefore share the same execution, error handling, parsing, and
#'   metadata attachment.
#'
#' @param input_call List: Output from .rba_httr(), with call and parser
#'   elements.
#' @param response_parser Character, Function, or List: (optional) A recognized
#'   parser name, parser function, or sequence of parser names and functions to
#'   be applied by .rba_response_parser().
#'
#' @return The parsed result, or NULL when the response body is empty or no
#'   parser is available. With skip_error = TRUE, request and parsing failures
#'   return character error messages. Request metadata is attached when
#'   metadata is TRUE.
#'
#' @family internal_api_calls
#' @noRd
.rba_skeleton <- function(input_call,
                          response_parser = NULL) {
  ## 0 assign options variables
  diagnostics <- get0("diagnostics", envir = parent.frame(1), ifnotfound = getOption("rba_diagnostics"))
  metadata <- get0("metadata", envir = parent.frame(1), ifnotfound = getOption("rba_metadata"))
  verbose <- get0("verbose", envir = parent.frame(1), ifnotfound = getOption("rba_verbose"))
  retry_max <- get0("retry_max", envir = parent.frame(1), ifnotfound = getOption("rba_retry_max"))
  retry_wait <- get0("retry_wait", envir = parent.frame(1), ifnotfound = getOption("rba_retry_wait"))
  skip_error <- get0("skip_error", envir = parent.frame(1), ifnotfound = getOption("rba_skip_error"))

  ## 1 Make API Call
  api_output <- .rba_api_call(
    input_call = input_call$call,
    skip_error = skip_error,
    retry_max = retry_max,
    retry_wait = retry_wait,
    verbose = verbose,
    diagnostics = diagnostics,
    metadata = metadata
  )
  response <- api_output$result
  requests <- api_output$requests

  ## 2 Parse the the response if possible
  # Parser supplied via .rba_skeleton's 'response parser' argument will
  # override the 'parser' supplied in input call
  if (!is.null(response_parser)) {
    parser_input <- response_parser
  } else {
    parser_input <- input_call$parser
  }

  ## 3 Return the output
  if (inherits(response, "response")) {
    # There is a HTTP response, not an error message
    if (!is.null(parser_input)) {

      # A parser is provided for the response
      parser_output <- .rba_response_parser(
        response = response,
        parsers = parser_input
      )
      parsed_response <- parser_output$result
      if (length(requests)) {
        requests[[length(requests)]]$parsers <- parser_output$parsers_invoked
      }

      if (!inherits(parsed_response, "try-error")) {
        if (inherits(parsed_response, "rba_api_error")) {
          # The API returned an error message with status 2XX
          error_message <- as.character(parsed_response)

          if (isTRUE(skip_error)) {
            return(.rba_metadata_attach(error_message, requests))
          } else {
            stop(error_message, call. = diagnostics)
          }
        }
        # The parsed API response seems OK
        return(.rba_metadata_attach(parsed_response, requests))
      } else if (identical(httr::content(response, as = "text", encoding = "UTF-8"), "")) {
        # The API returned empty response or the response is empty after parsing
        return(NULL)
      } else {
        # The parsing raised an error
        parse_error_msg <- paste(
          "Internal Error:",
          "Failed to parse the server's response.",
          "This is probably because the server has changed the response format.",
          "Please report this bug to us:",
          "\n",
          parsed_response,
          sep = " "
        )
        if (isTRUE(skip_error)) {
          return(.rba_metadata_attach(parse_error_msg, requests))
        } else {
          stop(parse_error_msg, call. = TRUE)
        }
      }

    } else {

      # No parser is provided for the response
      return(invisible(NULL))

    }

  } else {

    return(.rba_metadata_attach(response, requests))

  }
}

#### Check Arguments #######

#' Infer NULL Constraints for Required Arguments
#'
#' For each supplied constraint, set no_null = TRUE when its argument has no
#'   default, unless the constraint explicitly allows NULL. Leave constraints
#'   for arguments with defaults unchanged. Contributors therefore only need to
#'   set no_null = TRUE for a defaulted argument whose downstream use cannot
#'   accept NULL; no_null = FALSE allows a required argument to accept NULL.
#'
#' Used by .rba_args() immediately before constraint evaluation so requiredness
#'   follows the calling function's signature instead of being repeated in each
#'   constraint definition.
#'
#' @param cons List: Constraint definitions supplied to .rba_args().
#' @param n Numeric: (default = 2) Number of calling functions to step back when
#'   locating the function to inspect.
#'
#' @return cons with required arguments marked as non-NULL.
#'
#' @family internal_arguments_check
#' @noRd
.rba_args_req <- function(cons, n = 2) {
  # List required arguments *arguments with no default value
  f <- sys.function(sys.parent(n))

  if (is.function(f)) {

    f_args <- formals(f)
    req <- names(f_args)[vapply(
      X = names(f_args),
      FUN = function(x) {
        x != "..." && identical(f_args[[x]], quote(expr = ))
      },
      FUN.VALUE = logical(1)
    )]
    # Add `no_null = TRUE` to required arguments unless explicitly set to `FALSE`
    cons <- lapply(
      X = cons,
      FUN = function(x) {
        if (
          x[["arg"]] %in% req &&
          !identical(x[["no_null"]], FALSE)
        ) {
          x[["no_null"]] <- TRUE
        }
        return(x)
      }
    )

  }

  return(cons)
}

#' Add rbioapi Option Checks to Argument Validation
#'
#' Detect standard rbioapi option variables present in the function being
#'   validated, then add their shared constraints or conditions. Keeping these
#'   rules here avoids repeating every option check in each API-facing function.
#'
#' Used twice by .rba_args(): once to add option constraints and once to add
#'   option conditions before endpoint-specific checks are evaluated.
#'
#' @param cons List: (optional) Existing argument constraints to extend.
#' @param cond List: (optional) Existing argument conditions to extend.
#' @param what Character: Type of check to return. Accepted values are "cons"
#'   for constraints and "cond" for conditions.
#'
#' @return The supplied checks plus those for rbioapi options present in the
#'   calling function.
#'
#' @family internal_arguments_check
#' @noRd
.rba_args_opts <- function(cons = NULL, cond = NULL, what) {
  if (what == "cons") {

    ext_cons <- list(
      timeout = list(arg = "timeout", class = "numeric", len = 1, ran = c(0.001, 3600)),
      dir_name = list(arg = "dir_name", class = "character", len = 1),
      diagnostics = list(arg = "diagnostics", class = "logical", len = 1),
      metadata = list(arg = "metadata", class = "logical", len = 1),
      retry_max = list(
        arg = "retry_max", class = "numeric", len = 1,
        integerish = TRUE, min_val = 0
      ),
      progress = list(arg = "progress", class = "logical", len = 1),
      save_file = list(arg = "save_file", class = c("logical", "character"), len = 1),
      skip_error = list(arg = "skip_error", class = "logical", len = 1),
      verbose = list(arg = "verbose", class = "logical", len = 1),
      retry_wait = list(arg = "retry_wait", class = "numeric", len = 1, min_val = 0)
    )
    cons <- append(
      ext_cons[names(ext_cons) %in% ls(envir = parent.frame(2))],
      cons
    )
    return(cons)

  } else if (what == "cond") {

    ext_cond <- list(
      dir_name = list(
        quote(
          !is.null(dir_name) &&
            grepl("[\\\\/:\"*?<>|]+", dir_name, perl = TRUE)
        ),
        "Invalid dir_name. Directory name cannot include these characters: \\/?%*:|<>"
      ),
      retry_wait = list(
        quote(!is.null(retry_wait) && !is.finite(retry_wait)),
        "Invalid retry_wait. It should be a finite, non-negative numeric scalar."
      ),
      save_file = list(
        quote(
          !is.null(save_file) &&
            !is.logical(save_file) &&
            !grepl("^[a-zA-z]:|^\\\\\\w|^/|\\w+\\.\\w+$", save_file)
        ),
        "Invalid save_file. You should set it to 'logical' or 'a valid file path'."
      )
    )
    cond <- append(
      ext_cond[names(ext_cond) %in% ls(envir = parent.frame(2))],
      cond
    )

    return(cond)

  } else {

    stop("Internal Error; `what` should be `cons` or `cond.`", call. = TRUE)

  }
}

#' Evaluate One Argument Constraint
#'
#' Apply one constraint to the evaluated argument stored in cons_i. NULL passes
#'   here because .rba_args_cons_wrp() handles it first.
#'
#' Used by .rba_args() for class checks and by .rba_args_cons_wrp() for the
#'   remaining value, range, length, whole-number, and pattern checks.
#'
#' @param cons_i List: Evaluated argument definition from .rba_args().
#' @param what Character: Constraint to evaluate. Accepted values are "class",
#'   "val", "ran", "integerish", "len", "min_len", "max_len", "min_val",
#'   "max_val", or "regex".
#'
#' @return TRUE when the selected constraint passes; FALSE otherwise.
#'
#' @family internal_arguments_check
#' @noRd
.rba_args_cons_chk <- function(cons_i, what) {
  if (!is.null(cons_i[["evl_arg"]])) {

    # Keep the original object for class and length checks.
    evl_arg <- cons_i[["evl_arg"]]
    value_arg <- evl_arg

    # Remove allowed missing values only from value-based checks.
    if (
      identical(cons_i[["no_na"]], FALSE) &&
      anyNA(value_arg, recursive = TRUE)
    ) {
      if (!is.atomic(value_arg)) {
        value_arg <- unlist(
          value_arg,
          recursive = TRUE,
          use.names = FALSE
        )
      }
      value_arg <- value_arg[!is.na(value_arg)]
    }

    # Run the requested constraint against the appropriate representation.
    output <- all(
      switch(
        what,
        "class" = inherits(evl_arg, cons_i[["class"]]),
        "val" = all(value_arg %in% cons_i[["val"]]),
        "ran" = all(
          value_arg >= cons_i[["ran"]][[1]],
          value_arg <= cons_i[["ran"]][[2]]
        ),
        "integerish" = !is.numeric(value_arg) ||
          all(is.finite(value_arg) & value_arg == trunc(value_arg)),
        "len" = length(evl_arg) == cons_i[["len"]],
        "min_len" = length(evl_arg) >= cons_i[["min_len"]],
        "max_len" = length(evl_arg) <= cons_i[["max_len"]],
        "min_val" = value_arg >= cons_i[["min_val"]],
        "max_val" = value_arg <= cons_i[["max_val"]],
        "regex" = grepl(
          pattern = cons_i[["regex"]],
          x = value_arg,
          ignore.case = FALSE, perl = TRUE
        ),
        stop("Internal Error; constrian is not defiend: ", what, call. = TRUE)
      )
    )
    return(output)

  } else {

    return(TRUE)

  }
}

#' Describe an Argument Constraint Failure
#'
#' Build the error message for one failed argument constraint.
#'
#' Used by .rba_args() and .rba_args_cons_wrp() to turn failed class, NULL,
#'   missing-value, value, range, length, whole-number, and pattern checks into
#'   consistent messages.
#'
#' @param cons_i List: Evaluated argument definition associated with the
#'   failure.
#' @param what Character: Failed constraint. Accepted values are "no_null",
#'   "no_na", "class", "val", "ran", "integerish", "len",
#'   "min_len", "max_len", "min_val", "max_val", or "regex".
#'
#' @return The error message as a character string.
#'
#' @family internal_arguments_check
#' @noRd
.rba_args_cons_msg <- function(cons_i, what) {
  switch(
    what,
    "no_null" = sprintf(
      "Invalid Argument: `%s` cannot be NULL.", cons_i[["arg"]]
    ),
    "no_na" = sprintf(
      "Invalid Argument: `%s` cannot contain `NA` or `NaN` values.",
      cons_i[["arg"]]
    ),
    "class" = sprintf(
      "Invalid Argument: %s should be of class `%s`.\n\t(Your supplied argument is \"%s\".)",
      cons_i[["arg"]],
      .paste2(cons_i[["class"]], last = " or ", quote = "\""),
      .paste2(class(cons_i[["evl_arg"]]), last = " and ")
    ),
    "val" = sprintf(
      "Invalid Argument: %s should be either `%s`.\n\t(Your supplied argument is `%s`.)",
      cons_i[["arg"]],
      .paste2(cons_i[["val"]], last = " or ", quote = "\""),
      cons_i[["evl_arg"]]
    ),
    "ran" = sprintf(
      "Invalid Argument: %s should be `from %s to %s`.\n\t(Your supplied argument is `%s`.)",
      cons_i[["arg"]],
      cons_i[["ran"]][[1]],
      cons_i[["ran"]][[2]],
      cons_i[["evl_arg"]]
    ),
    "integerish" = sprintf(
      "Invalid Argument: Numeric values in `%s` should be finite whole numbers.",
      cons_i[["arg"]]
    ),
    "len" = sprintf(
      "Invalid Argument: %s should be of length `%s`.\n\t(Your supplied argument's length is `%s`.)",
      cons_i[["arg"]],
      cons_i[["len"]],
      length(cons_i[["evl_arg"]])
    ),
    "min_len" = sprintf(
      "Invalid Argument: %s should be of minimum length `%s`.\n\t(Your supplied argument's length is `%s`.)",
      cons_i[["arg"]],
      cons_i[["min_len"]],
      length(cons_i[["evl_arg"]])
    ),
    "max_len" = sprintf(
      "Invalid Argument: %s should be of maximum length `%s`.\n\t(Your supplied argument's length is `%s`.)",
      cons_i[["arg"]],
      cons_i[["max_len"]],
      length(cons_i[["evl_arg"]])
    ),
    "min_val" = sprintf(
      "Invalid Argument: %s should be equal to or greater than `%s`.\n\t(Your supplied argument is `%s`.)",
      cons_i[["arg"]],
      cons_i[["min_val"]],
      cons_i[["evl_arg"]]
    ),
    "max_val" = sprintf(
      "Invalid Argument: %s should be equal to or less than `%s`.\n\t(Your supplied argument is `%s`.)",
      cons_i[["arg"]],
      cons_i[["max_val"]],
      cons_i[["evl_arg"]]
    ),
    "regex" = sprintf(
      "Invalid Argument: %s do not have a valid format.\n\t(It should match regex pattern: %s ).",
      cons_i[["arg"]],
      cons_i[["regex"]]
    ),
    stop("Internal Error: constrian message is not defiend: ", what, call. = TRUE)
  )
}

#' Evaluate All Constraints for One Argument
#'
#' Check whether NULL and missing values are allowed, then evaluate each
#'   remaining constraint and collect its failure message.
#'
#' Used by .rba_args() for each evaluated argument after requiredness and class
#'   handling, allowing all remaining failures for that argument to be reported
#'   together.
#'
#' @param cons_i List: Evaluated argument definition from .rba_args().
#'
#' @return One or more failure messages, or NA when all constraints pass.
#'
#' @family internal_arguments_check
#' @noRd
.rba_args_cons_wrp <- function(cons_i) {
  # Handle NULL policy before any other constraint.
  if (is.null(cons_i[["evl_arg"]])) {

    if (isTRUE(cons_i[["no_null"]])) {
      return(.rba_args_cons_msg(cons_i = cons_i, what = "no_null"))
    } else {
      return(NA)
    }

  } else {

    # Enforce missing-value policy before the remaining constraints.
    has_na <- anyNA(cons_i[["evl_arg"]], recursive = TRUE)

    if (has_na && !identical(cons_i[["no_na"]], FALSE)) {
      return(.rba_args_cons_msg(cons_i = cons_i, what = "no_na"))
    }

    # Run each remaining constraint and collect its failure message.
    all_cons <- setdiff(
      names(cons_i),
      c("arg", "class", "evl_arg", "no_null", "no_na")
    )
    cons_i_errs <- lapply(
      all_cons,
      function(x){
        if (.rba_args_cons_chk(cons_i = cons_i, what = x)) {
          return(NA)
        } else {
          return(.rba_args_cons_msg(cons_i = cons_i, what = x))
        }
      }
    )

    if (any(!is.na(cons_i_errs))) {
      return(unlist(cons_i_errs[which(!is.na(cons_i_errs))]))
    } else {
      return(NA)
    } #end of any(!is.na(cons_i_errs))

  } #end of if (is.null(cons_i[["evl_arg"]]))
}


#' Evaluate One Custom Argument Condition
#'
#' Evaluate one condition in the environment of the function being checked. In
#'   the validation protocol, TRUE represents a failure. A condition may add a
#'   custom message and may mark that failure as a warning.
#'
#' Used by .rba_args() after value constraints pass to evaluate relationships
#'   and other rules that cannot be expressed as one argument constraint.
#'
#' @param cond_i List: Condition definition with a quoted R call or a
#'   character string of R code, optionally followed by a message, a warning
#'   flag, or both.
#'
#' @return NA when the condition is FALSE; otherwise, a list with msg and warn
#'   for .rba_args().
#'
#' @family internal_arguments_check
#' @noRd
.rba_args_cond <- function(cond_i) {
  cond_n <- length(cond_i)

  ## Validate the condition construct
  if (!is.list(cond_i) || !(cond_n %in% 1:3)) {
    stop(
      "Internal Error; invalid condition definition.",
      call. = TRUE
    )
  }

  cond_expr <- cond_i[[1]]
  cond_chr <- is.character(cond_expr) &&
    length(cond_expr) == 1L &&
    !is.na(cond_expr)

  if (!is.call(cond_expr) && !cond_chr) {
    stop(
      "Internal Error; the first element in the condition sublist ",
      "should be either a character scalar or quoted call.",
      call. = TRUE
    )
  }

  if (cond_chr) {
    cond_expr <- parse(text = cond_expr)
  }
  cond_i_1 <- eval(cond_expr, envir = parent.frame(3))

  if (
    !is.logical(cond_i_1) ||
    length(cond_i_1) != 1L ||
    is.na(cond_i_1)
  ) {
    stop(
      "Internal Error; an evaluated condition should return one ",
      "non-missing logical value.",
      call. = TRUE
    )
  }

  if (!isTRUE(cond_i_1)) {
    return(NA)
  }

  ## Create an Error message
  if (cond_n == 3L) {
    err_obj <- list(
      msg = cond_i[[2]],
      warn = isTRUE(cond_i[[3]])
    )
  } else if (cond_n == 2L && is.character(cond_i[[2]])) {
    err_obj <- list(
      msg = cond_i[[2]],
      warn = FALSE
    )
  } else {
    err_obj <- list(
      msg = sprintf(
        "Argument's conditions are not satisfied; `%s` is TRUE.",
        as.character(enquote(cond_i[[1]]))[[2]]
      ),
      warn = cond_n == 2L && isTRUE(cond_i[[2]])
    )
  }

  return(err_obj)
}

#' Validate Arguments in the Calling Function
#'
#' Validate argument values and relationships among arguments in the calling
#'   function. A constraint definition takes a form such as
#'   list(arg = "species", class = c("character", "numeric")). A condition
#'   definition begins with a quoted call or a character string of R code and
#'   may add a message, warn = TRUE, or both; a TRUE result marks a failure.
#'
#' Used throughout API-facing functions and selected helpers to keep argument
#'   contracts in one validation path. Standard rbioapi option checks are added
#'   automatically when their variables are present in the calling function.
#'
#' @param cons List: (optional) Argument constraints. Each list element names an
#'   argument in arg and may define no_null, no_na, class, val, ran, integerish,
#'   min_val, max_val, len, min_len, max_len, or regex. Arguments without
#'   defaults reject NULL unless no_null = FALSE; arguments with defaults allow
#'   NULL unless no_null = TRUE. Non-NULL values reject NA and NaN unless
#'   no_na = FALSE. integerish = TRUE requires finite whole numbers but does not
#'   impose a sign or range.
#' @param cond List: (optional) Argument conditions evaluated in the calling
#'   function. Each condition starts with a quoted R call or character string
#'   of R code and must return one non-missing logical value. TRUE marks a
#'   failure. Optional elements supply a message and warning flag.
#' @param cond_warning Logical: (default = FALSE) Treat all condition failures
#'   as warnings. When FALSE, failures produce a warning only if every failed
#'   condition has warn = TRUE; otherwise, validation stops.
#'
#' @return NULL invisibly when validation succeeds or raises warnings only.
#'   Invalid arguments and condition failures not marked as warnings stop
#'   execution.
#'
#' @family internal_arguments_check
#' @noRd
.rba_args <- function(cons = NULL,
                      cond = NULL,
                      cond_warning = FALSE){
  ### 0 set diagnostics
  diagnostics <- get0("diagnostics", envir = parent.frame())
  if (
    is.null(diagnostics) ||
    length(diagnostics) != 1L ||
    !is.logical(diagnostics) ||
    is.na(diagnostics)
  ) {
    diagnostics <- getOption("rba_diagnostics")
  }
  ### 1.1 append extra arguments which occurs in most functions:
  cons <- .rba_args_opts(cons = cons, what = "cons")
  cond <- .rba_args_opts(cond = cond, what = "cond")

  ### 2 Check Arguments
  errors <- c()
  ## 2.1 check if the supplied object can be evaluated
  cons <- lapply(
    X = cons,
    FUN = function(cons_i){
      cons_i[["evl_arg"]] <- try(
        expr = get(
          x = cons_i[["arg"]],
          envir = parent.frame(3),
          inherits = FALSE
        ),
        silent = TRUE
      )
      return(cons_i)
    }
  )
  cons_not_exist <- vapply(
    X = cons,
    FUN = function(x) {
      inherits(x[["evl_arg"]], "try-error")
    },
    FUN.VALUE = logical(1)
  )

  if (any(cons_not_exist)) { # some object didn't exist!

    #generate errors
    errors <- append(
      errors,
      vapply(
        X = cons[cons_not_exist],
        FUN = function(x){
          error_message <- regmatches(
            x[["evl_arg"]],
            regexpr("(?<=(Error: )|(Error : )).*?(?=\n)", x[["evl_arg"]], perl = TRUE)
          )
          return(
            ifelse(
              length(error_message) == 0,
              yes = sub("^Error in.*: +\n", "", x[["evl_arg"]][[1]], perl = TRUE),
              no = error_message
            )
          )
        },
        FUN.VALUE = character(1)
      )
    )
    #remove from cons
    cons <- cons[!cons_not_exist]

  }

  ## 2.2 check missing values and class
  class_errs <- lapply(
    cons,
    function(x) {
      evl_arg <- x[["evl_arg"]]
      has_na <- anyNA(evl_arg, recursive = TRUE)

      if (has_na && !identical(x[["no_na"]], FALSE)) {
        return(.rba_args_cons_msg(cons_i = x, what = "no_na"))
      }

      all_na <- FALSE
      if (has_na) {
        if (!is.atomic(evl_arg)) {
          evl_arg <- unlist(
            evl_arg,
            recursive = TRUE,
            use.names = FALSE
          )
        }
        all_na <- length(evl_arg[!is.na(evl_arg)]) == 0L
      }

      if (all_na || .rba_args_cons_chk(cons_i = x, what = "class")) {
        return(NA)
      } else {
        return(.rba_args_cons_msg(cons_i = x, what = "class"))
      }
    }
  )

  if (any(!is.na(class_errs))) {
    errors <- append(errors, unlist(class_errs[!is.na(class_errs)]))
    cons <- cons[is.na(class_errs)] # remove elements with wrong class
  }

  ## 2.3 check other constrains if their class is correct
  ### Add no_null for arguments with no default value
  cons <- .rba_args_req(cons = cons, n = 2)

  ### Check
  other_errs <- lapply(cons, .rba_args_cons_wrp)
  if (any(!is.na(other_errs))) {
    errors <- append(errors, other_errs[!is.na(other_errs)])
  }

  ## 2.4 Take actions for the errors
  errors <- unlist(errors, recursive = TRUE, use.names = FALSE)
  if (length(errors) == 1) {

    stop(errors, call. = diagnostics)

  } else if (length(errors) > 1) {

    error_message <- paste0("\n", seq_along(errors), "- ", errors)
    stop(
      sprintf("Your supplied arguments contains the following `%s Errors`.", length(errors)),
      error_message,
      call. = diagnostics
    )

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
        cond_msg <- paste0(
          "\n", seq_along(cond_err), "- ",
          vapply(X = cond_err, FUN = function(x) { x[["msg"]] }, FUN.VALUE = character(1)),
          collapse = ""
        )
        cond_msg <- sprintf(
          "Your supplied arguments contains the following `%s Conditional Issues`.:%s",
          length(cond_err),
          cond_msg
        )
      }
      ## 3.3 Take actions for the errors
      if (isTRUE(cond_warning) || all(vapply(X = cond_err,
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

#' Apply Parsers to an API Response
#'
#' Apply one or more parsers sequentially to an HTTP response, passing each
#'   parser's output to the next. Each parser may be a single-argument function
#'   or one of these predefined names:
#'   "json->df", "json->df_no_flat", "json->list_simp", "json->list",
#'   "json->list_simp_flt_df", "json->chr", "text->chr", "text->df", "tsv->df".
#'   Stop after the first parser that fails or returns an rba_api_error object.
#'
#' Used by .rba_skeleton() for successful responses and by .rba_error_parser()
#'   for service-specific error bodies.
#'
#' @param response Response: An httr response object.
#' @param parsers Character, Function, or List: One parser or an ordered parser
#'   sequence.
#'
#' @return A list with the parsed result or parser error in result and the
#'   attempted parser functions in parsers_invoked.
#'
#' @family internal_response_parser
#' @noRd
.rba_response_parser <- function(response, parsers) {
  if (!is.vector(parsers)) { parsers <- list(parsers)}

  parsers <- lapply(
    X = parsers,
    FUN = function(parser){
      #create a parser if not supplied
      if (!is.function(parser)) {
        parser <- switch(
          parser,
          "json->df" = function(x) {
            data.frame(
              jsonlite::fromJSON(
                httr::content(x, as = "text", encoding = "UTF-8"),
                flatten = TRUE
              ),
              stringsAsFactors = FALSE
            )
          },
          "json->df_no_flat" = function(x) {
            data.frame(
              jsonlite::fromJSON(
                httr::content(x, as = "text", encoding = "UTF-8"),
                flatten = FALSE
              ),
              stringsAsFactors = FALSE
            )
          },
          "json->list_simp" = function(x) {
            as.list(
              jsonlite::fromJSON(
                httr::content(x, as = "text", encoding = "UTF-8"),
                simplifyVector = TRUE
              )
            )
          },
          "json->list_simp_flt_df" = function(x) {
            sapply(
              X = as.list(
                jsonlite::fromJSON(
                  httr::content(x, as = "text", encoding = "UTF-8"),
                  simplifyVector = TRUE
                )
              ),
              FUN = function(y){
                if (is.data.frame(y)) {
                  jsonlite::flatten(y)
                } else {
                  y
                }
              }
            )
          },
          "json->list" = function(x) {
            as.list(
              jsonlite::fromJSON(
                httr::content(x, as = "text", encoding = "UTF-8"),
                simplifyVector = FALSE
              )
            )
          },
          "json->chr" = function(x) {
            as.character(
              jsonlite::fromJSON(
                httr::content(x, as = "text", encoding = "UTF-8")
              )
            )
          },
          "text->chr" = function(x) {
            as.character(
              httr::content(x, as = "text", encoding = "UTF-8")
            )
          },
          "text->df" = function(x) {
            utils::read.table(
              text = httr::content(x, type = "text/plain", as = "text", encoding = "UTF-8"),
              header = FALSE,
              stringsAsFactors = FALSE
            )
          },
          "tsv->df" = function(x) {
            as.character(
              httr::content(x, as = "text", encoding = "UTF-8")
            )
          },
          stop("Internal Error; Specify a valid parser name or supply a function!", call. = TRUE)
        )
      }
      return(parser)
    }
  )

  # sequentially handle the response to the parsers
  last_invoked <- 0L
  for (parser in seq_along(parsers)) {
    last_invoked <- parser
    response <- try(
      do.call(what = parsers[[parser]], args = list(response)),
      silent = TRUE
    )

    if (
      inherits(response, "try-error") ||
      inherits(response, "rba_api_error")
    ) {
      break
    }
  }

  return(list(
    result = response,
    parsers_invoked = parsers[seq_len(last_invoked)]
  ))
}

#' Parse a Service-Specific API Error Response
#'
#' Match the response URL against each service's ptn value in .rba_stg(). When
#'   the status matches err_ptn, apply err_prs and use the parsed message if it
#'   is one non-missing, non-empty character value. Otherwise, fall back to the
#'   raw response body, or report that no body was returned. Prefix the details
#'   with the service name and HTTP status.
#'
#' Used by .rba_api_call() for every non-2xx response that remains after
#'   connectivity handling, so service-specific details follow the common
#'   error-handling path.
#'
#' @param response Response: An httr response object from an API service.
#' @return A list with the formatted error message in result and any attempted
#'   service-specific parsers in parsers_invoked.
#'
#' @family internal_response_parser
#' @noRd
.rba_error_parser <- function(response) {
  # Identify the responding service.
  services <- .rba_stg("db")
  service <- services[vapply(
    X = services,
    FUN = function(service) {
      grepl(
        pattern = .rba_stg(service, "ptn"),
        x = response$url,
        perl = TRUE,
        ignore.case = TRUE
      )
    },
    FUN.VALUE = logical(1)
  )]

  is_valid_message <- function(x) {
    !inherits(x, "try-error") &&
      is.character(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      nzchar(x)
  }

  # Try the service-specific parser for its known error statuses.
  if (
    grepl(
      pattern = .rba_stg(service, "err_ptn"),
      x = response$status_code
    )
  ) {
    parser_output <- .rba_response_parser(
      response = response,
      parsers = .rba_stg(service, "err_prs")
    )
  } else {
    parser_output <- list(result = NULL, parsers_invoked = list())
  }
  parsed_message <- parser_output$result

  # Use the raw response unless tailored parsing produced a message.
  if (is_valid_message(parsed_message)) {
    error_details <- sprintf(
      "The server provided the following error message:\n%s",
      parsed_message
    )
  } else {
    raw_response <- tryCatch(
      httr::content(response, as = "text", encoding = "UTF-8"),
      error = function(e) NULL
    )

    if (is_valid_message(raw_response)) {
      error_details <- sprintf(
        "The response did not match the service's known error format. For reference, the raw server response was:\n%s",
        raw_response
      )
    } else {
      error_details <- "The server did not provide a response body."
    }
  }

  # Prepend the common service and HTTP-status description.
  return(list(
    result = sprintf(
      "%s returned an error response with %s.\n%s",
      .rba_stg(service, "name"),
      .rba_http_status(
        http_status = response$status_code,
        as_sentence = FALSE
      ),
      error_details
    ),
    parsers_invoked = parser_output$parsers_invoked
  ))
}
#### Miscellaneous ####
#' Format and Show a Conditional Message
#'
#' Show a message only when the value named by cond is TRUE in the calling
#'   function. Use sprintf() when enabled and fmt contains "%s"; otherwise,
#'   join the values with paste().
#'
#' Used throughout API-facing functions and internal helpers to keep verbose and
#'   diagnostics output conditional without repeating the same value lookup and
#'   formatting logic.
#'
#' @param fmt Any: Format string for sprintf(), or the first value passed to
#'   paste().
#' @param ... Any: (optional) Additional values passed to sprintf() or paste().
#' @param sprintf Logical: (default = TRUE) Use sprintf() when fmt is a
#'   character string containing "%s". If FALSE, always use paste().
#' @param cond Character: (default = "verbose") Name of a logical value in the
#'   calling function. The message is suppressed unless the value is exactly
#'   TRUE.
#' @param sep Character: (default = "") Separator passed to paste().
#' @param collapse Character: (optional) Value passed to paste().
#'
#' @return NULL invisibly.
#'
#' @family internal_misc
#' @noRd
.msg <- function(fmt,
                 ...,
                 sprintf = TRUE,
                 cond = "verbose",
                 sep = "",
                 collapse = NULL) {
  if (isTRUE(get0(cond, envir = parent.frame(1), ifnotfound = FALSE))) {
    m <- ifelse(
      isTRUE(sprintf) && is.character(fmt) && grepl("%s", fmt, fixed = TRUE),
      yes = sprintf(fmt, ...),
      no = paste(fmt, ..., sep = sep, collapse = collapse)
    )
    if (!is.na(m)) {
      message(m, appendLF = TRUE)
    }
  }
  invisible()
}

#' Join Values as an English List
#'
#' Join values with sep, using last before the final value. Optionally quote
#'   each value or the completed result to produce readable lists such as
#'   "alpha, beta, and gamma".
#'
#' Used by .rba_args_cons_msg() and .rba_ext_args() for validation messages,
#'   and by rba_mieaa_enrich_submit(), rba_panther_ortholog(),
#'   rba_panther_homolog(), rba_uniprot_taxonomy_lca(), and
#'   rba_uniprot_taxonomy() for readable service messages.
#'
#' @param ... Any: (optional) Values to combine.
#' @param last Character: (default = " and ") Separator between the final two
#'   values.
#' @param sep Character: (default = ", ") Separator between preceding values.
#' @param quote Character: (optional) Delimiter placed around each value.
#' @param quote_all Character: (optional) Delimiter placed around the completed
#'   result.
#'
#' @return NULL when no values are supplied; one unquoted value unchanged;
#'   otherwise, one combined character string.
#'
#' @family internal_misc
#' @noRd
.paste2 <- function(...,
                    last = " and ",
                    sep = ", ",
                    quote = NULL,
                    quote_all = NULL) {
  input <- c(...)
  len <- length(input)
  if (!is.null(quote)) {
    input <- sprintf("%s%s%s", quote, input, quote)
  }
  if (len > 1) {
    input <- paste(
      paste0(input[-len], collapse = sep), input[len], sep = last
    )
  }
  if (!is.null(quote_all)) {
    input <- sprintf("%s%s%s", quote_all, input, quote_all)
  }
  return(input)
}

#' Resolve a File Path for an API Response
#'
#' Return FALSE when saving is disabled. A character save_to value supplies a
#'   file or directory path; TRUE builds a path from file and dir_name. An
#'   invalid character path warns and falls back to path generation. Explicit
#'   file paths may overwrite, while generated paths receive a numeric suffix
#'   when needed to preserve an existing file. If save_to is NULL, read
#'   save_file from the calling function and use FALSE when it is unavailable.
#'
#' Used by API-facing functions that support saving responses. The selected
#'   path is passed to .rba_httr() so per-call and package-wide saving options
#'   follow the same rules.
#'
#' @param file Character: Default file name, including its extension.
#' @param save_to Logical or Character: (optional) FALSE to disable saving, TRUE
#'   to generate a path, or a file or directory path.
#' @param dir_name Character: (optional) Directory used when a path must be
#'   generated.
#'
#' @return FALSE when saving is disabled; otherwise, the selected file path.
#'
#' @family internal_misc
#' @noRd
.rba_file <- function(file,
                      save_to = NULL,
                      dir_name = NULL) {
  if (is.null(save_to)) {
    save_to <- get0(
      x = "save_file",
      ifnotfound = FALSE,
      envir = parent.frame(1)
    )
  }

  if (
    is.atomic(save_to) &&
    length(save_to) == 1L &&
    is.na(save_to)
  ) {
    save_to <- FALSE
  }

  if (!isFALSE(save_to)) {
    ## 1 file path will be generated unless save_to == FALSE
    # set values
    diagnostics <- get0(
      "diagnostics",
      envir = parent.frame(1),
      ifnotfound = getOption("rba_diagnostics")
    )
    verbose <- get0(
      "verbose",
      envir = parent.frame(1),
      ifnotfound = getOption("rba_verbose")
    )

    # set defaults
    def_file_ext <- regmatches(
      file,
      regexpr("(?<=\\.)\\w+?$", file, perl = TRUE)
    )

    def_file_name <- regmatches(
      file,
      regexpr(sprintf("^.*(?=\\.%s$)", def_file_ext), file, perl = TRUE)
    )

    ## File path is in "save_to", if not in "file = file_name.file_ext"
    if (is.character(save_to)) {


      # 2a the user supplied a file path, just check if it is valid
      if (!grepl("^[a-zA-z]:|^\\\\\\w|^/|^\\w+\\.\\w+$", save_to)) {
        ## 2a.1 not a valid file path!
        warning(
          sprintf("\"%s\" is not a valid file path. Ignored that.", save_to),
          call. = diagnostics
        )
        save_to <- TRUE

      } else {

        ## 2a.2 the supplied file path is valid
        ## 2a.2.1 Does the path end to a directory or file?
        if (!grepl("/$", save_to, perl = TRUE) &&
            grepl("\\S+\\.\\S*", basename(save_to), perl = TRUE)) {
          # 2a.2.1a it's file!
          overwrite <- TRUE
          # extract the file name and extension
          file_ext <- regmatches(
            basename(save_to),
            regexpr("(?<=\\.)\\w+?$", basename(save_to), perl = TRUE)
          )
          file_name <- regmatches(
            basename(save_to),
            regexpr(sprintf("^.*(?=\\.%s$)", file_ext), basename(save_to), perl = TRUE)
          )
          # 2a.3 Check if the path and extension agree
          if (!grepl(def_file_ext, file_ext, ignore.case = TRUE)) {
            warning(
              sprintf(
                "The Response file's type (\"%s\") does not match the extension of your supplied file path(\"%s\").",
                def_file_ext, basename(save_to)
              ),
              call. = diagnostics
            )
          }

        } else {

          #2a.2.1b it's directory
          overwrite <- FALSE
          ## append the default file name to the directory path
          file_ext <- def_file_ext
          file_name <- def_file_name
          save_to <- file.path(
            sub("/$", "", save_to),
            paste0(file_name, ".", file_ext)
          )

        }

      }
    }

    if (isTRUE(save_to)) {
      ## 2b User didn't supply a file path, use defaults
      overwrite <- FALSE
      ## 2b.1 extract the default file name and extension
      file_ext <- def_file_ext
      file_name <- def_file_name
      ## 2b.2 set directory name
      dir_name <- ifelse(
        is.null(dir_name),
        yes = get0("dir_name", envir = parent.frame(1), ifnotfound = getOption("rba_dir_name")),
        no = dir_name
      )
      ## 2b.3 set file path
      save_to <- file.path(getwd(), dir_name, paste0(file_name, ".", file_ext))
    } # end of if is.character(save_to)

    ## 3 now that you have a file path...
    ## 3.1 check if a file doesn't exist with this path
    if (isFALSE(overwrite) && file.exists(save_to)) {

      ## add an incremented file
      exst_files <- list.files(
        path = dirname(save_to),
        pattern = sprintf("(^%s)(_\\d+)*(\\.%s$)", file_name, file_ext),
        full.names = FALSE
      )
      incrt <- regmatches(
        exst_files,
        regexpr(sprintf("(?<=^%s_)(\\d+)*(?=\\.%s)", file_name, file_ext), exst_files, perl = TRUE)
      )
      if (length(incrt) == 0) {
        incrt <- 1
      } else {incrt <- max(as.numeric(incrt)) + 1}

      save_to <- file.path(
        dirname(save_to),
        paste0(file_name, "_", incrt, ".", file_ext)
      )

    } else {

      ## 3.2 file doesn't exist. create the directory just in case
      ### 4 create the directory
      dir.create(dirname(save_to), showWarnings = FALSE, recursive = TRUE)

    }

    .msg(
      "Saving the server response to: \"%s\"",
      save_to
    )

  } # end if !isFALSE(save_to)
  return(save_to)
}

#### Options ####
#' Apply Per-Call rbioapi Option Overrides
#'
#' Read the allowed names from getOption("rba_user_options"). Create option
#'   variables in the calling function, using a non-NULL supplied override when
#'   present and the current package option otherwise. These assignments are
#'   limited to the current call and do not modify package options. Ignore
#'   unnamed and unknown arguments with a warning.
#'
#' Used near the start of API-facing functions to make option values available
#'   to argument validation, request construction, messaging, and file handling
#'   without changing the package options.
#'
#' @param ... Any: (optional) Named rbioapi option overrides.
#' @param ignore_save Logical: (default = FALSE) Ignore a save_file override
#'   when the calling API-facing function has its own file-saving argument.
#'
#' @return NULL invisibly. Warnings identify option overrides that were
#'   ignored.
#'
#' @family internal_options
#' @noRd
.rba_ext_args <- function(..., ignore_save = FALSE) {
  ext_args <- list(...)
  rba_opts <- getOption("rba_user_options") #available options for the end-users

  if (length(ext_args) > 0) { #user supplied something in ...

    ext_arg_names <- names(ext_args)

    if (is.null(ext_arg_names)) {
      unnamed_args <- seq_along(ext_args)
    } else {
      unnamed_args <- which(ext_arg_names == "" | is.na(ext_arg_names))
    }
    invalid_args <- which(!ext_arg_names %in% c(rba_opts, ""))

    if (length(c(unnamed_args, invalid_args)) > 0) {
      warning(
        sprintf(
          "invalid rbioapi options were ignored:%s%s",
          ifelse(
            length(unnamed_args) != 0,
            yes = sprintf(
              "\n- unnamed argument(s): %s",
              .paste2(ext_args[unnamed_args], quote = "`")
            ),
            no = ""
          ),
          ifelse(
            length(invalid_args) != 0,
            yes = sprintf(
              "\n- %s",
              .paste2(
                sprintf(
                  "%s = %s",
                  ext_arg_names[invalid_args],
                  ext_args[invalid_args]),
                last = " and ",
                quote = "`"
              )
            ),
            no = ""
          )
        ),
        call. = FALSE
      )
      ext_args <- ext_args[-c(unnamed_args, invalid_args)]

    }

    if (isTRUE(ignore_save) && utils::hasName(ext_args, "save_file")) {

      warning(
        "This function has a dedicated file-saving argument, ",
        "'save_file' option was ignored.",
        call. = FALSE
      )
      rba_opts <- rba_opts[names(rba_opts) != "rba_save_file"]

    }
  } #end of if (length(ext_args) > 0)

  # create option variables
  for (opt in rba_opts) {
    assign(
      x = opt,
      value = if (is.null(ext_args[[opt]])) {
        getOption(paste0("rba_", opt))
      } else {
        ext_args[[opt]]
      },
      envir = parent.frame(1)
    )
  }

  invisible()
}
