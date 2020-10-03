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

  cat("-", "Internet", ":\r\n")
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
    cat("-", names(urls)[[i]], ":\r\n")
    cat(rba_ba_api_check(urls[[i]], diagnostics = diagnostics), "\r\n")
  }
  invisible()
}

#' Set rbioapi global options
#'
#' @param client_timeout
#' @param diagnostics
#' @param dir_name
#' @param max_retries
#' @param progress_bar
#' @param skip_error
#' @param verbose
#' @param wait_time
#'
#' @return
#' @export
#'
#' @examples
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
                               "As a global option, you can only set save_resp_file as 'logical', not a file path.")))
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
