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
#' What to cite?
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_citation = function(...) {
  ## 1 Prepare input
  input = as.character(substitute(list(...)))
  cat("-- If you used rbioapi to publish a research paper, kindly cite:\r\n",
      "---- 1- ", rba_ba_stg("citation", "rbioapi"), "\r\n",
      "-- Also, rbioapi is an interface between R and Biological databases. \r\n",
      "-- So, depending on the databases you have used, you will also have to cite R and those databases. \r\n",
      "   to cite R:\r\n",
      "---- 2- ", rba_ba_stg("citations", "r"), "\r\n")
  if (length(input) == 1) {
    cat("\r\n-- If you are not sure what papers & databases to cite, you could call rba_citation() with:\r\n",
        "   1- The name of the functions which you have used.\r\n",
        "   2- The path to your script file[s].\r\n",
        "   3- Working directory [getwd()] and/or any directories that contains your files.\r\n",
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
      cat(sprintf("-- to cite %s:\r\n---- %s- %s\r\n",
                  rba_used, seq_along(rba_used) + 2, rba_cites))
    } else {
      cat("-- Nothing further was found in your provided arguments.")
    }
  }
  invisible()
}

