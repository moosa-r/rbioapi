#' Retrieve a List of available libraries from Enrichr
#' @description This function will retrieve a list of available libraries and
#' their statistics from Enrichr. You should call this function once per session
#' with argument 'store_in_options = TRUE' before querying data from Enrichr.
#' Nevertheless, rbioapi will do this for you in the background in the first
#' time you call any function pertinent to Enrichr.
#'
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_info = function(verbose = TRUE,
                            progress_bar = FALSE,
                            store_in_options = TRUE,
                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  if (verbose == TRUE){
    message("Retrieving List of available libraries and statistics of Enrichr.")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_enrichr"),
                                    path = "Enrichr/datasetStatistics",
                                    httr::user_agent(getOption("rba_ua")),
                                    httr::accept_json()
  ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  ## save library names as global options
  if (store_in_options == TRUE && is.null(getOption("rba_enrichr_libs"))) {
    options(rba_enrichr_libs = final_output[["statistics.libraryName"]])

  }
  return(final_output)
}

#' Upload your gene set to Enrichr
#'
#' @param description
#' @param verbose
#' @param progress_bar
#' @param gene_list
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_add_list = function(gene_list,
                                description = NA,
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = gene_list,
                                                    name = "gene_list",
                                                    class = "character"),
                                               list(arg = description,
                                                    name = "description",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  if (verbose == TRUE){
    message("Uploading ", length(gene_list),
            " gene IDs to Enrichr.")
  }

  ## build POST API request's URL
  call_body = list("format" = "text",
                   "list" = paste(unique(gene_list),collapse = "\n"))

  additional_pars = list(list(!is.na(description),
                              list("description" = description)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_enrichr"),
                                     path = "Enrichr/addList",
                                     body = call_body,
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
  ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)


  response_parser_input = quote(as.list(jsonlite::fromJSON(httr::content(output,
                                                                         as = "text",
                                                                         encoding = "UTF-8"),
                                                           flatten = TRUE)))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

#' View your uploaded gene list
#'
#' @param verbose
#' @param progress_bar
#' @param user_list_id
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_view_list = function(user_list_id,
                                 verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = user_list_id,
                                                    name = "user_list_id",
                                                    class = c("numeric", "integer"),
                                                    len = 1),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  if (verbose == TRUE){
    message("Retrieving your uploaded gene list under the provided ID.")
  }

  ## build GET API request's query
  call_query = list("userListId" = user_list_id)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_enrichr"),
                                    path = "Enrichr/view",
                                    query = call_query,
                                    httr::user_agent(getOption("rba_ua")),
                                    httr::accept_json()
  ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)


  response_parser_input = quote(as.list(jsonlite::fromJSON(httr::content(output,
                                                                         as = "text",
                                                                         encoding = "UTF-8"),
                                                           flatten = TRUE)))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

#' Internal function for rba_enrichr_enrich
#'
#' @param user_list_id_input
#' @param gene_set_library_input
#' @param verbose_input
#' @param progress_bar_input
#' @param diagnostics_input
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_enrich_internal = function(user_list_id_input,
                                       gene_set_library_input,
                                       verbose_input = FALSE,
                                       progress_bar_input = FALSE,
                                       diagnostics_input = FALSE) {
  ## build GET API request's query
  call_query = list("userListId" = user_list_id_input,
                    "backgroundType" = gene_set_library_input)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_enrichr"),
                                    path = "Enrichr/export",
                                    query = call_query,
                                    httr::accept("text/tab-separated-values"),
                                    httr::user_agent(getOption("rba_ua"))
  ))


  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics_input,
                                         progress_bar = progress_bar_input)


  response_parser_input = quote(httr::content(output,
                                              as = "text",
                                              type = "text/tab-separated-values",
                                              encoding = "UTF-8"))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose_input,
                                  diagnostics = diagnostics_input)

  final_output = utils::read.delim(textConnection(final_output),
                                   sep = "\t", header = TRUE)
  return(final_output)
}

#' Get Enrichr enrichment results
#'
#' @param user_list_id
#' @param verbose
#' @param gene_set_library
#' @param multi_libs_progress_bar
#' @param diagnostics
#' @param progress_bar
#' @param regex_library_name
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_enrich = function(user_list_id,
                              gene_set_library = "all",
                              regex_library_name = FALSE,
                              verbose = TRUE,
                              multi_libs_progress_bar = TRUE,
                              progress_bar = FALSE,
                              diagnostics = FALSE){

  # get a list of available
  if (is.null(getOption("rba_enrichr_libs"))){
    if (verbose == TRUE) {
      message("Calling rba_enrichr_info() to get a list of available enricr libraries.")
    }
    invisible(rba_enrichr_info(verbose = FALSE,
                               progress_bar = FALSE,
                               store_in_options = TRUE,
                               diagnostics = FALSE))
  }

  #### handle different gene_set_library input situations
  if (length(gene_set_library) > 1) {
    run_mode = "multiple"

  } else if (gene_set_library == "all") {
    run_mode = "multiple"
    gene_set_library = getOption("rba_enrichr_libs")

  } else {
    if (regex_library_name == FALSE) {
      run_mode = "single"
    } else {
      gene_set_library = grep(gene_set_library,
                              getOption("rba_enrichr_libs"),
                              ignore.case = TRUE, value = TRUE)
      #check the results of regex
      if (length(gene_set_library) == 0) {
        stop("Your regex pattern did not match any enrichr library name.")
      } else if (length(gene_set_library) == 1) {
        run_mode = "single"
      } else if (length(gene_set_library) > 1) {
        run_mode = "multiple"
      }
    }
  }

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = user_list_id,
                                                    name = "user_list_id",
                                                    class = c("numeric", "integer"),
                                                    len = 1),
                                               list(arg = gene_set_library,
                                                    name = "gene_set_library",
                                                    class = "character",
                                                    val = getOption("rba_enrichr_libs")),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = multi_libs_progress_bar,
                                                    name = "multi_libs_progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))
  ## call Enrichr API
  if (run_mode == "single") {
    if (verbose == TRUE) {
      message("Enriching Gene set ", user_list_id,
              " using Enrichr library: ", gene_set_library)
    }

    final_output = rba_enrichr_enrich_internal(user_list_id_input = user_list_id,
                                               gene_set_library_input = gene_set_library,
                                               verbose_input = diagnostics,
                                               progress_bar_input = progress_bar,
                                               diagnostics_input = diagnostics)
    return(final_output)

  } else if (run_mode == "multiple") {
    if (verbose == TRUE) {
      message("Enriching Gene set ", user_list_id,
              " using multiple Enrichr libraries:") }

    message("note: You have selected ", length(gene_set_library),
            " Enrichr libraries. ",
            "note that for each library, a seperate call should be send to the ",
            "Enrichr server. thus, this could take a while depending on the ",
            "number of selected libraries and your network connection.")

    final_output = as.list(gene_set_library)
    names(final_output) = gene_set_library

    ## initiate progress bar
    if (multi_libs_progress_bar == TRUE) {
      pb = utils::txtProgressBar(min = 0,
                                 max = length(gene_set_library),
                                 style = 3)
    }

    final_output = purrr::map(final_output, function(x){
      lib_enrich_res = rba_enrichr_enrich_internal(user_list_id_input = user_list_id,
                                                   gene_set_library_input = x,
                                                   verbose_input = verbose,
                                                   progress_bar_input = progress_bar,
                                                   diagnostics_input = diagnostics)
      #advance the progress bar
      if (multi_libs_progress_bar == TRUE) {
        utils::setTxtProgressBar(pb, which(final_output == x))
      }
      return(lib_enrich_res)
    })
    close(pb)
    return(final_output)
  }

}

#' Find terms that contain a given gene
#'
#' @param gene
#' @param catagorize
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_gene_map = function(gene,
                                catagorize = FALSE,
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = gene,
                                                    name = "gene",
                                                    class = "character",
                                                    len = 1),
                                               list(arg = catagorize,
                                                    name = "catagorize",
                                                    class = "logical"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  if (verbose == TRUE){
    message("Finding terms that contain gene: ", gene)
  }

  ## build GET API request's query
  call_query = list("gene" = gene,
                    "json" = "true")

  additional_pars = list(list(catagorize == TRUE,
                              list("setup" = "true")))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_enrichr"),
                                    path = "Enrichr/genemap",
                                    query = call_query,
                                    httr::user_agent(getOption("rba_ua")),
                                    httr::accept_json()
  ))


  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)


  response_parser_input = quote(as.list(jsonlite::fromJSON(httr::content(output,
                                                                         as = "text",
                                                                         encoding = "UTF-8"),
                                                           flatten = TRUE)))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

#' Gene Set Enrichment Using Enrichr With One Function Call
#'
#' @param gene_list
#' @param description
#' @param gene_set_library
#' @param regex_library_name
#' @param multi_libs_progress_bar
#' @param progress_bar
#' @param verbose
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr = function(gene_list,
                       description = NA,
                       gene_set_library = "all",
                       regex_library_name = FALSE,
                       multi_libs_progress_bar = TRUE,
                       progress_bar = FALSE,
                       verbose = TRUE,
                       diagnostics = FALSE) {

  invisible(rba_ba_arguments_check(cons = list(list(arg = gene_list,
                                                    name = "gene_list",
                                                    class = "character"),
                                               list(arg = description,
                                                    name = "description",
                                                    class = "character"),
                                               list(arg = regex_library_name,
                                                    name = "regex_library_name",
                                                    class = "logical"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = multi_libs_progress_bar,
                                                    name = "multi_libs_progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  if (verbose == TRUE){
    message("--Step 1:")
  }
  invisible(rba_enrichr_info(verbose = verbose,
                             progress_bar = progress_bar,
                             store_in_options = TRUE,
                             diagnostics = FALSE))

  if (verbose == TRUE){
    message("--Step 2:")
  }
  list_id = rba_enrichr_add_list(gene_list = gene_list,
                                 description = description,
                                 verbose = verbose,
                                 progress_bar = progress_bar,
                                 diagnostics = diagnostics)

  if (verbose == TRUE){
    message("--Step 3:")
  }
  enriched = rba_enrichr_enrich(user_list_id = list_id$userListId,
                                gene_set_library = gene_set_library,
                                regex_library_name = regex_library_name,
                                verbose = verbose,
                                multi_libs_progress_bar = multi_libs_progress_bar,
                                progress_bar = progress_bar,
                                diagnostics = diagnostics)
  return(enriched)
}
