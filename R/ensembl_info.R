#### information endpoints in Ensembl API


#' List the names of analyses involved in generating Ensembl data
#'
#' @param species
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_analysis = function(species,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     store_in_options = TRUE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = "species",
                                                    name = "species",
                                                    class = c("numeric",
                                                              "character"),
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
    message("List the names of analyses involved in generating Ensembl data")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/info/analysis/",
                                                  species),
                                    httr::user_agent(getOption("rba_ua")),
                                    httr::accept_json()
  ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(as.list(jsonlite::fromJSON(httr::content(output,
                                                                         as = "text",
                                                                         encoding = "UTF-8"),
                                                           flatten = TRUE)
                                        ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' List the currently available assemblies for a species, along with toplevel
#' sequences, chromosomes and cytogenetic bands.
#'
#' @param species
#' @param bands
#' @param synonyms
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_aassembly = function(species,
                                      bands = FALSE,
                                      synonyms = FALSE,
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      store_in_options = TRUE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = species,
                                                    name = "species",
                                                    class = c("numeric",
                                                              "character"),
                                                    len = 1),
                                               list(arg = bands,
                                                    name = "bands",
                                                    class = "logical"),
                                               list(arg = synonyms,
                                                    name = "synonyms",
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
    message("GET info/assembly/:species")
  }

  ## build GET API request's query
  additional_pars = list(list(bands == TRUE,
                              list("bands" = "1")),
                         list(synonyms == TRUE,
                              list("synonyms" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/info/assembly/",
                                                  species),
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
                                                           flatten = TRUE)
                                        ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Returns information about the specified toplevel sequence region for the
#' given species.
#'
#' @param species
#' @param region_name
#' @param bands
#' @param synonyms
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_assembly_region_name = function(species,
                                                 region_name,
                                                 bands = FALSE,
                                                 synonyms = FALSE,
                                                 verbose = TRUE,
                                                 progress_bar = FALSE,
                                                 store_in_options = TRUE,
                                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = species,
                                                    name = "species",
                                                    class = c("numeric",
                                                              "character"),
                                                    len = 1),
                                               list(arg = region_name,
                                                    name = "region_name",
                                                    class = c("numeric",
                                                              "character"),
                                                    len = 1),
                                               list(arg = bands,
                                                    name = "bands",
                                                    class = "logical"),
                                               list(arg = synonyms,
                                                    name = "synonyms",
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
    message("GET info/assembly/:species/:region_name")
  }

  ## build GET API request's query
  additional_pars = list(list(bands == TRUE,
                              list("bands" = "1")),
                         list(synonyms == TRUE,
                              list("synonyms" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/info/assembly/",
                                                  species,
                                                  "/",
                                                  region_name),
                                    query = call_query,
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

  return(final_output)
}

#'  List the functional classifications of gene models that Ensembl associates
#'  with a particular species. Useful for restricting the type of
#'  genes/transcripts retrieved by other endpoints.
#'
#' @param species
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes = function(species,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     store_in_options = TRUE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = "species",
                                                    name = "species",
                                                    class = c("numeric",
                                                              "character"),
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
    message("GET info/biotypes/:species")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/info/biotypes/",
                                                  species),
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

  return(final_output)
}
