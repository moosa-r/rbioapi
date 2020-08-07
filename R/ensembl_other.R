#### Archive Endpoints ####

#' Retrieve the latest version for a set of identifiers
#'
#' @param id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_Archive = function(id,
                               verbose = TRUE,
                               progress_bar = FALSE,
                               diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = id,
                                         class = "character",
                                         max_len = 1000)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST archive/id")
  }

  ## build POST API request's URL
  call_body = jsonlite::toJSON(list("id" = as.array(id)))

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_ensembl"),
                                     path = "archive/id",
                                     body = call_body,
                                     httr::accept_json(),
                                     httr::content_type("application/json")
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### 	Cross References Endpoints ####

#' Looks up an external symbol and returns all Ensembl objects linked to it.
#' This can be a display name for a gene/transcript/translation, a synonym or
#' an externally linked reference. If a gene's transcript is linked to the
#' supplied symbol the service will return both gene and transcript
#' (it supports transient links).
#'
#' @param species
#' @param db_type
#' @param external_db
#' @param object_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#' @param external_symbol
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_symbol = function(external_symbol,
                                    species,
                                    db_type = NA,
                                    external_db = NA,
                                    object_type = NA,
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = external_symbol,
                                         name = "external_symbol",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET xrefs/symbol/:species/:symbol")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("xrefs/symbol/",
                                                  species, "/",
                                                  external_symbol),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Perform lookups of Ensembl Identifiers and retrieve their external
#' references in other databases
#'
#' @param ensembl_id
#' @param species
#' @param all_levels
#' @param db_type
#' @param external_db
#' @param object_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_id = function(ensembl_id,
                                species = NA,
                                all_levels = FALSE,
                                db_type = NA,
                                external_db = NA,
                                object_type = NA,
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character",
                                         len = 1),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = all_levels,
                                         name = "all_levels",
                                         class = "logical"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET xrefs/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(all_levels == TRUE,
                              list("all_levels" = "1")),
                         list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("xrefs/id/",
                                                  ensembl_id),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Performs a lookup based upon the primary accession or display label of an
#' external reference and returning the information we hold about the entry
#'
#' @param name
#' @param species
#' @param all_levels
#' @param db_type
#' @param external_db
#' @param object_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_name = function(name,
                                  species,
                                  all_levels = FALSE,
                                  db_type = NA,
                                  external_db = NA,
                                  object_type = NA,
                                  verbose = TRUE,
                                  progress_bar = FALSE,
                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = name,
                                         name = "name",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET xrefs/name/:species/:name")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("xrefs/name/",
                                                  species, "/",
                                                  name),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

