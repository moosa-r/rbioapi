#### Cross References Endpoints ####

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
#' @param external_symbol
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_symbol = function(external_symbol,
                                    species,
                                    db_type = "core",
                                    external_db = NA,
                                    object_type = NA,
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "external_symbol",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character"),
                          list(arg = "object_type",
                               class = "character")))

  v_msg("GET xrefs/symbol/:species/:symbol")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("db_type",
                                 db_type != "core",
                                 db_type),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("xrefs/symbol/",
                                         species, "/",
                                         external_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_xrefs_symbol.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param object_type
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_id = function(ensembl_id,
                                species = NA,
                                db_type = "core",
                                external_db = NA,
                                object_type = NA,
                                all_levels = FALSE,
                                ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character",
                               len = 1),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "all_levels",
                               class = "logical"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character"),
                          list(arg = "object_type",
                               class = "character")))

  v_msg("GET xrefs/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("species",
                                 !is.na(species),
                                 species),
                            list("all_levels",
                                 all_levels == TRUE,
                                 "1"),
                            list("db_type",
                                 db_type != "core",
                                 db_type),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("xrefs/id/",
                                         ensembl_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_xrefs_id.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Performs a lookup based upon the primary accession or display label of an
#' external reference and returning the information we hold about the entry
#'
#' @param name
#' @param species
#' @param db_type
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param external_db
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_name = function(name,
                                  species,
                                  db_type = "core",
                                  external_db = NA,
                                  ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "name",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character")))

  v_msg("GET xrefs/name/:species/:name")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("db_type",
                                 db_type != "core",
                                 db_type),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("xrefs/name/",
                                         species, "/",
                                         name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_xrefs_name.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
