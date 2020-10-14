#### Lookup Endpoints ####

#' Find the species and database for several identifiers.
#' IDs that are not found are returned with no data
#'
#' @param ids
#' @param db_type
#' @param expand
#' @param format
#' @param object_type
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_lookup_id = function(ids,
                                 db_type = NA,
                                 expand = FALSE,
                                 format = "full",
                                 object_type = NA,
                                 species = NA,
                                 ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 1000),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "expand",
                               class = "logical"),
                          list(arg = "format",
                               class = "character",
                               val = c("full",
                                       "condensed")),
                          list(arg = "object_type",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"))))

  v_msg("POST lookup/id")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("db_type",
                                 !is.na(db_type),
                                 db_type),
                            list("expand",
                                 expand == TRUE,
                                 "1"),
                            list("format",
                                 format != "full",
                                 format),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type),
                            list("species",
                                 !is.na(species),
                                 species))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "lookup/id",
                           body = call_body,
                           query = call_query,
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_lookup_id.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find the species and database for a set of symbols in a linked external
#' database. Unknown symbols are omitted from the response
#'
#' @param symbols
#' @param species
#' @param expand
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param format
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_lookup_symbol = function(symbols,
                                     species,
                                     expand = FALSE,
                                     format = "full",
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "symbols",
                               class = "character",
                               max_len = 1000),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "expand",
                               class = "logical"),
                          list(arg = "format",
                               class = "character",
                               val = c("full",
                                       "condensed"))))

  v_msg("POST lookup/symbol/:species/:symbol")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"),
                            list("format",
                                 format != "full",
                                 format))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("symbols" = as.array(symbols)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("/lookup/symbol/",
                                         species),
                           body = call_body,
                           query = call_query,
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_lookup_symbol.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
