#### Taxonomy Endpoints ####

#' Return the taxonomic classification of a taxon node
#'
#' @param ...
#' @param taxon_id
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_classification = function(taxon_id,
                                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "taxon_id",
                               class = c("numeric",
                                         "character"))))

  v_msg("GET taxonomy/classification/:id")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("taxonomy/classification/",
                                         taxon_id),
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_taxonomy_classification.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Search for a taxonomic term by its identifier or name
#'
#' @param taxon_id
#' @param ...
#' @param simple
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_id = function(taxon_id,
                                   simple = FALSE,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "taxon_id",
                               class = c("numeric",
                                         "character")),
                          list(arg = "simple",
                               class = "logical")))

  v_msg("GET taxonomy/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("simple",
                                 simple = TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("taxonomy/id/",
                                         taxon_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_taxonomy_id.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Search for a taxonomic id by a non-scientific name
#'
#' @param ...
#' @param name
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_name = function(name,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "name",
                               class = "character")))

  v_msg("GET taxonomy/name/:name")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("taxonomy/name/",
                                         name),
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_taxonomy_name.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
