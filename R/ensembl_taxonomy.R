#### Taxonomy Endpoints ####

#' Return the taxonomic classification of a taxon node
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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

#' Search for a taxonomic term by its identifier, name or non-scientific name
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param simple
#' @param taxon
#' @param non_scientific_name
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_search = function(taxon = NA,
                                       non_scientific_name = NA,
                                       simple = FALSE,
                                       ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "taxon",
                               class = c("numeric",
                                         "character")),
                          list(arg = "non_scientific_name",
                               class = "character"),
                          list(arg = "simple",
                               class = "logical")),
              cond = list(list(quote(sum(!is.na(taxon), !is.na(non_scientific_name)) != 1),
                               "You shoul provide one of 'taxon' or 'non_scientific_name'."),
                          list(quote(isTRUE(simple) && is.na(taxon)),
                               "You can only set 'simple' to 'TRUE' when providing 'taxon'")))

  v_msg("GET taxonomy/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("simple",
                                 simple,
                                 "1"))
  ## Build Function-Specific Call
  path_input = ifelse(!is.na(taxon),
                      yes = paste0("taxonomy/id/", taxon),
                      no = paste0("taxonomy/name/", non_scientific_name))
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_taxonomy_search.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

