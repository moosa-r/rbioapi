#### Ontologies Endpoints ####

#' Reconstruct the entire ancestry of a term from is_a and part_of relationships
#'
#' @param term_id
#' @param chart
#' @param ...
#' @param ontology
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_ancestors = function(term_id,
                                          chart = FALSE,
                                          ontology = NA,
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "term_id",
                               class = "character"),
                          list(arg = "chart",
                               class = "logical"),
                          list(arg = "ontology",
                               class = "character")))

  v_msg("GET ontology/ancestors/chart/:id \r\n",
        "GET ontology/ancestors/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("ontology",
                                 !is.na(ontology),
                                 ontology))

  ## Build Function-Specific Call
  if (chart == TRUE) {
    path_input = paste0("ontology/ancestors/chart/",
                        term_id)
    parser_input = "json->list_simp"
  } else {
    path_input = paste0("ontology/ancestors/",
                        term_id)
    parser_input = "json->df"
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = parser_input,
                           save_to = rba_ba_file("ensembl_ontology_ancestors.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find all the terms descended from a given term. By default searches are
#' conducted within the namespace of the given identifier
#'
#' @param term_id
#' @param closest_term
#' @param ontology
#' @param subset
#' @param ...
#' @param zero_distance
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_descendants = function(term_id,
                                            closest_term = FALSE,
                                            ontology = NA,
                                            subset = NA,
                                            zero_distance = NA,
                                            ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "term_id",
                               class = "character"),
                          list(arg = "closest_term",
                               class = "logical"),
                          list(arg = "ontology",
                               class = "character"),
                          list(arg = "subset",
                               class = "character"),
                          list(arg = "zero_distance",
                               class = "logical")))

  v_msg("GET ontology/descendants/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("closest_term",
                                 closest_term = TRUE,
                                 "1"),
                            list("ontology",
                                 !is.na(ontology),
                                 ontology),
                            list("subset",
                                 !is.na(subset),
                                 subset),
                            list("zero_distance",
                                 zero_distance = TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ontology/descendants/",
                                         term_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_ontology_descendants.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Search for an ontological term by its namespaced identifier
#'
#' @param term_id
#' @param relation
#' @param ...
#' @param simple
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_id = function(term_id,
                                   relation = NA,
                                   simple = FALSE,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "term_id",
                               class = "character"),
                          list(arg = "relation",
                               class = "character"),
                          list(arg = "simple",
                               class = "logical")))

  v_msg("GET ontology/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("relation",
                                 !is.na(relation),
                                 relation),
                            list("simple",
                                 simple = TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ontology/id/",
                                         term_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_ontology_id.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Search for a list of ontological terms by their name
#'
#' @param name
#' @param ontology
#' @param relation
#' @param ...
#' @param simple
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_name = function(name,
                                     ontology = NA,
                                     relation = NA,
                                     simple = FALSE,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "name",
                               class = "character"),
                          list(arg = "ontology",
                               class = "character"),
                          list(arg = "relation",
                               class = "character"),
                          list(arg = "simple",
                               class = "logical")))

  v_msg("GET ontology/name/:name")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("ontology",
                                 !is.na(ontology),
                                 ontology),
                            list("relation",
                                 !is.na(relation),
                                 relation),
                            list("simple",
                                 simple = TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ontology/name/",
                                         name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_ontology_name.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
