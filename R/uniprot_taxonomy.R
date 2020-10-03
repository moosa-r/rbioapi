#### taxonomy Endpoints ####

#' This service returns the lowest common ancestor (LCA) of two taxonomy nodes.
#'
#' @param ...
#' @param ids
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy_ancestor = function(ids,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "numeric",
                               min_len = 2))
  )

  v_msg("get /ancestor/{ids} This service returns the lowest common ancestor (LCA) of two taxonomy nodes.")
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "taxonomy/ancestor/",
                                         paste0(ids, collapse = ",")),
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_taxonomy_ancestor.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' This service returns a list of children/siblings/parent nodes that belongs
#' to a taxonomy node with links to its parent, sibling and children nodes.
#'
#' @param ids
#' @param hierarchy
#' @param ...
#' @param node
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy = function(ids,
                                hierarchy = NA,
                                node = FALSE,
                                ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = "numeric"),
                          list(arg = "hierarchy",
                               class = "character",
                               val = c("children",
                                       "parent",
                                       "siblings")),
                          list(arg = "node",
                               class = "logical")),
              cond = list(list(quote(length(ids) > 1 && !is.na(hierarchy)),
                               "you cannot specify 'hierarchy' when providing more than 1 ids."))
  )

  v_msg("get /id/{id}/siblings etc")
  ## Build GET API Request's query
  call_query = list("size" = "-1")

  ## Build Function-Specific Call
  path_input = sprintf("%staxonomy/%s/%s",
                       rba_ba_stg("uniprot", "pth"),
                       ifelse(length(ids) > 1,
                              yes = "ids",
                              no = "id"),
                       paste0(ids, collapse = ",")
  )
  if (!is.na(hierarchy)) {
    path_input = paste0(path_input, "/", hierarchy)
  }
  if (node == TRUE) {
    path_input = paste0(path_input, "/node")
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_taxonomy.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' This service returns the taxonomic lineage for a given taxonomy node. It
#' lists the nodes as they appear in the taxonomic tree, with the more
#' specific listed first.
#'
#' @param ...
#' @param id
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy_ancestor = function(id,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = "numeric")))

  v_msg("get /lineage/{id} This service returns the taxonomic lineage for a given taxonomy node. It lists the nodes as they appear in the taxonomic tree, with the more specific listed first.")
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "taxonomy/lineage/",
                                         id),
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_taxonomy_ancestor.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#'get /path This service returns all taxonomic nodes that have a relationship
#'with the queried taxonomy ID in a specific direction (TOP or BOTTOM) and
#'depth level.
#'
#' @param id
#' @param direction
#' @param ...
#' @param depth
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy_path = function(id,
                                     direction,
                                     depth = 5,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = "numeric"),
                          list(arg = "direction",
                               class = "character",
                               val = c("TOP",
                                       "BOTTOM")),
                          list(arg = "depth",
                               class = "numeric",
                               ran = c(1,5))))

  v_msg("get /path This service returns all taxonomic nodes that have a relationship with the queried taxonomy ID in a specific direction (TOP or BOTTOM) and depth level.")

  ## Build GET API Request's query
  call_query = list("id" = id,
                    "direction" = direction,
                    "depth" = depth)

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "taxonomy/path"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("uniprot_taxonomy_path.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' This service returns the shortest path between two taxonomy nodes showing
#' their relationship
#'
#' @param from
#' @param ...
#' @param to
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy_relationship = function(from,
                                             to,
                                             ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "from",
                               class = "numeric"),
                          list(arg = "to",
                               class = "numeric"))
  )

  v_msg("get /relationship This service returns the shortest path between two taxonomy nodes showing their relationship.")

  ## Build GET API Request's query
  call_query = list("from" = from,
                    "to" = to)

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "taxonomy/relationship"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("uniprot_taxonomy_relationship.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
