#### taxonomy Endpoints ####

#' This service returns the lowest common ancestor (LCA) of two taxonomy nodes.
#'
#' @param ids
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy_ancestor = function(ids,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "numeric",
                               min_len = 2))
  )

  v_msg(paste("get /ancestor/{ids} This service returns the lowest common ancestor (LCA) of two taxonomy nodes."))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "taxonomy/ancestor/",
                                         paste0(ids, collapse = ",")),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' This service returns a list of children/siblings/parent nodes that belongs
#' to a taxonomy node with links to its parent, sibling and children nodes.
#'
#' @param ids
#' @param hierarchy
#' @param node
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy = function(ids,
                                hierarchy = NA,
                                node = FALSE,
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE) {
  ## Check input arguments
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

  v_msg(paste("get /id/{id}/siblings etc"))
  ## build GET API request's query
  call_query = list("size" = "-1")

  ## make function-specific calls
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
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' This service returns the taxonomic lineage for a given taxonomy node. It
#' lists the nodes as they appear in the taxonomic tree, with the more
#' specific listed first.
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
rba_uniprot_taxonomy_ancestor = function(id,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = "numeric")))

  v_msg(paste("get /lineage/{id} This service returns the taxonomic lineage for a given taxonomy node. It lists the nodes as they appear in the taxonomic tree, with the more specific listed first."))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "taxonomy/lineage/",
                                         id),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#'get /path This service returns all taxonomic nodes that have a relationship
#'with the queried taxonomy ID in a specific direction (TOP or BOTTOM) and
#'depth level.
#'
#' @param id
#' @param direction
#' @param depth
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy_path = function(id,
                                     direction,
                                     depth = 5,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = "numeric"),
                          list(arg = "direction",
                               class = "character",
                               val = c("TOP",
                                       "BOTTOM")),
                          list(arg = "depth",
                               class = "numeric",
                               ran = c(1,5))))

  v_msg(paste("get /path This service returns all taxonomic nodes that have a relationship with the queried taxonomy ID in a specific direction (TOP or BOTTOM) and depth level."))

  ## build GET API request's query
  call_query = list("id" = id,
                    "direction" = direction,
                    "depth" = depth)

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "taxonomy/path"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' This service returns the shortest path between two taxonomy nodes showing
#' their relationship
#'
#' @param from
#' @param to
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_taxonomy_relationship = function(from,
                                             to,
                                             verbose = TRUE,
                                             progress_bar = FALSE,
                                             diagnostics = FALSE) {
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "from",
                               class = "numeric"),
                          list(arg = "to",
                               class = "numeric"))
  )

  v_msg(paste("get /relationship This service returns the shortest path between two taxonomy nodes showing their relationship."))

  ## build GET API request's query
  call_query = list("from" = from,
                    "to" = to)

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "taxonomy/relationship"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
