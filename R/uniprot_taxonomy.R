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
  invisible(rba_ba_args(cons = list(list(arg = ids,
                                         name = "ids",
                                         class = "numeric",
                                         min_len = 2)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /ancestor/{ids} This service returns the lowest common ancestor (LCA) of two taxonomy nodes.")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "taxonomy/ancestor/",
                                                  paste0(ids, collapse = ",")),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

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
  invisible(rba_ba_args(cons = list(list(arg = ids,
                                         name = "id",
                                         class = "numeric"),
                                    list(arg = hierarchy,
                                         name = "hierarchy",
                                         class = "character",
                                         val = c("children",
                                                 "parent",
                                                 "siblings")),
                                    list(arg = node,
                                         name = "node",
                                         class = "logical")),
                        cond = list(list(length(ids) > 1 && !is.na(hierarchy),
                                         "you cannot specify 'hierarchy' when providing more than 1 id.")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /id/{id}/siblings etc")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  ## make function-specific calls
  if (length(ids) > 1) {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "taxonomy/ids/",
                        paste0(ids, collapse = ","))
  } else {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "taxonomy/id/",
                        ids)
  }
  if (!is.na(hierarchy)) {
    path_input = paste0(path_input, "/", hierarchy)
  }
  if (node == TRUE) {
    path_input = paste0(path_input, "/node")
  }

  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = path_input,
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

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
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = "numeric")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /lineage/{id} This service returns the taxonomic lineage for a given taxonomy node. It lists the nodes as they appear in the taxonomic tree, with the more specific listed first.")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "taxonomy/lineage/",
                                                  id),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

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
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = "numeric"),
                                    list(arg = direction,
                                         name = "direction",
                                         class = "character",
                                         val = c("TOP",
                                                 "BOTTOM")),
                                    list(arg = depth,
                                         name = "depth",
                                         class = "numeric",
                                         ran = c(1,5))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /path This service returns all taxonomic nodes that have a relationship with the queried taxonomy ID in a specific direction (TOP or BOTTOM) and depth level.")
  }

  ## build GET API request's query
  call_query = list("id" = id,
                    "direction" = direction,
                    "depth" = depth)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "taxonomy/path"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

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
  invisible(rba_ba_args(cons = list(list(arg = from,
                                         name = "from",
                                         class = "numeric"),
                                    list(arg = to,
                                         name = "to",
                                         class = "numeric")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /relationship This service returns the shortest path between two taxonomy nodes showing their relationship.")
  }

  ## build GET API request's query
  call_query = list("from" = from,
                    "to" = to)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "taxonomy/relationship"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}
