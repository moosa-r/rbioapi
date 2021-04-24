#### taxonomy Endpoints ####

#' Get Lowest Common Ancestor (LCA) of Two Taxonomy Nodes
#'
#' Use this function to retrieve lowest common ancestor (LCA) of two
#'  taxonomy nodes in
#'  \href{UniProt Taxonomy database}{https://www.uniprot.org/help/taxonomy}
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/ancestor/{ids}"
#'
#' @param ids (numeric) Numeric vector of
#'   \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifiers}, with minimum length of two.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list with UniProt taxonomy information of your provided taxonomy
#'   elements.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_taxonomy_lca(c(9606,10090,9823,7712))
#' @family "UniProt API, Taxonomy"
#' @export
rba_uniprot_taxonomy_lca <- function(ids,
                                     ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "ids",
                             class = "numeric",
                             min_len = 2))
  )
  .msg("Retrieving LCA of ", .paste2(ids, sep = ", ", last = " and "))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "taxonomy/ancestor/",
                                        paste0(ids, collapse = ",")),
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("uniprot_taxonomy_lca.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt Taxonomy Nodes
#'
#' Using this function, you can retrieve taxonomic nodes information by
#' providing their
#' \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifiers}. also, you can explicitly retrieve other nodes in relation
#'   to your provided node's hierarchy in
#'   \href{UniProt Taxonomy database}{https://www.uniprot.org/help/taxonomy}.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/ids/{ids}"
#'  "GET https://ebi.ac.uk/proteins/api/ids//id/{id}/node"
#'  "GET https://ebi.ac.uk/proteins/api/id/{id}/node"
#'  "GET https://ebi.ac.uk/proteins/api/id/{id}/children"
#'  "GET https://ebi.ac.uk/proteins/api/id/{id}/children/node"
#'  "GET https://ebi.ac.uk/proteins/api/id/{id}/parent"
#'  "GET https://ebi.ac.uk/proteins/api/id/{id}/parent/node"
#'  "GET https://ebi.ac.uk/proteins/api/id/{id}/siblings"
#'  "GET https://ebi.ac.uk/proteins/api/id/{id}/siblings/node"
#'
#' @param ids (numeric) a single or a numeric vector of
#'   \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifier(s)}
#' @param hierarchy Retrieve taxonomic nodes that have specific hierarchical
#'   relation to your provided taxonomic node. should be one of: "children",
#'   "parent" or "siblings".
#' @param node_only Retrieve only the node(s) information and exclude URL links
#'  to parents, siblings and children nodes.
#' @param page_size (numeric) Only when hierarchy is provided. hierarchy
#'  information may be very long, thus UniProt API will paginate the results,
#'  you may use this argument to control the pagination. maximum value is 200.
#' @param page_number (numeric) Only when hierarchy is provided. hierarchy
#'  information may be very long, thus UniProt API will paginate the results,
#'  you may use this argument to control the pagination.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return a list containing your provided nodes or their related nodes
#'   taxonomic information.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_taxonomy(ids = c(9606, 10090))
#' rba_uniprot_taxonomy(ids = 9989, hierarchy = "children")
#' @family "UniProt API, Taxonomy"
#' @export
rba_uniprot_taxonomy <- function(ids,
                                 hierarchy = NA,
                                 node_only = TRUE,
                                 page_size = 200,
                                 page_number = 1,
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "ids",
                             class = "numeric"),
                        list(arg = "hierarchy",
                             class = "character",
                             val = c("children",
                                     "parent",
                                     "siblings")),
                        list(arg = "node_only",
                             class = "logical"),
                        list(arg = "page_size",
                             class = "numeric",
                             ran = c(1,200)),
                        list(arg = "page_number",
                             class = "numeric")),
            cond = list(list(quote(length(ids) > 1 && !is.na(hierarchy)),
                             "you cannot specify 'hierarchy' when providing more than 1 ids."),
                        list(quote(is.na(hierarchy) && (page_size != 200 | page_number != 1)),
                             "Because hierarchy argument was not provided, page_size and page_number were ignored.",
                             warn = TRUE))
  )

  .msg("Retrieving %snodes information of %s.",
       ifelse(!is.na(hierarchy),
              yes = hierarchy,
              no = ""),
       .paste2(ids, sep = ", ", last = " and "))
  ## Build GET API Request's query
  call_query <- list()
  ## Build Function-Specific Call
  path_input <- sprintf("%staxonomy/%s/%s",
                        .rba_stg("uniprot", "pth"),
                        ifelse(length(ids) > 1,
                               yes = "ids",
                               no = "id"),
                        paste0(ids, collapse = ",")
  )
  if (!is.na(hierarchy)) {
    path_input <- paste0(path_input, "/", hierarchy)
    ## Build GET API Request's query
    call_query <- list("pageSize" = page_size,
                       "pageNumber" = page_number)
  }
  if (isTRUE(node_only)) {
    path_input <- paste0(path_input, "/node")
  }

  parser_input <- ifelse(isTRUE(node_only),
                         yes = "json->list_simp",
                         no = "json->list")
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_taxonomy.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Taxonomic Lineage
#'
#' Use this function to retrieve the taxonomic lineage of your provided
#'   taxonomy node.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/lineage/{id}"
#'  "GET https://ebi.ac.uk/proteins/api/lineage/{id}"
#'
#' @param id (numeric) a
#' \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifier}
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list with a data frame containing All the nodes that preceded your
#'   provided node in the taxonomic tree. with your node as the first row
#'   and the root node in the last row.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_taxonomy_lineage(id = 9989)
#' @family "UniProt API, Taxonomy"
#' @export
rba_uniprot_taxonomy_lineage <- function(id,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "id",
                             class = "numeric")))

  .msg("Retrieving Taxonomic Lineage of node %s.", id)
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "taxonomy/lineage/",
                                        id),
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("rba_uniprot_taxonomy_lineage.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search UniProt Taxonomic Names
#'
#' Using this function, you can search and retrieve taxonomic nodes using
#'   their names from
#'   \href{UniProt Taxonomy database}{https://www.uniprot.org/help/taxonomy}.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/name/{name}"
#'  "GET https://ebi.ac.uk/proteins/api/name/{name}/node"
#'
#' @param name a name to to be used as search query.
#' @param field Specify the field that your provided name should be searched.
#'   It should be one of : "scientific" (default), "common" or "mnemonic".
#' @param search_type The logical relationship between your provided search
#'   query and the taxanomic name field. It should be one of "equal_to"
#'   (default), "start_with", "end_with" or "contain".
#' @param node_only (logical) Retrieve only the node(s) information and exclude URL links
#'  to parents, siblings and children nodes. default = TRUE
#' @param page_size (numeric) Your search results may be very long, thus
#'  UniProt API will paginate the results, you may use this argument to control
#'  the pagination. maximum value is 200.
#' @param page_number (numeric) Your search results may be very long, thus
#'  UniProt API will paginate the results, you may use this argument to control
#'  the pagination. maximum value is 200.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return a list containing taxonomic nodes that match your provided inputs.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_taxonomy_name(name = "homo", field = "scientific",
#'   search_type = "start_with")
#' rba_uniprot_taxonomy_name(name = "adenovirus", field = "scientific",
#'   search_type = "contain", page_size = 200, page_number = 2)
#' @family "UniProt API, Taxonomy"
#' @export
rba_uniprot_taxonomy_name <- function(name,
                                      field = "scientific",
                                      search_type = "equal_to",
                                      node_only = TRUE,
                                      page_size = 200,
                                      page_number = 1,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "name",
                             class = "character"),
                        list(arg = "field",
                             class = "character",
                             val = c("scientific",
                                     "common",
                                     "mnemonic")),
                        list(arg = "search_type",
                             class = "character",
                             val = c("equal_to",
                                     "start_with",
                                     "end_with",
                                     "contain")),
                        list(arg = "node_only",
                             class = "logical"),
                        list(arg = "page_size",
                             class = "numeric",
                             ran = c(1,200)),
                        list(arg = "page_number",
                             class = "numeric")))

  .msg("Retrieving taxonomic nodes that their %s name field %s %s (page %s).",
       field, search_type, name, page_number)
  ## Build GET API Request's query
  call_query <- list("name" = name,
                     "fieldName" = switch(field,
                                          "scientific" = "SCIENTIFICNAME",
                                          "common" = "COMMONNAME",
                                          "mnemonic" = "MNEMONIC"),
                     "searchType" = switch(search_type,
                                           "equal_to"  = "EQUALSTO",
                                           "start_with" = "STARTSWITH",
                                           "end_with" = "ENDSWITH",
                                           "contain" = "CONTAINS"),
                     pageSize = page_size,
                     pageNumber = page_number)
  ## Build Function-Specific Call
  path_input <- sprintf("%staxonomy/name/%s",
                        .rba_stg("uniprot", "pth"),
                        name)
  if (isTRUE(node_only)) {
    path_input <- paste0(path_input, "/node")
  }
  parser_input <- ifelse(node_only,
                         yes = "json->list_simp",
                         no = "json->list")

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_taxonomy_name.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Traverse UniProt Taxonomic Tree Path
#'
#' Using this function you can retrieve nodes that are located in the top or
#'   the bottom of your provided node in
#'   \href{UniProt Taxonomy database tree}{https://www.uniprot.org/help/taxonomy}
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/path"
#'
#' @param id (numeric) a
#' \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifier}
#' @param direction direction of the taxonomic path, either "TOP" or "BOTTOM".
#' @param depth (numeric) How many levels should be traversed on
#' the taxonomic tree? (from 1 to 5, default = 5)
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return a nested list containing the node which are in the path specified by
#'   your provided argument in the UniProt taxonomic tree.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_taxonomy_path(id = 9606, direction = "TOP", depth = 3)
#' rba_uniprot_taxonomy_path(id = 207598, direction = "BOTTOM", depth = 3)
#' @family "UniProt API, Taxonomy"
#' @export
rba_uniprot_taxonomy_path <- function(id,
                                      direction,
                                      depth = 5,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "id",
                             class = "numeric"),
                        list(arg = "direction",
                             class = "character",
                             val = c("TOP",
                                     "BOTTOM")),
                        list(arg = "depth",
                             class = "numeric",
                             ran = c(1,5))))

  .msg("Retrieving the %s steps of nodes that are in the %s of %s node.",
       depth, direction, id)

  ## Build GET API Request's query
  call_query <- list("id" = id,
                     "direction" = direction,
                     "depth" = depth)

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "taxonomy/path"),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_taxonomy_path.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Shortest Path Between Two Taxonomy Nodes
#'
#' Use this function to retrieve the shortest path between two nodes
#' in the taxonomy tree of
#' \href{UniProt Taxonomy database}{https://www.uniprot.org/help/taxonomy}.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/relationship"
#'
#' @param from \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI
#' taxonomic identifier} of your initial node.
#' @param to \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI
#' taxonomic identifier} of your final node.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return a nested list containing the node which are in the shortest path
#'   between your provided nodes.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_taxonomy_relationship(from = 9606, to = 10090)
#' @family "UniProt API, Taxonomy"
#' @export
rba_uniprot_taxonomy_relationship <- function(from,
                                              to,
                                              ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "from",
                             class = "numeric"),
                        list(arg = "to",
                             class = "numeric"))
  )

  .msg("Retrieving the shortest path on the toxonomy tree from node %s to %s.",
       from, to)

  ## Build GET API Request's query
  call_query <- list("from" = from,
                     "to" = to)

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "taxonomy/relationship"),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_taxonomy_relationship.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
