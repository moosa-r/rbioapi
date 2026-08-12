#### Taxonomy Endpoints ####

#' Get Lowest Common Ancestor (LCA) of Two Taxonomy Nodes
#'
#' Retrieve the lowest common ancestor (LCA) of two or more nodes in the
#'   \href{https://www.uniprot.org/help/taxonomy}{UniProt Taxonomy database}.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/taxonomy/ancestor/\{ids\}"
#'
#' @param ids Numeric:
#'   \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifiers}. Supply at least two IDs.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the lowest common ancestor's taxonomy information.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_taxonomy_lca(c(9606,10090,9823,7712))
#' }
#'
#' @family "UniProt - Taxonomy"
#' @export
rba_uniprot_taxonomy_lca <- function(ids,
                                     ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "ids", class = c("numeric", "integer"),
        min_len = 2, integerish = TRUE, min_val = 1
      )
    )
  )

  .msg(
    "Retrieving LCA of ", .paste2(ids, sep = ", ", last = " and ")
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "taxonomy/ancestor/", paste0(ids, collapse = ",")),
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("uniprot_taxonomy_lca.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt Taxonomy Nodes
#'
#' Retrieve taxonomic-node information using
#' \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifiers}. You can also retrieve nodes related to one supplied node in
#'   \href{https://www.uniprot.org/help/taxonomy}{UniProt Taxonomy database}.
#'   Child and sibling results are paginated.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/\{id\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/ids/\{ids\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/ids/\{ids\}/node"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/\{id\}/node"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/\{id\}/children"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/\{id\}/children/node"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/\{id\}/parent"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/\{id\}/parent/node"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/\{id\}/siblings"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/\{id\}/siblings/node"
#'
#' @param ids Numeric: One or more
#'   \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifiers}.
#' @param hierarchy Character: (optional) Retrieve nodes related to one supplied
#'   node. One of "children", "parent", or "siblings".
#' @param node_only Logical: (default = \code{TRUE}) If \code{TRUE}, return
#'   node information without links to parent, sibling, and child nodes.
#' @param page_size Numeric: (default = \code{200}) Number of child or sibling
#'   nodes per page. The maximum is 200.
#' @param page_number Numeric: (default = \code{1}) Page of child or sibling
#'   nodes to retrieve.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing taxonomy information for the requested nodes or
#'   their related nodes.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_taxonomy(ids = c(9606, 10090))
#' }
#' \donttest{
#' rba_uniprot_taxonomy(ids = 9989, hierarchy = "children")
#' }
#'
#' @family "UniProt - Taxonomy"
#' @export
rba_uniprot_taxonomy <- function(ids,
                                 hierarchy = NULL,
                                 node_only = TRUE,
                                 page_size = 200,
                                 page_number = 1,
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "ids", class = c("numeric", "integer"),
        min_len = 1L, integerish = TRUE, min_val = 1
      ),
      list(
        arg = "hierarchy", class = "character",
        len = 1L, val = c("children", "parent", "siblings")
      ),
      list(
        arg = "node_only", class = "logical", len = 1L,
        no_null = TRUE
      ),
      list(
        arg = "page_size", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, ran = c(1, 200), no_null = TRUE
      ),
      list(
        arg = "page_number", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, min_val = 1, no_null = TRUE
      )
    ),
    cond = list(
      list(
        quote(length(ids) > 1 && !is.null(hierarchy)),
        "`hierarchy` can be used only when one taxonomy ID is supplied."
      ),
      list(
        quote(
          (is.null(hierarchy) || identical(hierarchy, "parent")) &&
            (page_size != 200 || page_number != 1)
        ),
        "`page_size` and `page_number` are ignored unless `hierarchy` is `children` or `siblings`.",
        warn = TRUE
      )
    )
  )

  .msg(
    "Retrieving %snodes information of %s.",
    ifelse(!is.null(hierarchy), yes = hierarchy, no = ""),
    .paste2(ids, sep = ", ", last = " and ")
  )

  ## Build GET API Request's query
  call_query <- NULL

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%staxonomy/%s/%s",
    .rba_stg("uniprot", "pth"),
    ifelse(length(ids) > 1, yes = "ids", no = "id"),
    paste0(ids, collapse = ",")
  )

  if (!is.null(hierarchy)) {
    path_input <- paste0(path_input, "/", hierarchy)
  }

  if (!is.null(hierarchy) && hierarchy %in% c("children", "siblings")) {
    call_query <- list("pageSize" = page_size, "pageNumber" = page_number)
  }

  if (isTRUE(node_only)) {
    path_input <- paste0(path_input, "/node")
  }

  parser_input <- ifelse(
    isTRUE(node_only),
    yes = "json->list_simp",
    no = "json->list"
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_taxonomy.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Taxonomic Lineage
#'
#' Use this function to retrieve the taxonomic lineage of your supplied
#'   taxonomy node.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/taxonomy/lineage/\{id\}"
#'
#' @param id Numeric: An
#' \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifier}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the requested node's lineage, ordered from the
#'   supplied node to the root.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_taxonomy_lineage(id = 9989)
#' }
#'
#' @family "UniProt - Taxonomy"
#' @export
rba_uniprot_taxonomy_lineage <- function(id,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "id", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, min_val = 1
      )
    )
  )

  .msg(
    "Retrieving Taxonomic Lineage of node %s.",
    id
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "taxonomy/lineage/", id),
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("uniprot_taxonomy_lineage.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search UniProt Taxonomic Names
#'
#' Search and retrieve taxonomic nodes by name from the
#'   \href{https://www.uniprot.org/help/taxonomy}{UniProt Taxonomy database}.
#'   Search results are paginated.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/taxonomy/name/\{name\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/taxonomy/name/\{name\}/node"
#'
#' @param name Character: Taxonomic name to search.
#' @param field Character: (default = \code{"scientific"}) Name field to
#'   search. One of "scientific", "common", or "mnemonic".
#' @param search_type Character: (default = \code{"equal_to"}) Relationship
#'   between the query and taxonomic name. One of "equal_to", "start_with",
#'   "end_with", or "contain".
#' @param node_only Logical: (default = \code{TRUE}) If \code{TRUE}, return
#'   node information without links to parent, sibling, and child nodes.
#' @param page_size Numeric: (default = \code{200}) Number of results per page.
#'   The maximum is 200.
#' @param page_number Numeric: (default = \code{1}) Page to retrieve.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing matching taxonomic nodes and page information.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_taxonomy_name(name = "homo", field = "scientific",
#'     search_type = "start_with")
#' }
#' \donttest{
#' rba_uniprot_taxonomy_name(name = "adenovirus", field = "scientific",
#'     search_type = "contain", page_size = 200, page_number = 2)
#' }
#'
#' @family "UniProt - Taxonomy"
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
  .rba_args(
    cons = list(
      list(arg = "name", class = "character", len = 1L),
      list(
        arg = "field", class = "character", len = 1L, no_null = TRUE,
        val = c("scientific", "common", "mnemonic")
      ),
      list(
        arg = "search_type", class = "character", len = 1L, no_null = TRUE,
        val = c("equal_to", "start_with", "end_with", "contain")
      ),
      list(
        arg = "node_only", class = "logical", len = 1L,
        no_null = TRUE
      ),
      list(
        arg = "page_size", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, ran = c(1, 200), no_null = TRUE
      ),
      list(
        arg = "page_number", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, min_val = 1, no_null = TRUE
      )
    )
  )

  .msg(
    "Retrieving taxonomic nodes that their %s name field %s %s (page %s).",
    field, search_type, name, page_number
  )

  ## Build GET API Request's query
  call_query <- list(
    "fieldName" = switch(
      field,
      "scientific" = "SCIENTIFICNAME",
      "common" = "COMMONNAME",
      "mnemonic" = "MNEMONIC"
    ),
    "searchType" = switch(
      search_type,
      "equal_to"  = "EQUALSTO",
      "start_with" = "STARTSWITH",
      "end_with" = "ENDSWITH",
      "contain" = "CONTAINS"
    ),
    pageSize = page_size,
    pageNumber = page_number
  )

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%staxonomy/name/%s",
    .rba_stg("uniprot", "pth"),
    name
  )

  if (isTRUE(node_only)) {
    path_input <- paste0(path_input, "/node")
  }

  parser_input <- ifelse(
    node_only,
    yes = "json->list_simp",
    no = "json->list"
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_taxonomy_name.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Traverse UniProt Taxonomic Tree Path
#'
#' Traverse upward or downward from a supplied node in the
#'   \href{https://www.uniprot.org/help/taxonomy}{UniProt Taxonomy database
#'   tree}.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/taxonomy/path"
#'
#' @param id Numeric: An
#' \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifier}.
#' @param direction Character: Direction of the taxonomic path, either "TOP"
#'   or "BOTTOM".
#' @param depth Numeric: (default = \code{5}) Number of taxonomic-tree levels
#'   to traverse, from 1 to 5.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A nested list containing the requested taxonomic path.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_taxonomy_path(id = 9606, direction = "TOP", depth = 3)
#' }
#' \donttest{
#' rba_uniprot_taxonomy_path(id = 207598, direction = "BOTTOM", depth = 3)
#' }
#'
#' @family "UniProt - Taxonomy"
#' @export
rba_uniprot_taxonomy_path <- function(id,
                                      direction,
                                      depth = 5,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "id", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, min_val = 1
      ),
      list(
        arg = "direction", class = "character", len = 1L,
        val = c("TOP", "BOTTOM")
      ),
      list(
        arg = "depth", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, ran = c(1, 5), no_null = TRUE
      )
    )
  )

  .msg(
    "Retrieving the %s steps of nodes that are in the %s of %s node.",
    depth, direction, id
  )

  ## Build GET API Request's query
  call_query <- list("id" = id, "direction" = direction, "depth" = depth)

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "taxonomy/path"),
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_taxonomy_path.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Shortest Path Between Two Taxonomy Nodes
#'
#' Use this function to retrieve the shortest path between two nodes
#' in the taxonomy tree of
#' \href{https://www.uniprot.org/help/taxonomy}{UniProt Taxonomy database}.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/taxonomy/relationship"
#'
#' @param from Numeric:
#'   \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifier} of the initial node.
#' @param to Numeric:
#'   \href{https://www.uniprot.org/help/taxonomic_identifier}{NCBI taxonomic
#'   identifier} of the final node.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A nested list containing the shortest path between the supplied
#'   nodes.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_taxonomy_relationship(from = 9606, to = 10090)
#' }
#'
#' @family "UniProt - Taxonomy"
#' @export
rba_uniprot_taxonomy_relationship <- function(from,
                                              to,
                                              ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "from", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, min_val = 1
      ),
      list(
        arg = "to", class = c("numeric", "integer"),
        len = 1L, integerish = TRUE, min_val = 1
      )
    )
  )

  .msg(
    "Retrieving the shortest path on the taxonomy tree from node %s to %s.",
    from, to
  )

  ## Build GET API Request's query
  call_query <- list("from" = from, "to" = to)

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "taxonomy/relationship"),
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_taxonomy_relationship.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
