#### database Endpoints ####
#' The version number of current database
#'
#' Returns the current version of Reactome database.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/database/version"
#'
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Character string containing the version of Reactome database.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_version()
#' }
#'
#' @family "Reactome Content Service - Database Info Queries"
#' @export
rba_reactome_version <- function(...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args()

  .msg(
    "Retrieving Reactome Content Service's database version."
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = paste0(
      .rba_stg("reactome", "pth", "content"),
      "data/database/version"
    ),
    accept = "text/plain",
    parser = "text->chr",
    save_to = .rba_file("reactome_version.txt")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### diseases Endpoints ####

#' Reactome Diseases
#'
#' This function Retrieve a list of all diseases or disease DOIDs annotated in
#'   Reactome.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/diseases"
#'  \cr "GET https://reactome.org/ContentService/data/diseases/doid"
#'
#' @param doid Logical: (default = \code{FALSE}) Return disease DOIDs instead of
#'   diseases?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Data frame containing all the disease annotation available at
#'   Reactome. If doid was set to TRUE, DOID info will be returned instead.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_diseases()
#' }
#' \donttest{
#' rba_reactome_diseases(doid = TRUE)
#' }
#'
#' @family "Reactome Content Service - Disease Related Queries"
#' @export
rba_reactome_diseases <- function(doid = FALSE,
                                  ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "doid", class = "logical", len = 1L)
    )
  )

  .msg(
    "Retrieving Reactome's diseases %s.",
    ifelse(isTRUE(doid), yes = "DOID data", no = "annotations")
  )

  ## Build Function-Specific Call
  if (isFALSE(doid)) {

    path_input <- paste0(
      .rba_stg("reactome", "pth", "content"),
      "data/diseases"
    )
    accept_input <- "application/json"
    parser_input <- "json->df"
    file_ext <- "json"

  } else {

    path_input <- paste0(
      .rba_stg("reactome", "pth", "content"),
      "data/diseases/doid"
    )
    accept_input <- "text/plain"
    parser_input <- "text->df"
    file_ext <- "txt"

  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    accept = accept_input,
    parser = parser_input,
    save_to = .rba_file(paste0("reactome_diseases.", file_ext))
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Entities Endpoints ####

#' Get a Complex's Subunits
#'
#' This function will return a list of subunits which are participants of
#' your supplied complex.
#'
#' Subunits will be returned recursively; Which means that if a subunit was
#'   itself a complex, subunit of that complex will be also returned in the
#'   results.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/complex/\{id\}/subunits"
#'
#' @param complex_id Character: Reactome stable Identifier of the complex.
#' @param exclude_structures Logical: (default = \code{FALSE}) Should the
#'   contained complexes and entity sets be excluded from the results?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Data frame which each row is a subunit of your supplied complex
#' and the columns are pertinent information of that subunit.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_complex_subunits(complex_id = "R-HSA-5674003",
#'     exclude_structures = FALSE)
#' }
#'
#' @family "Reactome Content Service - Physical Entity Queries"
#' @export
rba_reactome_complex_subunits <- function(complex_id,
                                          exclude_structures = FALSE,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "complex_id", class = "character", len = 1L),
      list(arg = "exclude_structures", class = "logical", len = 1L)
    )
  )

  .msg(
    "Recursively retrieving subunits of %s complex.",
    complex_id
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("excludeStructures", exclude_structures, "true")
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sdata/complex/%s/subunits",
      .rba_stg("reactome", "pth", "content"), complex_id
    ),
    query = call_query,
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("reactome_complex_subunits.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Complexes That Include a Molecule
#'
#' This function will retrieve a list of complexes that include your supplied
#'   molecule as a component.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/complexes/\{resource\}/
#'  \{identifier\}"
#'
#' @param id Character: Molecule's external Identifier
#' @param resource Character: What is the resource of your supplied ID? see:
#' \href{https://reactome.org/content/schema/objects/ReferenceDatabase/}{Reactome External
#' Identifiers}
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Data frame where each row is a complex containing your supplied
#'   molecule and columns are pertinent information.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_complex_list(id = "3845", resource = "NCBI Gene")
#' }
#' \donttest{
#' rba_reactome_complex_list(id = "P00533", resource = "UniProt")
#' }
#'
#' @family "Reactome Content Service - Physical Entity Queries"
#' @export
rba_reactome_complex_list <- function(id,
                                      resource,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "id", class = "character", len = 1L),
      list(arg = "resource", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving complexes that contain a molecule with '%s ID: %s'.",
    resource, id
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sdata/complexes/%s/%s",
      .rba_stg("reactome", "pth", "content"), resource, id
    ),
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("reactome_complex_list.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)

  return(final_output)
}

#' Get Larger Reactome Structures Which Include an Entity
#'
#' This function will retrieve a list of complexes and sets that Your
#'   supplied entity ID participates in (e.g. as a complex component,
#'   reaction output).
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/entity/\{id\}/componentOf"
#'
#' @param entity_id Character: Reactome's entity ID.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List of Reactome database Entities which Your supplied ID is a
#'   participant in them.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_participant_of(entity_id = "R-HSA-199420")
#' }
#'
#' @family "Reactome Content Service - Physical Entity Queries"
#' @seealso
#' \code{\link{rba_reactome_participants}}
#' @export
rba_reactome_participant_of <- function(entity_id,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "entity_id", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving Reactome structures which have %s as a participant.",
    entity_id
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sdata/entity/%s/componentOf",
      .rba_stg("reactome", "pth", "content"),
      entity_id
    ),
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("rba_reactome_participant_of.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Other forms of a Reactome Entity
#'
#' This function retrieve a list containing all other forms of your supplied
#'   Physical Entity ID.
#'
#' According to Reactome API documentation, "These other forms are Physical
#'   Entities that share the same Reference Entity identifier, e.g. PTEN
#'   H93R R-HSA-2318524 and PTEN C124R R-HSA-2317439 are two forms of PTEN."
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/entity/\{id\}/otherForms"
#'
#' @param entity_id Character: Reactome's entity ID.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Data frame where each row is other forms of your supplied Entity ID
#'   and columns are pertinent information.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_entity_other_forms("R-HSA-199420")
#' }
#'
#' @family "Reactome Content Service - Physical Entity Queries"
#' @export
rba_reactome_entity_other_forms <- function(entity_id,
                                            ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "entity_id", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving Other forms of Reactome's entity: %s",
    entity_id
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sdata/entity/%s/otherForms",
      .rba_stg("reactome", "pth", "content"),
      entity_id
    ),
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("reactome_entity_other_forms.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Events Endpoints ####

#' Get Reactome Events Ancestors
#'
#' Along with Reactome's events hierarchy, This function will retrieve all the
#'  events beginning from your supplied event up to the "Top level Pathway".
#'  see "Details section" for more information.
#'
#' By Reactome's definition, Events are the building blocks of biological
#'   processes and could be of two main classes: "Pathway" or
#'   "Reaction-like events". The events are organized in a hierarchical
#'   structure; and each event could be child or parent to another event; The
#'   hierarchy will always begin with a "Top level pathway" event. Also note
#'   that a given event could be part of more that one hierarchies.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/event/\{id\}/ancestors"
#'
#' @param event_id Character: Reactome event's identifier.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List which every element is a Data frame listing your supplied
#'   event along with it's ancestor events. Because any given event can be
#'   part of more than one pathway hierarchy, the list may contain multiple
#'   data frames.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_event_ancestors("R-HSA-5673001")
#' }
#'
#' @family "Reactome Content Service - Queries Related to Events"
#' @export
rba_reactome_event_ancestors <- function(event_id,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "event_id", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving the ancestors of event %s.",
    event_id
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sdata/event/%s/ancestors",
      .rba_stg("reactome", "pth", "content"),
      event_id
    ),
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("reactome_event_ancestors.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Full Event Hierarchy of a Species
#'
#' This function will retrieve the full Events hierarchy of your supplied
#'   species. Directly under each species, each child element is a "top Level
#'   Pathway". You can traverse the events tree down by following the "children"
#'   element.
#'
#' By Reactome's definition, Events are the building blocks of biological
#'   processes and could be of two main classes: "Pathway" or
#'   "Reaction-like events". The events are organized in a hierarchical
#'   structure; and each event could be child or parent to another event; The
#'   hierarchy will always begin with a "Top level pathway" event. Also note
#'   that a given event could be part of more that one hierarchies.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/eventsHierarchy/\{species\}"
#'
#' @param species Character or Numeric: NCBI Taxonomy identifier (Human Taxonomy
#'    ID is 9606.) or species name (e.g. "Homo sapiens"). See
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List which is a representation of the species's events hierarchy
#'   described in the "Details section".
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' #very large response!
#' rba_reactome_event_hierarchy("Homo sapiens")
#' }
#' \dontrun{
#' #very large response!
#' rba_reactome_event_hierarchy(9606)
#' }
#'
#' @family "Reactome Content Service - Queries Related to Events"
#' @export
rba_reactome_event_hierarchy <- function(species,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "species", class = c("character", "numeric", "integer"), len = 1L)
    )
  )

  .msg(
    "Retrieving the complete events hierarchy tree of species %s.",
    species
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sdata/eventsHierarchy/%s",
      .rba_stg("reactome", "pth", "content"),
      species
    ),
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("reactome_event_hierarchy.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Exporter Endpoints ####

#' Get a Reactome Event Diagram
#'
#' This function could be called in two scenarios: \enumerate{
#'   \item With create_document = FALSE: To retrieve an image of that event's
#'   Diagram.
#'   \item With create_document = TRUE: To retrieve a PDF document with the
#'   event's diagram image and additional information.}
#'   see "Details section" for more information
#'
#' If the function is called with create_document = FALSE:
#'   \cr The result will be an image with the format supplied in "output_format"
#'   argument. If the supplied event ID refers to a pathway, the image's
#'   content will be the that pathways diagram. If the supplied event ID refers
#'   to a sub-pathway or reaction event, the parent pathway's diagram will be
#'   exported, with that reaction or sub-pathway's events highlighted.
#'   \cr Note that to export an image of reaction-like event separately, you
#'   should use \code{\link{rba_reactome_exporter_reaction}}.
#'   \cr If the function is called with create_document = TRUE:
#'   \cr A PDF document will contain an image of the event's diagram and the
#'   following information of that events: Summation, Literature references,
#'   Edit history type, location, compartments and diseases.
#'   note that if you call the function with "document level = 1", information
#'   of your supplied event's children will also be included.
#'
#' @section Corresponding API Resources:
#'   "GET https://reactome.org/ContentService/exporter/diagram/\{identifier\}
#'   .\{ext\}"
#'   \cr "GET https://reactome.org/ContentService/exporter/document/event/
#'   \{identifier\}.pdf"
#'
#' @param event_id Character: Reactome event's identifier.
#' @param save_to NULL or Character: (default = \code{NULL}) \itemize{
#'   \item NULL: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param create_document Logical: (default = \code{FALSE}) Create PDF document
#'   instead of image?
#' @param resource Character: (default = \code{"TOTAL"}) The analysis resource
#'   for which the results will be overlaid on top of the given pathways
#'   overview.
#' @param diagram_profile Character: (default = \code{"Modern"}) Color profile
#'   of diagrams, should be either "Modern" or "Standard".
#' @param analysis_profile Character: (default = \code{"Standard"}) Color
#'   profile of analysis, should be one of: "Standard", "Strosobar" or "Copper
#'   Plus".
#' @param token Character: (optional) The analysis Token for which the results
#'   will be overlaid on top of the given pathways overview. see:
#'   \code{\link{rba_reactome_analysis}}.
#' @param exp_column Numeric: (optional) (only if token is supplied) Specify the
#'   expression column for the overlay.
#' @param document_level Numeric: (default = \code{1}) (Only if
#'   "create_document" is TRUE) use 0 to exclude the event's children or 1 to
#'   include them.
#' @param output_format Character: (default = \code{"png"}) (Only if
#'   "create_document" is FALSE) Image format of the saved diagram. Can be one
#'   of: png, jpg, jpeg, svg or gif.
#' @param image_quality Numeric: (default = \code{5}) (Only if "create_document"
#'   is FALSE), a number ranging from 1 to 10. 1 is the lowest quality and 10 is
#'   the highest.
#' @param flag_element Character: (optional) (Only if "create_document" is
#'   FALSE) gene name, protein ID, chemical ID or Reactome ID of a diagram's
#'   element to be flagged.
#' @param flg_interactors Logical: (default = \code{TRUE}) (Only if
#'   "create_document" is FALSE) Should the interactor be considered when
#'   flagging a diagram element?
#' @param sel Character vector: (optional) (Only if "create_document" is FALSE)
#'   CSV line for highlighting element(s) selection in the diagram.
#' @param title Logical: (default = \code{TRUE}) (Only if "create_document" is
#'   FALSE) Should the pathway name be displayed below the image?
#' @param margin Numeric: (default = \code{15}) (Only if "create_document" is
#'   FALSE) A number ranging from 0 to 20 to set as the image's margin.
#' @param ehld Logical: (default = \code{TRUE}) (Only if "create_document" is
#'   FALSE) Should Enhanced High Level Diagrams be considered?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return NULL, Based to the inputs, an image or PDF file will be saved to
#'   disk.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_exporter_diagram(event_id = "R-HSA-177929",
#'   create_document = FALSE)
#' }
#' \dontrun{
#' rba_reactome_exporter_diagram(event_id = "R-HSA-6787403",
#'     create_document = FALSE)
#' }
#' \dontrun{
#' rba_reactome_exporter_diagram(event_id = "R-HSA-177929",
#'     create_document = TRUE)
#' }
#' \dontrun{
#' rba_reactome_exporter_diagram(event_id = "R-HSA-177929",
#'     output_format = "svg",
#'     save_to = "reactome_event_diagram.svg")
#' }
#'
#' @family "Reactome Content Service - Format Exporter"
#' @seealso
#' \code{\link{rba_reactome_exporter_reaction}}
#' \code{\link{rba_reactome_analysis}}
#' @export
rba_reactome_exporter_diagram <- function(event_id,
                                          save_to = NULL,
                                          create_document = FALSE,
                                          resource = "TOTAL",
                                          diagram_profile = "Modern",
                                          analysis_profile = "Standard",
                                          token = NULL,
                                          exp_column = NULL,
                                          document_level  = 1,
                                          output_format = "png",
                                          image_quality = 5,
                                          flag_element = NULL,
                                          flg_interactors = TRUE,
                                          sel = NULL,
                                          title = TRUE,
                                          margin = 15,
                                          ehld = TRUE,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "event_id", class = "character", len = 1L),
      list(arg = "save_to", class = "character", len = 1L, no_na = FALSE),
      list(arg = "create_document", class = "logical", len = 1L),
      list(
        arg = "document_level", class = c("numeric", "integer"),
        len = 1L, val = c(0, 1)
      ),
      list(
        arg = "output_format",
        class = "character", len = 1L,
        val = c("png", "jpg", "jpeg", "svg", "gif")
      ),
      list(
        arg = "image_quality", class = c("numeric", "integer"),
        len = 1L, ran = c(1, 10)
      ),
      list(arg = "flag_element", class = "character", len = 1L),
      list(arg = "flg_interactors", class = "logical", len = 1L),
      list(arg = "sel", class = "character", min_len = 1L),
      list(arg = "title", class = "logical", len = 1L),
      list(
        arg = "margin", class = c("numeric", "integer"),
        len = 1L, ran = c(0, 20)
      ),
      list(arg = "ehld", class = "logical", len = 1L),
      list(
        arg = "diagram_profile", class = "character", len = 1L,
        val = c("Modern", "Standard")
      ),
      list(arg = "token", class = "character", len = 1L),
      list(
        arg = "resource", class = "character", len = 1L,
        val = c(
          "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR", "MIRBASE",
          "NCBI_PROTEIN", "EMBL", "COMPOUND", "PUBCHEM_COMPOUND"
        )
      ),
      list(
        arg = "analysis_profile", class = "character", len = 1L,
        val = c("Standard", "Strosobar", "Copper Plus")
      ),
      list(arg = "exp_column", class = c("numeric", "integer"), len = 1L)
    ),
    cond = list(
      list(
        quote(!is.null(exp_column) && is.null(token)),
        "You cannot specify expression column without providing a token."
      ),
      list(
        quote(!is.finite(image_quality) || image_quality != floor(image_quality)),
        "`image_quality` should be a finite integer from 1 to 10."
      ),
      list(
        quote(!is.finite(margin) || margin != floor(margin)),
        "`margin` should be a finite integer from 0 to 20."
      ),
      list(
        quote(!is.null(exp_column) && (!is.finite(exp_column) || exp_column != floor(exp_column))),
        "`exp_column` should be a finite integer."
      )
    )
  )

  ## Build Function-Specific Call
  call_query <- .rba_query(
    init = list(),
    list("resource", resource != "TOTAL", resource),
    list("diagramProfile", diagram_profile != "Modern", diagram_profile),
    list("analysisProfile", analysis_profile != "Standard", analysis_profile),
    list("token", !is.null(token), token),
    list("expColumn", !is.null(exp_column), exp_column)
  )

  if (isTRUE(create_document)) {

    .msg(
      "Retrieving a PDF document of event %s details.",
      event_id
    )

    ## Build Function-Specific Call
    call_query <- .rba_query(
      init = call_query,
      list("level", document_level != 1, document_level)
    )

    accept_input <- "application/pdf"
    output_format <- "pdf"
    path_input <- sprintf(
      "%sexporter/document/event/%s.pdf",
      .rba_stg("reactome", "pth", "content"), event_id)

  } else {

    .msg(
      "Retrieving event %s diagram's image in %s format.",
      event_id, output_format
    )

    ## Build Function-Specific Call
    call_query <- .rba_query(
      init = call_query,
      list("quality", image_quality != 5, image_quality),
      list("flg", !is.null(flag_element), flag_element),
      list("flgInteractors", !flg_interactors, "false"),
      list("sel", !is.null(sel), sel),
      list("title", !title, "false"),
      list("margin", margin != 15, as.integer(margin)),
      list("ehld", !ehld, "false")
    )

    accept_input <- switch(
      output_format,
      "svg" = "image/svg+xml",
      "jpg" = "image/jpeg",
      "jpeg" = "image/jpeg",
      paste0("image/", output_format)
    )

    path_input <- sprintf(
      "%sexporter/diagram/%s.%s",
      .rba_stg("reactome", "pth", "content"), event_id, output_format
    )

  }

  # create file_path
  save_to <- .rba_file(
    file = paste0(event_id, ".", output_format),
    save_to = ifelse(
      is.null(save_to) || is.na(save_to),
      yes = TRUE, no = save_to
    )
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    query = call_query,
    accept = accept_input,
    save_to = save_to,
    parser = NULL
  )

  ## Call API
  invisible(.rba_skeleton(input_call))
}

#' Exports A Reactome Event to SBGN or SBML
#'
#' This function will export a supplied Reactome Event (Pathway or Reaction)
#'   to a SBGN (Systems Biology Graphical Notation) or SBML (Systems Biology
#'   Markup Language)
#'
#' @param event_id Character: Reactome event's database IDs (DbId) or Stable IDs
#'   (StId).
#' @param output_format Character: Either "sbgn" or "sbml".
#' @param save_to NULL or Character: (default = \code{NULL}) \itemize{
#'   \item NULL: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/exporter/event/
#'  \{identifier\}.sbgn"
#'  \cr "GET https://reactome.org/ContentService/exporter/event/
#'  \{identifier\}.sbml"
#'
#' @return NULL, According to the inputs, a SBGN or SBML file will be saved to
#'   disk.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_exporter_event(event_id = "R-HSA-177929",
#'     output_format = "sbgn",
#'     save_to = "R-HSA-177929.sbgn")
#' }
#' \dontrun{
#' rba_reactome_exporter_event(event_id = "R-HSA-177929",
#'     output_format = "sbgn")
#' }
#'
#' @family "Reactome Content Service - Format Exporter"
#' @export
rba_reactome_exporter_event <- function(event_id,
                                        output_format,
                                        save_to = NULL,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "event_id", class = "character", len = 1L),
      list(
        arg = "output_format", class = "character", len = 1L,
        val = c("sbgn", "sbml")
      ),
      list(arg = "save_to", class = "character", len = 1L, no_na = FALSE))
  )

  .msg(
    "Exporting event %s as a %s file.",
    event_id, output_format
  )

  ## Build Function-Specific Call
  # create file_path
  save_to <- .rba_file(
    file = paste0(event_id, ".", output_format),
    save_to = ifelse(
      is.null(save_to) || is.na(save_to),
      yes = TRUE, no = save_to
    )
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sexporter/event/%s.%s",
      .rba_stg("reactome", "pth", "content"), event_id, output_format
    ),
    save_to = save_to,
    parser = NULL
  )

  ## Call API
  invisible(.rba_skeleton(input_call))
}

#' Get a Reactome Pathway Overview
#'
#' This function will save a pathway overview of the supplied species
#'   as an image file.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/exporter/fireworks/\{species\}.\{ext\}"
#'
#' @param species Character or Numeric: NCBI Taxonomy identifier (Human Taxonomy
#'    ID is 9606.) or species name (e.g. "Homo sapiens"). See
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param output_format Character: (default = \code{"png"}) Image format, can be
#'   one of: png, jpg, jpeg, svg or gif.
#' @param save_to NULL or Character: (default = \code{NULL}) \itemize{
#'   \item NULL: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param image_quality Numeric: (default = \code{5}) A number  ranging from 1
#'   to 10. 1 is the lowest quality and 10 is the highest.
#' @param flag_element Character: (optional) Gene name, protein ID, chemical ID
#'   or Reactome ID of a diagram's element to be flagged.
#' @param flg_interactors Logical: (default = \code{TRUE}) Should the interactor
#'   be considered when flagging a diagram element?
#' @param sel Character vector: (optional) CSV line for highlighting element(s)
#'   selection in the diagram.
#' @param title Logical: (default = \code{TRUE}) Should the pathway name be
#'   displayed below the image?
#' @param margin Numeric: (default = \code{15}) A number ranging from 0 to 20 to
#'   set as the image's margin.
#' @param diagram_profile Character: (default = \code{"Copper"}) Color profile
#'   of the overview, should be one of "Copper", "Copper plus", "Barium Lithium"
#'   or "Calcium Salts".
#' @param token Character: (optional) The analysis Token for which the results
#'   will be overlaid on top of the given pathways overview. see:
#'   \code{\link{rba_reactome_analysis}}.
#' @param resource Character: (default = \code{"TOTAL"}) The analysis resource
#'   for which the results will be overlaid on top of the given pathways
#'   overview.
#' @param exp_column Numeric: (optional) (only if token is supplied) Specify the
#'   expression column for the overlay.
#' @param coverage Logical: (default = \code{FALSE}) Should the analysis
#'   coverage values be overlaid?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return NULL, Based to the inputs, an image file will be saved to disk.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_exporter_overview(species = 9606,
#'     output_format = "svg",
#'     save_to = "human_pathways.svg")
#' }
#' \dontrun{
#' rba_reactome_exporter_overview(species = 9606,
#'     token = 123456789)
#' }
#'
#' @family "Reactome Content Service - Format Exporter"
#' @seealso
#' \code{\link{rba_reactome_analysis}}
#' @export
rba_reactome_exporter_overview <- function(species,
                                           output_format = "png",
                                           save_to = NULL,
                                           image_quality = 5,
                                           flag_element = NULL,
                                           flg_interactors = TRUE,
                                           sel = NULL,
                                           title = TRUE,
                                           margin = 15,
                                           diagram_profile = "Copper",
                                           token = NULL,
                                           resource = "TOTAL",
                                           exp_column = NULL,
                                           coverage = FALSE,
                                           ...) {

  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "species", class = c("character", "numeric", "integer"), len = 1L),
      list(
        arg = "output_format", class = "character", len = 1L, no_null = TRUE,
        val = c("png",
                "jpg",
                "jpeg",
                "svg",
                "gif")
      ),
      list(arg = "save_to", class = "character", len = 1L, no_na = FALSE),
      list(
        arg = "image_quality", class = c("numeric", "integer"),
        len = 1L, ran = c(1, 10)
      ),
      list(arg = "flag_element", class = "character", len = 1L),
      list(arg = "flg_interactors", class = "logical", len = 1L),
      list(arg = "sel", class = "character", min_len = 1L),
      list(arg = "title", class = "logical", len = 1L),
      list(
        arg = "margin", class = c("numeric", "integer"),
        len = 1L, ran = c(0, 20)
      ),
      list(
        arg = "diagram_profile", class = "character", len = 1L,
        val = c("Copper",
                "Copper plus",
                "Barium Lithium",
                "Calcium Salts")
      ),
      list(arg = "token", class = "character", len = 1L),
      list(
        arg = "resource", class = "character", len = 1L,
        val = c(
          "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR", "MIRBASE",
          "NCBI_PROTEIN", "EMBL", "COMPOUND", "PUBCHEM_COMPOUND"
        )
      ),
      list(arg = "exp_column", class = c("numeric", "integer"), len = 1L),
      list(arg = "coverage", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(!is.null(exp_column) && is.null(token)),
        "You cannot specify expression column without providing a token."
      ),
      list(
        quote(!is.finite(image_quality) || image_quality != floor(image_quality)),
        "`image_quality` should be a finite integer from 1 to 10."
      ),
      list(
        quote(!is.finite(margin) || margin != floor(margin)),
        "`margin` should be a finite integer from 0 to 20."
      ),
      list(
        quote(!is.null(exp_column) && (!is.finite(exp_column) || exp_column != floor(exp_column))),
        "`exp_column` should be a finite integer."
      )
    )
  )

  .msg(
    "Retrieving species %s pathway overview image in %s format.",
    species, output_format
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("quality", image_quality != 5, image_quality),
    list("flg", !is.null(flag_element), flag_element),
    list("flgInteractors", !flg_interactors, "false"),
    list("sel", !is.null(sel), sel),
    list("title", !title, "false"),
    list("margin", margin != 15, as.integer(margin)),
    list("diagramProfile", diagram_profile != "Copper", diagram_profile),
    list("token", !is.null(token), token),
    list("resource", resource != "TOTAL", resource),
    list("expColumn", !is.null(exp_column), exp_column),
    list("coverage", coverage, "true")
  )

  ## Build Function-Specific Call
  accept_input <- switch(
    output_format,
    "svg" = "image/svg+xml",
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    paste0("image/", output_format)
  )

  # create file_path
  save_to <- .rba_file(
    file = paste0(species, ".", output_format),
    save_to = ifelse(
      is.null(save_to) || is.na(save_to),
      yes = TRUE, no = save_to
    )
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sexporter/fireworks/%s.%s",
      .rba_stg("reactome", "pth", "content"), gsub(" ", "%20",species), output_format
    ),
    query = call_query,
    accept = accept_input,
    save_to = save_to,
    parser = NULL
  )

  ## Call API
  invisible(.rba_skeleton(input_call))
}

#' Get a Reactome Reaction Event
#'
#' This function will Save a Reactome event of class "ReactionLikeEvent" as
#'   an image file.
#'
#' Note that this function will save Reaction-like event separately and out
#'   of it's parent pathway context. To overlay a Reaction on it's parent
#'   pathway, use \code{\link{rba_reactome_exporter_diagram}}.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/exporter/reaction/
#'  \{identifier\}.\{ext\}"
#'
#' @param event_id Character: Reactome
#' \href{https://reactome.org/content/schema/ReactionLikeEvent/}{Reaction-like
#' event}'s identifier.
#' @param output_format Character: (default = \code{"png"}) Image format, can be
#'   one of: png, jpg, jpeg, svg or gif.
#' @param save_to NULL or Character: (default = \code{NULL}) \itemize{
#'   \item NULL: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param image_quality Numeric: (default = \code{5}) A number  ranging from 1
#'   to 10. 1 is the lowest quality and 10 is the highest.
#' @param flag_element Character: (optional) Gene name, protein ID, chemical ID
#'   or Reactome ID of a diagram's element to be flagged.
#' @param flg_interactors Logical: (default = \code{TRUE}) Should the interactor
#'   be considered when flagging a diagram element?
#' @param sel Character vector: (optional) CSV line for highlighting element(s)
#'   selection in the diagram.
#' @param title Logical: (default = \code{TRUE}) Should the pathway name be
#'   displayed below the image?
#' @param margin Numeric: (default = \code{15}) A number ranging from 0 to 20 to
#'   set as the image's margin.
#' @param diagram_profile Character: (default = \code{"Modern"}) Color profile
#'   of diagrams, should be either "Modern" or "Standard".
#' @param token Character: (optional) The analysis Token for which the results
#'   will be overlaid on top of the given pathways overview. see:
#'   \code{\link{rba_reactome_analysis}}.
#' @param analysis_profile Character: (default = \code{"Standard"}) Color
#'   profile of analysis, should be one of: "Standard", "Strosobar" or "Copper
#'   Plus".
#' @param resource Character: (default = \code{"TOTAL"}) The analysis resource
#'   for which the results will be overlaid on top of the given pathways
#'   overview.
#' @param exp_column Numeric: (optional) (only if token is supplied) Specify the
#'   expression column for the overlay.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return NULL, Based to the inputs, an image file will be saved to disk.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_exporter_diagram(event_id = "R-HSA-6787403",
#'     create_document = FALSE)
#' }
#' \dontrun{
#' rba_reactome_exporter_diagram(event_id = "R-HSA-6787403",
#'      output_format = "svg",
#'      save_to = "reactome_reacion_image.svg")
#' }
#'
#' @family "Reactome Content Service - Format Exporter"
#' @seealso
#' \code{\link{rba_reactome_exporter_diagram}}
#' \code{\link{rba_reactome_analysis}}
#' @export
rba_reactome_exporter_reaction <- function(event_id,
                                           save_to = NULL,
                                           output_format = "png",
                                           resource = "TOTAL",
                                           diagram_profile = "Modern",
                                           analysis_profile = "Standard",
                                           token = NULL,
                                           exp_column = NULL,
                                           image_quality = 5,
                                           flag_element = NULL,
                                           flg_interactors = TRUE,
                                           sel = NULL,
                                           title = TRUE,
                                           margin = 15,
                                           ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "event_id", class = "character", len = 1L),
      list(arg = "save_to", class = "character", len = 1L, no_na = FALSE),
      list(
        arg = "output_format", no_null = TRUE, class = "character", len = 1L,
        val = c("png", "jpg", "jpeg", "svg", "gif")
      ),
      list(
        arg = "image_quality", class = c("numeric", "integer"),
        len = 1L, ran = c(1, 10)
      ),
      list(arg = "flag_element", class = "character", len = 1L),
      list(arg = "flg_interactors", class = "logical", len = 1L),
      list(arg = "sel", class = "character", min_len = 1L),
      list(arg = "title", class = "logical", len = 1L),
      list(
        arg = "margin", class = c("numeric", "integer"),
        len = 1L, ran = c(0, 20)
      ),
      list(
        arg = "diagram_profile", class = "character", len = 1L,
        val = c("Modern", "Standard")
      ),
      list(arg = "token", class = "character", len = 1L),
      list(
        arg = "resource", class = "character", len = 1L,
        val = c(
          "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR", "MIRBASE",
          "NCBI_PROTEIN", "EMBL", "COMPOUND", "PUBCHEM_COMPOUND"
        )
      ),
      list(
        arg = "analysis_profile", class = "character", len = 1L,
        val = c("Standard", "Strosobar", "Copper Plus")),
      list(arg = "exp_column", class = c("numeric", "integer"), len = 1L)
    ),
    cond = list(
      list(
        quote(!is.null(exp_column) && is.null(token)),
        "You cannot specify expression column without providing a token."
      ),
      list(
        quote(!is.finite(image_quality) || image_quality != floor(image_quality)),
        "`image_quality` should be a finite integer from 1 to 10."
      ),
      list(
        quote(!is.finite(margin) || margin != floor(margin)),
        "`margin` should be a finite integer from 0 to 20."
      ),
      list(
        quote(!is.null(exp_column) && (!is.finite(exp_column) || exp_column != floor(exp_column))),
        "`exp_column` should be a finite integer."
      )
    )
  )

  .msg(
    "Retrieving Reaction-like event %s image in %s format.",
    event_id, output_format
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("quality", image_quality != 5, image_quality),
    list("flg", !is.null(flag_element), flag_element),
    list("flgInteractors", !flg_interactors, "false"),
    list("sel", !is.null(sel), sel),
    list("title", !title, "false"),
    list("margin", margin != 15, as.integer(margin)),
    list("diagramProfile", diagram_profile != "Modern", diagram_profile),
    list("analysisProfile", analysis_profile != "Standard", analysis_profile),
    list("token", !is.null(token), token),
    list("resource", resource != "TOTAL", resource),
    list("expColumn", !is.null(exp_column), exp_column)
  )

  ## Build Function-Specific Call
  accept_input <- switch(
    output_format,
    "svg" = "image/svg+xml",
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    paste0("image/", output_format)
  )

  # create file_path
  save_to <- .rba_file(
    file = paste0(event_id, ".", output_format),
    save_to = ifelse(
      is.null(save_to) || is.na(save_to),
      yes = TRUE,
      no = save_to
    )
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sexporter/reaction/%s.%s",
      .rba_stg("reactome", "pth", "content"), event_id, output_format
    ),
    query = call_query,
    accept = accept_input,
    save_to = save_to,
    parser = NULL)

  ## Call API
  invisible(.rba_skeleton(input_call))
}

#### Interactors Endpoints ####

#' The interface From Reactome to PSICQUIC
#'
#' You can call this function in two scenarios:
#'   1- To retrieve information of all available PSICQUIC resources, call the
#'   function without providing any argument; i.e
#'   rba_reactome_interactors_psicquic().
#'   2-To retrieve a list of interactors of specific protein(s), fill out the
#'   function's arguments.
#'
#' @section Corresponding API Resources:
#'  "POST https://reactome.org/ContentService/interactors/psicquic/molecules/
#'    \{resource\}/details"
#'  \cr "POST https://reactome.org/ContentService/interactors/psicquic/molecules/
#'    \{resource\}/summary"
#'  \cr "GET https://reactome.org/ContentService/interactors/psicquic/resources"
#'
#' @param proteins Character or Numeric vector: (optional) Proteins to retrieve
#'   PSICQUIC interactors.
#' @param resource Character: (optional) The PSICQUIC resource for your supplied
#'   proteins. Call rba_reactome_interactors_psicquic() without argument to get
#'   the available options.
#' @param details Logical: (default = \code{TRUE}) If TRUE a detailed list of
#'   interactors will be returned. If FALSE, only a summary of available
#'   interactors will be returned.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Depending your input, a list containing the detailed or summary of
#'   PSICQUIC interactions or a data frame of all registered PSICQUIC
#'   resources.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_interactors_psicquic()
#' }
#' \donttest{
#' rba_reactome_interactors_psicquic(proteins = c("TP53", "MYC"),
#'     resource = "BioGrid",
#'     details = FALSE)
#' }
#' \donttest{
#' rba_reactome_interactors_psicquic(proteins = c("TP53", "MYC"),
#'     resource = "BioGrid",
#'     details = TRUE)
#' }
#'
#' @family "Reactome Content Service - Molecule Interactors"
#' @export
rba_reactome_interactors_psicquic <- function(proteins = NULL,
                                              resource = NULL,
                                              details = TRUE,
                                              ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "proteins", class = c("character", "numeric", "integer"),
        min_len = 1L, max_len = 1000
      ),
      list(arg = "resource", class = "character", len = 1L),
      list(arg = "details", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(sum(!is.null(proteins), !is.null(resource)) == 1),
        "You should supply `proteins` and `resource` together."
      )
    )
  )

  if (!is.null(proteins)) {

    details <- ifelse(isTRUE(details), yes = "details", no = "summary")

    .msg(
      "Retrieving %s of clustered interactions of %s protein(s) from %s.",
      details,
      ifelse(length(proteins) == 1, yes = proteins, no = length(proteins)),
      resource
    )

    ## Build POST API Request's URL
    call_body <- paste(unique(proteins),collapse = "\n")
    input_call <- .rba_httr(
      httr = "post",
      url = .rba_stg("reactome", "url"),
      path = sprintf(
        "%sinteractors/psicquic/molecules/%s/%s",
        .rba_stg("reactome", "pth", "content"), resource, details
      ),
      body = call_body,
      accept = "application/json",
      httr::content_type("text/plain"),
      parser = "json->list",
      save_to = .rba_file("reactome_interactors_psicquic.json")
    )

  } else {

    .msg(
      "Retrieving a table of all Psicquic Registries services."
    )

    ## Build Function-Specific Call
    input_call <- .rba_httr(
      httr = "get",
      url = .rba_stg("reactome", "url"),
      path = paste0(
        .rba_stg("reactome", "pth", "content"),
        "interactors/psicquic/resources"
      ),
      accept = "application/json",
      parser = "json->df",
      save_to = .rba_file("reactome_interactors_psicquic.json")
    )

  }

  ## Call API
  final_output <- .rba_skeleton(input_call)

  return(final_output)
}

#' Get Static(IntAct) Interaction Information of a Protein
#'
#' Reactome maintains a locally hosted snapshot of the IntAct interactions
#'   database. Using this function, you can retrieve IntAct information for
#'   one or more proteins in two scenarios: \enumerate{
#'   \item If \code{endpoint = "details"} or \code{endpoint = "summary"},
#'     retrieve detailed or summary information for the supplied accessions.
#'   \item If \code{endpoint = "pathways"}, retrieve Reactome pathways which
#'   include your supplied protein accession. Pathways with the class
#'   "TopLevelPathway" will be excluded.}
#'   Results depend on Reactome's current static interaction snapshot; a valid
#'   accession can therefore have no mapped pathways.
#'
#' @section Corresponding API Resources:
#'  "POST https://reactome.org/ContentService/interactors/static/
#'  molecules/details"
#'  \cr "POST https://reactome.org/ContentService/interactors/static/
#'  molecules/summary"
#'  \cr "GET https://reactome.org/ContentService/interactors/static/molecule/
#'  \{identifier\}/pathways"
#'
#' @param proteins Character or Numeric vector: UniProt protein accession(s). If
#'   \code{endpoint = "pathways"}, only a single protein accession can be
#'   supplied.
#' @param endpoint Character: (default = \code{"details"}) Can be one of:
#'   \enumerate{ \item "details": Return detailed information for the supplied
#'   accessions. \item "summary": Return summary information for the supplied
#'   accessions. \item "pathways": Return pathways containing the interacting
#'   molecules (excluding the TopLevelPathway class).}
#' @param only_diagrammed Logical: (default = \code{FALSE}) (only when
#'   \code{endpoint = "pathways"}) If TRUE, pathways without diagram will be
#'   excluded.
#' @param species Character: (optional) (only when \code{endpoint = "pathways"})
#'   The scientific name of the species to search for pathways. See
#'   \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List which it's content varies based on the  supplied "endpoint"
#'   argument.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_interactors_static(proteins = "Q9BXM7-1",
#'     endpoint = "pathways", species = "Homo sapiens")
#' }
#' \donttest{
#' rba_reactome_interactors_static(proteins = c("Q9BXM7-1", "Q13501"),
#'     endpoint = "details")
#' }
#' \donttest{
#' rba_reactome_interactors_static(proteins = c("Q9BXM7-1", "Q13501"),
#'     endpoint = "summary")
#' }
#'
#' @family "Reactome Content Service - Molecule Interactors"
#' @export
rba_reactome_interactors_static <- function(proteins,
                                            endpoint = "details",
                                            only_diagrammed = FALSE,
                                            species = NULL,
                                            ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "proteins", class = c("character", "numeric", "integer"),
        min_len = 1L, max_len = 1000
      ),
      list(
        arg = "endpoint", class = "character", len = 1L,
        val = c("details",
                "summary",
                "pathways")
      ),
      list(arg = "only_diagrammed", class = "logical", len = 1L),
      list(arg = "species", class = "character", len = 1L)
    ),
    cond = list(
      list(
        quote(endpoint == "pathways" && length(proteins) != 1),
        "When 'endpoint = pathways', you can only supply one protein."
      ),
      list(
        quote(!is.null(species) && endpoint != "pathways"),
        "You can only supply `species` when `endpoint = \"pathways\"`."
      )
    )
  )

  if (endpoint == "pathways") {

    .msg(
      "Retrieving pathways with the Static(IntAct) Interactors of protein %s.",
      proteins
    )

    call_query <- .rba_query(
      init = list(
        "onlyDiagrammed" = ifelse(
          isTRUE(only_diagrammed), yes = "true", no = "false"
        )
      ),
      list("species", !is.null(species), species)
    )

    input_call <- .rba_httr(
      httr = "get",
      url = .rba_stg("reactome", "url"),
      path = sprintf(
        "%sinteractors/static/molecule/%s/pathways",
        .rba_stg("reactome", "pth", "content"), proteins
      ),
      query = call_query,
      accept = "application/json",
      parser = "json->df",
      save_to = .rba_file("reactome_interactors_static.json")
    )

  } else {

    ## Build POST API Request's URL
    .msg(
      "Retrieving %s of Static(IntAct) Interactors of protein %s.",
      endpoint, proteins
    )

    call_body <- paste(unique(proteins),collapse = "\n")

    ## Build Function-Specific Call
    parser_input <- ifelse(
      endpoint == "details",
      yes = "json->list",
      no = "json->list_simp"
    )

    input_call <- .rba_httr(
      httr = "post",
      url = .rba_stg("reactome", "url"),
      path = paste0(
        .rba_stg("reactome", "pth", "content"),
        "interactors/static/molecules/",
        endpoint
      ),
      body = call_body,
      accept = "application/json",
      httr::content_type("text/plain"),
      parser = parser_input,
      save_to = .rba_file("reactome_interactors_static.json")
    )

  }

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Mapping Endpoints ####

#' Map External ID to Reactome Pathways/Reactions
#'
#' By providing an external identifier from a given resource, you can retrieve
#'   a list of pathways/reactions that include your supplied ID.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/mapping/\{resource\}/
#'  \{identifier\}/pathways"
#'  \cr "GET https://reactome.org/ContentService/data/mapping/\{resource\}/
#'  \{identifier\}/reactions"
#'
#' @param id Character or Numeric: Molecule's external Identifier
#' @param resource Character: What is the resource of your supplied ID? see:
#' \href{https://reactome.org/content/schema/objects/ReferenceDatabase/}{Reactome External
#' Identifiers}
#' @param species Character or Numeric: (default = \code{"Homo sapiens"}) NCBI
#'   Taxonomy identifier (Human is 9606), species name (e.g. "Homo sapiens") or
#'   Reactome DbId (e.g Homo sapiens is 48887). See
#'   \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param map_to Character: Either "pathways" or "reactions".
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Data frame where each row is a pathway/reaction and columns are
#'   pertinent information.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_mapping(id = "PTEN", resource =  "UniProt",
#'     map_to = "reactions", species = 9606)
#' }
#'
#' @family "Reactome Content Service - Mapping Related Queries"
#' @export
rba_reactome_mapping <- function(id,
                                 resource,
                                 map_to,
                                 species = "Homo sapiens",
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "id", class = c("character", "numeric", "integer"), len = 1L),
      list(arg = "resource", class = "character", len = 1L),
      list(arg = "species", class = c("character", "numeric", "integer"), len = 1L),
      list(
        arg = "map_to", class = "character", len = 1L,
        val = c("pathways", "reactions")
      )
    )
  )

  .msg(
    "Retrieving Reactome %s that contain %s from %s resource.",
    map_to, id, resource
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("species", !is.null(species), species)
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sdata/mapping/%s/%s/%s",
      .rba_stg("reactome", "pth", "content"),
      resource, id, map_to
    ),
    query = call_query,
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("reactome_mapping.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Orthology Endpoints ####

#' Get Orthologous (Computationally Inferred) Events
#'
#' Reactome incorporate manually curated human reactions and PANTHER's
#'   protein homology data to Computationally infer events in other eukaryotic
#'   species.
#'
#' Reactome uses an orthology-based approach to project curated human events
#'   to supported non-human species. See
#'   \href{https://reactome.org/documentation/inferred-events/}{
#'   Reactome Computationally Inferred Events} for more information.
#'
#' @section Corresponding API Resources:
#'  "POST https://reactome.org/ContentService/data/orthologies/ids/
#'    species/\{speciesId\}"
#'
#' @param event_ids Character vector: Human Reactome event ID(s) to retrieve
#'   their orthologous events.
#' @param species_dbid Numeric: Reactome database ID (DbId) of the target
#'   species. (e.g Mus musculus is 48892). See
#'   \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List containing found Orthologous event(s) in your supplied species
#'   and their pertinent information.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_orthology(event_ids = c("R-HSA-6799198", " R-HSA-72764"),
#'     species_dbid = 49633)
#' }
#'
#' @family "Reactome Content Service - Orthology Related Queries"
#' @seealso
#' \code{\link{rba_reactome_analysis_species}}
#' @export
rba_reactome_orthology <- function(event_ids,
                                   species_dbid,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "event_ids", class = "character", min_len = 1L),
      list(
        arg = "species_dbid", class = c("numeric", "integer"),
        len = 1L, min_val = 0
      )
    ),
    cond = list(
      list(
        quote(!is.finite(species_dbid) || species_dbid != floor(species_dbid)),
        "`species_dbid` should be a finite, non-negative integer."
      )
    )
  )

  .msg(
    "Retrieving orthologous events of '%s' in the species with DbId '%s'.",
    ifelse(
      length(event_ids) == 1,
      yes = event_ids,
      no = paste0(length(event_ids), " input events")
    ),
    species_dbid
  )

  ## Build POST API Request's URL
  call_body <- paste(unique(event_ids),collapse = "\n")

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("reactome", "url"),
    path = paste0(
      .rba_stg("reactome", "pth", "content"),
      "data/orthologies/ids/species/",
      species_dbid
    ),
    body = call_body,
    accept = "application/json",
    httr::content_type("text/plain"),
    parser = "json->list_simp",
    save_to = .rba_file("reactome_orthology.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Participants Endpoints ####

#' Get Participants of a Reactome Event
#'
#' Participating molecules in a Reactome comprises set of 'Physical Entity' and
#'   'Reference Entities' class objects. Use this function to retrieve all, only
#'   'Physical Entity' or only 'Reference Entities' participants of given event.
#'
#' A 'Physical Entity' Instance could include an individual molecule,
#'  a multi-molecular complex or a set of molecule forming a group based on
#'  some characteristics. a single molecule can have different 'Physical Entity'
#'  instances based on it's associated attributes. For example, IgK Ig kappa
#'  chain, has two 'Physical Entity' instances; one, with ID
#'  \href{https://reactome.org/content/schema/instance/browser/R-HSA-197041}{
#'  "R-HSA-197041"} refers to the secreted antibody protein to the
#'  extra-cellular region; And the second one is with ID
#'  \href{https://reactome.org/content/schema/instance/browser/R-HSA-2038819}{
#'  "R-HSA-2038819"} and refers to the plasma-membrane-integrated form of
#'  the antibody protein.
#'  \cr To make it possible to link multiple 'Physical Entity' instances of a
#'  molecule, Reactome uses a data class named "'Reference Entities'"
#'  which correspond to the invariant attribute of a molecule. for example,
#'  both of the above-mentioned 'Physical Entities' see a 'Reference Entities'
#'  named \href{https://reactome.org/content/schema/instance/browser/57819}{
#'  "UniProt:P01834 IGKC}.
#'  \cr See \href{https://reactome.org/documentation/data-model/}{Reactome
#'  Data Model} for more information about the data model and Physical
#'  Entities.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/participants/\{id\}"
#'  \cr "GET https://reactome.org/ContentService/data/participants/\{id\}/
#'  participatingPhysicalEntities"
#'  \cr "GET https://reactome.org/ContentService/data/participants/\{id\}/
#'  referenceEntities"
#'
#' @param event_id Character or Numeric: Reactome event's database ID (DbId) or
#'   Stable ID (StId).
#' @param only_physical_entities Logical: (default = \code{FALSE}) If TRUE, only
#'   participating 'Physical Entities' will be returned.
#' @param only_reference_entities Logical: (default = \code{FALSE}) If TRUE,
#'   only participating 'Reference Entities' will be returned.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List with the participant of your supplied Event ID. A Data frame
#'  if only physical or 'Reference Entities' was requested.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_participants("R-HSA-5682012")
#' }
#' \donttest{
#' rba_reactome_participants("R-HSA-5682012", only_physical_entities = TRUE)
#' }
#' \donttest{
#' rba_reactome_participants("R-HSA-5682012", only_reference_entities = TRUE)
#' }
#'
#' @family "Reactome Content Service - Queries Related to Participants"
#' @seealso
#' \code{\link{rba_reactome_participant_of}}
#' @export
rba_reactome_participants <- function(event_id,
                                      only_physical_entities = FALSE,
                                      only_reference_entities = FALSE,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "event_id", class = c("character", "numeric", "integer"), len = 1L),
      list(arg = "only_physical_entities", class = "logical", len = 1L),
      list(arg = "only_reference_entities", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(sum(only_physical_entities, only_reference_entities) == 2),
        "You can only set one of `only_physical_entities` or `only_reference_entities` to TRUE."
      )
    )
  )

  .msg(
    "Retrieving %sParticipants of Reactome event %s.",
    ifelse(
      sum(only_physical_entities, only_reference_entities) == 0,
      yes = "",
      no = c("'Physical Entities' ",
             "'Reference Entities' ")[c(only_physical_entities,
                                        only_reference_entities)]
    ),
    event_id
  )

  ## Build Function-Specific Call
  path_input <- paste0(
    .rba_stg("reactome", "pth", "content"),
    "data/participants/",
    event_id
  )

  parser_input <- "json->list"

  if (isTRUE(only_physical_entities)) {

    path_input <- paste0(path_input, "/participatingPhysicalEntities")
    parser_input <- "json->df"

  } else if (isTRUE(only_reference_entities)) {

    path_input <- paste0(path_input, "/referenceEntities")
    parser_input <- "json->df"

  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("reactome_participants.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Pathways Endpoints ####

#' Get Events Contained in an Upstream Event
#'
#' Reactome events can contain other events; for example, a pathway can contain
#'   smaller pathways and reactions. This function recursively retrieves all
#'   events downstream of the supplied event, or one attribute of those events.
#'
#' Reactome defines events as the building blocks of biological processes.
#'   Events can be pathways or reaction-like events and are organized
#'   hierarchically. An event can be a child or parent of another event, each
#'   hierarchy begins with a top-level pathway, and an event can belong to more
#'   than one hierarchy.
#'
#' When \code{attribute_name} is supplied, the function returns one value for
#'   each contained event whenever the individual values can be identified
#'   reliably. Empty values and line breaks within a value are preserved.
#'   Otherwise, the complete result is returned unchanged with a warning. When
#'   \code{save_file} is used, the saved file always contains the result exactly
#'   as supplied by Reactome.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/pathway/\{id\}/
#'  containedEvents"
#'  \cr "GET https://reactome.org/ContentService/data/pathway/\{id\}/
#'  containedEvents/\{attributeName\}"
#'
#' @param event_id Character or Numeric: Reactome event's database ID (DbId) or
#'   Stable ID (StId).
#' @param attribute_name Character: Optional event attribute to return instead
#'   of complete event records. See
#'   \href{https://reactome.org/content/schema/Event}{Reactome Data Schema:
#'   Event} for available options.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list with information about the contained events. If
#'   \code{attribute_name} is supplied, one value for each contained event is
#'   returned. If the individual values cannot be identified reliably, the
#'   complete result is returned as a single value.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_pathways_events(event_id = "R-HSA-5673001")
#' }
#' \donttest{
#' rba_reactome_pathways_events(event_id = "R-HSA-5673001",
#'     attribute_name = "stId")
#' }
#'
#' @family "Reactome Content Service - Pathway Related Queries"
#' @export
rba_reactome_pathways_events <- function(event_id,
                                         attribute_name = NULL,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "event_id", class = c("numeric", "integer", "character"), len = 1L),
      list(arg = "attribute_name", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving %s contained events under the event %s.",
    ifelse(
      is.null(attribute_name),
      yes = "all",
      no = sprintf("attribute '%s' of all", attribute_name)
    ),
    event_id
  )

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%sdata/pathway/%s/containedEvents",
    .rba_stg("reactome", "pth", "content"),
    event_id
  )
  accept_input <- "application/json"
  parser_input <- "json->list"
  file_ext <- "json"

  if (!is.null(attribute_name)) {

    accept_input <- "text/plain"
    attribute_is_db_id <- identical(tolower(attribute_name), "dbid")

    ## Retrieve Contained-Event Count
    if (!attribute_is_db_id) {
      input_call <- .rba_httr(
        httr = "get",
        url = .rba_stg("reactome", "url"),
        path = paste0(path_input, "/dbId"),
        accept = "text/plain",
        parser = "text->chr",
        save_to = FALSE
      )
      contained_event_ids <- .rba_skeleton(input_call)
    }
    path_input <- paste0(path_input, "/", attribute_name)

    parser_input <- list(
      "text->chr",
      function(parsed_response) {
        ## Select Event-Count Source
        event_ids <- if (attribute_is_db_id) {
          parsed_response
        } else {
          contained_event_ids
        }

        valid_response <- length(parsed_response) == 1L &&
          !is.na(parsed_response) &&
          startsWith(parsed_response, "[") &&
          endsWith(parsed_response, "]") &&
          is.character(event_ids) &&
          length(event_ids) == 1L &&
          !is.na(event_ids) &&
          startsWith(event_ids, "[") &&
          endsWith(event_ids, "]")

        ## Parse Valid Responses
        if (valid_response) {
          event_ids <- substr(event_ids, 2L, nchar(event_ids) - 1L)

          ## Count Contained Events
          event_count <- if (nzchar(event_ids)) {
            length(strsplit(event_ids, split = ", ", fixed = TRUE)[[1L]])
          } else {
            0L
          }
          response_body <- substr(
            parsed_response,
            2L,
            nchar(parsed_response) - 1L
          )

          ## Return an Empty Result
          if (event_count == 0L && !nzchar(response_body)) {
            return(character())
          }

          ## Return a Single Value
          if (event_count == 1L) {
            return(response_body)
          }

          ## Parse Multiple Values
          if (event_count > 1L) {
            response_parts <- strsplit(
              response_body,
              split = ", ",
              fixed = TRUE
            )[[1L]]

            ## Preserve a Trailing Empty Value
            if (endsWith(response_body, ", ")) {
              response_parts <- c(response_parts, "")
            }

            ## Return Plain Values
            if (length(response_parts) == event_count) {
              return(response_parts)
            }

            ## Locate Collection Boundaries
            group_ends <- which(
              !nzchar(response_parts) | endsWith(response_parts, "\n")
            )

            ## Locate Database-Object Boundaries
            if (
              length(group_ends) != event_count ||
              group_ends[[event_count]] != length(response_parts)
            ) {
              tab_counts <- lengths(regmatches(
                response_parts,
                gregexpr("\t", response_parts, fixed = TRUE)
              ))
              total_tabs <- sum(tab_counts)
              group_ends <- integer()

              ## Use Consistent Tab Counts
              if (
                total_tabs >= event_count &&
                total_tabs %% event_count == 0L
              ) {
                tabs_per_event <- total_tabs %/% event_count
                cumulative_tabs <- cumsum(tab_counts)
                target_tabs <- seq_len(event_count) * tabs_per_event
                unique_boundaries <- !duplicated(cumulative_tabs) &
                  !duplicated(cumulative_tabs, fromLast = TRUE)
                group_ends <- which(
                  unique_boundaries & cumulative_tabs %in% target_tabs
                )
              }
            }

            ## Return Grouped Values
            if (
              length(group_ends) == event_count &&
              group_ends[[event_count]] == length(response_parts)
            ) {
              group_starts <- c(1L, group_ends[-event_count] + 1L)
              parsed_output <- vapply(
                X = seq_len(event_count),
                FUN = function(group) {
                  paste(
                    response_parts[group_starts[[group]]:group_ends[[group]]],
                    collapse = ", "
                  )
                },
                FUN.VALUE = character(1),
                USE.NAMES = FALSE
              )
              return(parsed_output)
            }
          }
        }

        ## Return the Unseparated Response
        warning(
          sprintf(
            paste0(
              "Reactome's '%s' response could not be separated ",
              "unambiguously; returning the unmodified response."
            ),
            attribute_name
          ),
          call. = get("diagnostics")
        )
        return(parsed_response)
      }
    )
    file_ext <- "txt"

  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    accept = accept_input,
    parser = parser_input,
    save_to = .rba_file(paste0("reactome_pathways_events", ".", file_ext))
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get lower level pathways Containing a 'Physical Entity' or Event
#'
#' Use this function to search the event hierarchy and retrieve a list of
#'   all lower level pathways (non TopLevelPathway class) that contain
#'   a given 'Physical Entity' or Event. See "Arguments section" on how to
#'   modify your search.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/pathways/low/entity/\{id\}"
#'  \cr "GET https://reactome.org/ContentService/data/pathways/low/diagram/
#'  entity/\{id\}"
#'  \cr "GET https://reactome.org/ContentService/data/pathways/low/diagram/
#'  entity/\{id\}/allForms"
#'
#' @param entity_id Character: The entity that should exist in the pathways.
#' @param with_diagram Logical: (default = \code{FALSE}) only include pathways
#'   with diagram?
#' @param all_forms Logical: (default = \code{FALSE}) should other variants of
#'   your supplied entity_id be considered? (e.g. same molecule but in different
#'   compartment, secretory form etc.) see
#'   \code{\link{rba_reactome_participants}}'s "Details section" to learn more
#'   about how Reactome classifies molecules.
#' @param species Character or Numeric: (optional) confine your search to a
#'   specific species by providing it's NCBI Taxonomy identifier
#'   (Human Taxonomy ID is 9606) or species name (e.g. "Homo sapiens").
#'   See \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Data frame where each row is a pathway that contains your supplied
#'   entity and columns are pertinent information.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_pathways_low(entity_id = "R-HSA-199420")
#' }
#' \donttest{
#' rba_reactome_pathways_low(entity_id = "R-HSA-199420", with_diagram = TRUE)
#' }
#' \donttest{
#' rba_reactome_pathways_low(entity_id = "R-HSA-199420", with_diagram = TRUE,
#'     all_forms = TRUE)
#' }
#'
#' @family "Reactome Content Service - Pathway Related Queries"
#' @export
rba_reactome_pathways_low <- function(entity_id,
                                      with_diagram = FALSE,
                                      all_forms = FALSE,
                                      species = NULL,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "entity_id", class = "character", len = 1L),
      list(arg = "all_forms", class = "logical", len = 1L),
      list(arg = "with_diagram", class = "logical", len = 1L),
      list(arg = "species", class = c("character", "numeric", "integer"), len = 1L)
    )
  )

  .msg(
    "Retrieving lower-level pathways that include %sentity %s%s.",
    ifelse(isTRUE(all_forms), yes = "any form of ", no = ""),
    entity_id,
    ifelse(isTRUE(with_diagram), yes = " and have diagram", no = "")
  )

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%sdata/pathways/%s/%s",
    .rba_stg("reactome", "pth", "content"),
    ifelse(isTRUE(with_diagram), yes = "low/diagram/entity", no = "low/entity"),
    entity_id)

  if (isTRUE(all_forms)) {
    path_input <- paste0(path_input, "/allForms")
  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    query = .rba_query(
      init = list(),
      list("species", !is.null(species), species)
    ),
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("reactome_pathways_low.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Top Level Pathways in a Species
#'
#' This function will Return a list of all pathways with the class
#'   "TopLevelPathway" which are annotated in your supplied species.
#'
#' Reactome's events hierarchy for any species begins with pathways with
#'   class "TopLevelPathway" (e.g. "Immune System", "Metabolism of proteins").
#'   further down in the event's hierarchy tree, each TopLevelPathway has
#'   has other events itself (e.g. "Adaptive immune system", "Innate immune
#'   system"). Based on the chosen pathway, the hierarchy tree would typically
#'   goes further down.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/pathways/top/\{species\}"
#'
#' @param species Character or Numeric: NCBI Taxonomy identifier (Human Taxonomy
#'   ID is 9606.) or species name (e.g. "Homo sapiens"). See
#'   \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Data frame where each row is a Top Level Pathway and columns are
#'   pertinent information.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_pathways_top(species = 9606)
#' }
#' \donttest{
#' rba_reactome_pathways_top(species = "Saccharomyces cerevisiae")
#' }
#'
#' @family "Reactome Content Service - Pathway Related Queries"
#' @export
rba_reactome_pathways_top <- function(species,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "species", class = c("character", "numeric", "integer"), len = 1L)
    )
  )

  .msg(
    "Retrieving all Reactome top level pathways of species %s.",
    species
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = paste0(
      .rba_stg("reactome", "pth", "content"),
      "data/pathways/top/",
      species
    ),
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("reactome_pathways_top.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Person Endpoints ####

#' Get Persons Information by Name
#'
#' Using this function you can query people by partially matching or exact
#'   name and retrieve a list of matching people in Reactome.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/people/name/\{name\}"
#'  \cr "GET https://reactome.org/ContentService/data/people/name/\{name\}/exact"
#'
#' @param person_name Character: first and last name of the person
#' @param exact_match Logical: (default = \code{FALSE}) should the supplied name
#'   be considered as an exact match?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List where each element is a search hit contains the person's
#'   information.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_people_name("Jupe")
#' }
#' \donttest{
#' rba_reactome_people_name("Steve Jupe", exact_match = TRUE)
#' }
#'
#' @family "Reactome Content Service - Person Queries"
#' @export
rba_reactome_people_name <- function(person_name,
                                     exact_match = FALSE,
                                     ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "person_name", class = "character", len = 1L),
      list(arg = "exact_match", class = "logical", len = 1L)
    )
  )

  .msg(
    "Retrieving the information of %s.",
    person_name
  )

  ## Build Function-Specific Call
  path_input <- paste0(
    .rba_stg("reactome", "pth", "content"),
    "data/people/name/",
    gsub(" ", "%20", person_name)
  )

  if (isTRUE(exact_match)) {
    path_input <- paste0(path_input, "/exact")
  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("reactome_people_name.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get a Person by Identifier
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/person/\{id\}"
#'  \cr "GET https://reactome.org/ContentService/data/person/\{id\}/
#'  authoredPathways"
#'  \cr "GET https://reactome.org/ContentService/data/person/\{id\}/publications"
#'  \cr "GET https://reactome.org/ContentService/data/person/\{id\}/
#'  \{attributeName\}"
#'
#' @param person_id Character: Reactome database ID (DbId) or ORCID identifier.
#' @param authored_pathways Logical: (default = \code{FALSE}) Only return
#'   Pathway list authored by the person?
#' @param publications Logical: (default = \code{FALSE}) Only return
#'   publications list authored by the person?
#' @param attribute_name Character: (optional) A Reactome person attribute to
#'   return only.
#'   see \href{https://reactome.org/content/schema/Person/}{Reactome Data
#'   Schema: person} for available options.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List containing the requested informations of your supplied
#'   person.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_people_id("391309")
#' }
#' \donttest{
#' rba_reactome_people_id(person_id = "391309", authored_pathways = TRUE)
#' }
#'
#' @family "Reactome Content Service - Person Queries"
#' @export
rba_reactome_people_id <- function(person_id,
                                   authored_pathways = FALSE,
                                   publications = FALSE,
                                   attribute_name = NULL,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "person_id", class = "character", len = 1L),
      list(arg = "authored_pathways", class = "logical", len = 1L),
      list(arg = "publications", class = "logical", len = 1L),
      list(arg = "attribute_name", class = "character", len = 1L)
    ),
    cond = list(
      list(
        quote(sum(!is.null(attribute_name), isTRUE(authored_pathways), isTRUE(publications)) > 1),
        "You can only use either attribute_name, authored_pathways or publications function call."
      )
    )
  )

  .msg(
    "Retrieving information of person with id %s.",
    person_id
  )

  ## Build Function-Specific Call
  path_input <- paste0(
    .rba_stg("reactome", "pth", "content"),
    "data/person/",
    person_id
  )

  accept_input <- "application/json"
  parser_type_input <- "json->list"
  file_ext <- "json"

  if (isTRUE(authored_pathways)) {

    path_input <- paste0(path_input, "/authoredPathways")

  } else if (isTRUE(publications)) {

    path_input <- paste0(path_input, "/publications")

  } else if (!is.null(attribute_name)) {

    path_input <- paste0(path_input, "/", attribute_name)
    accept_input <- "text/plain"
    parser_type_input <- "text->chr"
    file_ext <- "txt"

  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    accept = accept_input,
    parser = parser_type_input,
    save_to = .rba_file(paste0("reactome_people_id", ".", file_ext))
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Query Endpoints ####

#' Query and Retrieve any Reactome knowledge-base Object
#'
#' Using this Comprehensive function, You can Retrieve any object from
#'   \href{https://reactome.org/content/schema/DatabaseObject/}{Reactome
#'   knowledge-base}
#'
#' @section Corresponding API Resources:
#'  "POST https://reactome.org/ContentService/data/query/ids"
#'  \cr "POST https://reactome.org/ContentService/data/query/ids/map"
#'  \cr "GET https://reactome.org/ContentService/data/query/\{id\}"
#'  \cr "GET https://reactome.org/ContentService/data/query/enhanced/\{id\}"
#'  \cr "GET https://reactome.org/ContentService/data/query/\{id\}/\{attributeName\}"
#'
#' @param ids Character or Numeric vector: A single or Multiple database IDs
#'   (DbId), Stable IDs (StId) or a mixture of both.
#' @param enhanced Logical: (default = \code{FALSE}) If 'TRUE' more information
#'   on the supplied entry will be returned. (You can set this argument to
#'   'TRUE' Only when you supply a single ID).
#' @param map Logical: (default = \code{FALSE}) Should the supplied IDs be
#'   mapped? This argument will only be considered when you supply multiple IDs.
#'   (e.g. when you supply previous version of stable identifiers.)
#' @param attribute_name Character: (optional) Only Return an Attribute of the
#'   supplied Database Object. (You can use this argument Only when you supply a
#'   single ID)
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List containing your query outputs.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_query(ids = c("8953958", "11982506", "R-ALL-9649879"))
#' }
#' \donttest{
#' rba_reactome_query(ids = "R-HSA-9656256", enhanced = TRUE)
#' }
#' \donttest{
#' rba_reactome_query(ids = "8863054", attribute_name = "displayName")
#' }
#'
#' @family "Reactome Content Service - Common Data Retrieval"
#' @export
rba_reactome_query <- function(ids,
                               enhanced = FALSE,
                               map = FALSE,
                               attribute_name = NULL,
                               ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "ids", class = c("character", "numeric", "integer"),
        min_len = 1L, max_len = 20
      ),
      list(arg = "enhanced", class = "logical", len = 1L),
      list(arg = "map", class = "logical", len = 1L),
      list(arg = "attribute_name", class = "character", len = 1L)
    ),
    cond = list(
      list(
        quote(length(ids) > 1 && (isTRUE(enhanced) | !is.null(attribute_name))),
        "You can only use `enhanced` or `attribute_name` with a single ID."
      ),
      list(
        quote(!is.null(attribute_name) && isTRUE(enhanced)),
        "You can only supply 'attribute_name' when enhanced is 'FALSE'."
      )
    )
  )

  .msg(
    "Querying Reactome knowledgebase with the supplied ID(s)"
  )

  if (length(ids) > 1) {

    #### use POST
    ## Build POST API Request's URL
    call_body <- paste(unique(ids),collapse = ",")
    path_input <- paste0(
      .rba_stg("reactome", "pth", "content"),
      ifelse(isTRUE(map), yes = "data/query/ids/map", no = "data/query/ids")
    )

    ## Build Function-Specific Call
    input_call <- .rba_httr(
      httr = "post",
      url = .rba_stg("reactome", "url"),
      path = path_input,
      body = call_body,
      parser = "json->list",
      accept = "application/json",
      httr::content_type("text/plain"),
      save_to = .rba_file("reactome_query.json")
    )

  } else {

    #### use GET
    ## Build Function-Specific Call
    path_input <- paste0(
      .rba_stg("reactome", "pth", "content"),
      "data/query/",
      ids
    )
    accept_input <- "application/json"
    parser_input <- "json->list"
    file_ext <- "json"

    if (!is.null(attribute_name)) {

      path_input <- paste0(path_input, "/", attribute_name)
      accept_input <- "text/plain"
      parser_input <- "text->chr"
      file_ext <- "txt"

    } else if (isTRUE(enhanced)) {

      path_input <- sub("/query/", "/query/enhanced/", path_input)

    }

    input_call <- .rba_httr(
      httr = "get",
      url = .rba_stg("reactome", "url"),
      path = path_input,
      parser = parser_input,
      accept = accept_input,
      save_to = .rba_file(paste0("reactome_query.", file_ext))
    )
  }

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Search Endpoints ####

#' Search the Reactome Knowledgebase
#'
#' Search Reactome for entries that match a text query. The search can be
#'   limited by species, result type, cellular compartment, and keyword.
#'
#' By default, matches are separated into groups such as proteins, pathways,
#'   and reactions. In this case, \code{page_size} is applied separately to
#'   each group. If \code{cluster = FALSE}, matches are returned in one group
#'   and \code{page_size} applies to that group.
#'
#' Reactome normally removes supplied filters when they produce no matches.
#'   Set \code{force_filters = TRUE} to require all supplied filters, so that a
#'   search with no filtered matches is reported instead of being broadened.
#'   Reactome also marks matching text in some returned names and descriptions;
#'   rbioapi leaves these highlighting markers unchanged.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/search/query"
#'
#' @param query Character: Text to search for in the Reactome knowledgebase.
#' @param species Character vector: (optional) Scientific species name(s) used
#'   to filter the results, e.g. \code{"Homo sapiens"}. See
#'   \code{\link{rba_reactome_species}} for species available in Reactome.
#' @param types Character vector: (optional) Result type(s) used to filter the
#'   search, e.g. \code{"Protein"}, \code{"Pathway"}, or \code{"Reaction"}.
#' @param compartments Character vector: (optional) Cellular compartment
#'   name(s) used to filter the results.
#' @param keywords Character vector: (optional) Reactome search keyword(s) used
#'   to filter the results.
#' @param cluster Logical: (default = \code{TRUE}) Should matches be separated
#'   into groups according to their result type? If FALSE, matches are returned
#'   in one ranked group.
#' @param page_size Numeric: (default = \code{10}) Maximum number of matches to
#'   return from each result group on a page. If \code{cluster = FALSE}, this is
#'   the maximum number returned from the single combined group.
#' @param page Numeric: (default = \code{1}) One-based results page to retrieve.
#' @param scope Character: (default = \code{"PHYSICAL_ENTITY"}) Which form of
#'   matching entities should be returned? Can be one of: \itemize{
#'   \item "PHYSICAL_ENTITY": Return specific physical forms annotated in
#'     Reactome.
#'   \item "REFERENCE_ENTITY": Group applicable physical forms by their
#'     underlying reference molecule; entries without a reference molecule are
#'     retained as physical entities.
#'   \item "BOTH": Return both representations.}
#' @param force_filters Logical: (default = \code{FALSE}) Should Reactome keep
#'   all supplied filters when they produce no matches? If FALSE, Reactome may
#'   remove the filters and return results from a broader search.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#'
#' @return A list with the following elements: \describe{
#'   \item{results}{A data frame with one row per returned result group. The
#'   \code{entries} column contains data frames of matching Reactome entries;
#'   their fields vary according to result type.}
#'   \item{rowCount}{Number of matching entries returned on the requested page.}
#'   \item{numberOfGroups}{Number of matching result groups reported by
#'   Reactome.}
#'   \item{numberOfMatches}{Total number of matches reported by Reactome.}
#'   }
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_search(
#'   query = "TP53",
#'   species = "Homo sapiens",
#'   types = c("Protein", "Pathway")
#' )
#' }
#' \donttest{
#' rba_reactome_search(
#'   query = "apoptosis",
#'   cluster = FALSE,
#'   page_size = 20
#' )
#' }
#'
#' @family "Reactome Content Service - Search"
#' @seealso
#' \code{\link{rba_reactome_query}}
#' \code{\link{rba_reactome_species}}
#' \code{\link{rba_pages}}
#' @export
rba_reactome_search <- function(query,
                                species = NULL,
                                types = NULL,
                                compartments = NULL,
                                keywords = NULL,
                                cluster = TRUE,
                                page_size = 10,
                                page = 1,
                                scope = "PHYSICAL_ENTITY",
                                force_filters = FALSE,
                                ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "query", class = "character", len = 1L),
      list(arg = "species", class = "character", min_len = 1L),
      list(arg = "types", class = "character", min_len = 1L),
      list(arg = "compartments", class = "character", min_len = 1L),
      list(arg = "keywords", class = "character", min_len = 1L),
      list(arg = "cluster", class = "logical", len = 1L, no_null = TRUE),
      list(
        arg = "page_size", class = c("numeric", "integer"),
        len = 1L, min_val = 1, no_null = TRUE
      ),
      list(
        arg = "page", class = c("numeric", "integer"),
        len = 1L, min_val = 1, no_null = TRUE
      ),
      list(
        arg = "scope", class = "character", len = 1L, no_null = TRUE,
        val = c("REFERENCE_ENTITY", "PHYSICAL_ENTITY", "BOTH")
      ),
      list(
        arg = "force_filters", class = "logical", len = 1L,
        no_null = TRUE
      )
    ),
    cond = list(
      list(
        quote(!nzchar(trimws(query))),
        "`query` should contain at least one non-whitespace character."
      ),
      list(
        quote(any(!nzchar(trimws(c(species, types, compartments, keywords))))),
        "Search filters cannot contain empty character strings."
      ),
      list(
        quote(!is.finite(page_size) || page_size != floor(page_size)),
        "`page_size` should be a finite, positive whole number."
      ),
      list(
        quote(!is.finite(page) || page != floor(page)),
        "`page` should be a finite, positive whole number."
      )
    )
  )

  .msg(
    "Searching the Reactome knowledgebase for '%s'.",
    query
  )

  ## Build GET API Request's query
  start_row <- (page - 1) * page_size
  call_query <- .rba_query(
    init = list(
      "query" = query,
      "cluster" = ifelse(isTRUE(cluster), yes = "true", no = "false"),
      "Start row" = start_row,
      "rows" = page_size,
      "scope" = scope,
      "Force filters" = ifelse(
        isTRUE(force_filters), yes = "true", no = "false"
      )
    ),
    list("species", !is.null(species), species),
    list("types", !is.null(types), types),
    list("compartments", !is.null(compartments), compartments),
    list("keywords", !is.null(keywords), keywords)
  )

  ## Expand vector filters into possibly repeated Reactome query parameters
  ## (species, types, compartments, and keywords)
  query_names <- rep(names(call_query), lengths(call_query))
  call_query <- as.list(unlist(call_query, use.names = FALSE))
  names(call_query) <- query_names

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = paste0(
      .rba_stg("reactome", "pth", "content"),
      "search/query"
    ),
    query = call_query,
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("reactome_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### References Endpoints ####

#' Map Cross References IDs to Reactome ReferenceEntity
#'
#' Use this function To retrieve a list of Reactome ReferenceEntity associated
#'   to your supplied Cross Reference (i.e. External) ID.
#'
#' Reactome cross-references external database's identifiers to it's database
#'   Entries named ReferenceEntity, which resembles the invariant aspect of
#'   a molecule. Thus there is a one-to-many relationship between Reactome's
#'   ReferenceEntity object and the molecule's ID in external databases,
#'   which in Reactome's terms is called Cross Reference.
#'   \cr See \code{\link{rba_reactome_participants}}'s "Details section"
#'   to learn more about how Reactome classifies molecules.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/references/mapping/\{identifier\}"
#'
#' @param xref_id Character or Numeric: molecule's cross-reference (external)
#'   identifier.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List containing the ReferenceEntity corresponding to your
#'   supplied cross-reference (external) ID.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_xref("CD40")
#' }
#' \donttest{
#' rba_reactome_xref("ENSP00000361350")
#' }
#'
#' @family "Reactome Content Service - ReferenceEntity Queries"
#' @export
rba_reactome_xref <- function(xref_id,
                              ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "xref_id", class = c("character", "numeric", "integer"), len = 1L)
    )
  )

  .msg(
    "Retrieving Reactome's ReferenceEntity that have a cross-reference to %s.",
    xref_id
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = paste0(
      .rba_stg("reactome", "pth", "content"),
      "references/mapping/",
      xref_id
    ),
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("reactome_xref.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### species Endpoints ####

#' Get Reactome Species
#'
#' Use this function to retrieve a table of Available species in Reactome.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/species/all"
#'  \cr "GET https://reactome.org/ContentService/data/species/main"
#'
#' @param only_main Logical: (default = \code{FALSE}) If set to TRUE, will only
#'   return species which have either manually-curated or computationally
#'   inferred pathways.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Data frame where each row is a species and columns are pertinent
#'   information.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A,
#'   Hermjakob H. ReactomeGSA - Efficient Multi-Omics Comparative Pathway
#'   Analysis. Mol Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed
#'   PMID: 32907876.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite/}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_species()
#' }
#' \donttest{
#' rba_reactome_species(only_main = TRUE)
#' }
#'
#' @family "Reactome Content Service - Species Related Queries"
#' @export
rba_reactome_species <- function(only_main = FALSE,
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "only_main", class = "logical", len = 1L)
    )
  )

  .msg(
    "Retrieving %sspecies available in Reactome.",
    ifelse(isTRUE(only_main), yes = "main (i.e. with pathways) ", no = "")
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = paste0(
      .rba_stg("reactome", "pth", "content"),
      "data/species/",
      ifelse(isTRUE(only_main), yes = "main", no = "all")
    ),
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("reactome_species.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
