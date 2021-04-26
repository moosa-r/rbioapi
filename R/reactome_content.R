#### database Endpoints ####
#' The version number of current database
#'
#' Returns the current version of Reactome database.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/database/version"
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Character string containing the version of Reactome database.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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

  .msg("Retrieving Reactome Content Service's database version.")

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = paste0(.rba_stg("reactome", "pth", "content"),
                                        "data/database/version"),
                          accpet = "text/plain",
                          parser = "text->chr",
                          save_to = .rba_file("reactome_diseases.txt"))

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
#'  "GET https://reactome.org/ContentService/GET data/diseases"
#'  "GET https://reactome.org/ContentService/GET data/diseases/doid"
#'
#' @param doid (logical) Return disease DOIDs instead of diseases?
#'   (default = FALSE)
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame containing all the disease annotation available at
#'   Reactome. If doid was set to TRUE, DOID info will be returned instead.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "doid",
                             class = "logical")))

  .msg("Retrieving Reactome's diseases %s.",
       ifelse(isTRUE(doid), yes = "DOID data", no = "annotations"))

  ## Build Function-Specific Call
  if (isFALSE(doid)) {
    path_input <- paste0(.rba_stg("reactome", "pth", "content"),
                         "data/diseases")
    accept_input <- "application/json"
    parser_input <- "json->df"
    file_ext <- "json"
  } else {
    path_input <- paste0(.rba_stg("reactome", "pth", "content"),
                         "data/diseases/doid")
    accept_input <- "text/plain"
    parser_input <- "text->df"
    file_ext <- "txt"
  }
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          accpet = accept_input,
                          parser = parser_input,
                          save_to = .rba_file(paste0("reactome_diseases.",
                                                     file_ext)))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Entities Endpoints ####

#' Get a Complex's Subunits
#'
#' This function will return a list of subunits which are participants of
#' your provided complex.
#'
#' Subunits will be returned recursively; Which means that if a subunit was
#'   itself a complex, subunit of that complex will be also returned in the
#'   results.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/complex/{id}/subunits"
#'
#' @param complex_id Reactome stable Identifier of the complex.
#' @param exclude_structures (logical) Should the contained complexes and
#'   entity sets be excluded from the results? (default = FALSE)
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame which each row is a subunit of your provided complex
#' and the columns are pertinent information of that subunit.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_complex_subunits(complex_id = "R-HSA-5674003",
#'     exclude_structures = FALSE)
#' }
#' \donttest{
#' rba_reactome_complex_subunits(complex_id = "R-HSA-109783",
#'     exclude_structures = TRUE)
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
  .rba_args(cons = list(list(arg = "complex_id",
                             class = "character"),
                        list(arg = "exclude_structures",
                             class = "logical")))

  .msg("Recursively retrieving subunits of %s complex.", complex_id)
  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("excludeStructures",
                                exclude_structures,
                                "true"))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sdata/complex/%s/subunits",
                                         .rba_stg("reactome", "pth", "content"),
                                         complex_id),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("reactome_complex_subunits.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Complexes That Include a Molecule
#'
#' This function will retrieve a list of complexes that include your provided
#'   molecule as a component.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/complexes/{resource}/
#'  {identifier}"
#'
#' @param id Molecule's external Identifier
#' @param resource What is the resource of your provided ID? see:
#' \href{https://reactome.org/content/schema/objects/ReferenceDatabase}{Reactome External
#' Identifiers}
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame where each row is a complex containing your provided
#'   molecule and columns are pertinent information.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "id",
                             class = "character"),
                        list(arg = "resource",
                             class = "character")))

  .msg("Retrieving complexes that contain a molecule with '%s ID: %s'.",
       resource, id)

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sdata/complexes/%s/%s",
                                         .rba_stg("reactome", "pth", "content"),
                                         resource, id),
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("reactome_complex_list.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)

  return(final_output)
}

#' Get Larger Reactome Structures Which Include an Entity
#'
#' This function will retrieve a list of complexes and sets that Your
#'   provided entity ID participates in (e.g. as a complex component,
#'   reaction output).
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/entity/{id}/componentOf"
#'
#' @param entity_id Reactome's entity ID.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List of Reactome database Entities which Your provided ID is a
#'   participant in them.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "entity_id",
                             class = "character")))

  .msg("Retrieving Reactome structures which have %s as a participant.",
       entity_id)
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sdata/entity/%s/componentOf",
                                         .rba_stg("reactome", "pth", "content"),
                                         entity_id),
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("rba_reactome_participant_of.json"))
  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Other forms of a Reactome Entity
#'
#' This function retrieve a list containing all other forms of your provided
#'   Physical Entity ID.
#'
#' According to Reactome API documentation, "These other forms are Physical
#'   Entities that share the same Reference Entity identifier, e.g. PTEN
#'   H93R R-HSA-2318524 and PTEN C124R R-HSA-2317439 are two forms of PTEN."
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/entity/{id}/otherForms"
#'
#' @param entity_id Reactome's entity ID.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame where each row is other forms of your provided Entity ID
#'   and columns are pertinent information.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "entity_id",
                             class = "character")))

  .msg("Retrieving Other forms of Reactome's entity: %s", entity_id)
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sdata/entity/%s/otherForms",
                                         .rba_stg("reactome", "pth", "content"),
                                         entity_id),
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("reactome_entity_other_forms.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Events Endpoints ####

#' Get Reactome Events Ancestors
#'
#' Along with Reactome's events hierarchy, This function will retrieve all the
#'  events beginning from your provided event up to the "Top level Pathway".
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
#'  "GET https://reactome.org/ContentService/data/event/{id}/ancestors"
#'
#' @param event_id Reactome event's identifier.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List which every element is a Data frame listing your provided
#'   event along with it's ancestor events. Because any given event can be
#'   part of more than one pathway hierarchy, the list may contain multiple
#'   data frames.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "event_id",
                             class = "character")))

  .msg("GET /data/event/{id}/ancestors",
       "The ancestors of a given event")
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sdata/event/%s/ancestors",
                                         .rba_stg("reactome", "pth", "content"),
                                         event_id),
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("reactome_event_ancestors.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Full Event Hierarchy of a Species
#'
#' This function will retrieve the full Events hierarchy of your provided
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
#'  "GET https://reactome.org/ContentService/data/eventsHierarchy/{species}"
#'
#' @param species Numeric or Character: NCBI Taxonomy identifier (Human Taxonomy
#'    ID is 9606.) or species name (e.g. "Homo sapiens"). Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List which is a representation of the species's events hierarchy
#'   described in the "Details section".
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_event_hierarchy("Homo sapiens")
#' }
#' \donttest{
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
  .rba_args(cons = list(list(arg = "species",
                             class = c("character",
                                       "numeric"))))

  .msg("Retrieving the complete events hierarchy tree of the Specie %s.",
       species)
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sdata/eventsHierarchy/%s",
                                         .rba_stg("reactome", "pth", "content"),
                                         species),
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("reactome_event_hierarchy.json"))

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
#'   \cr The result will be an image with the format provided in "output_format"
#'   argument. If the provided event ID refers to a pathway, the image's
#'   content will be the that pathways diagram. If the provided event ID refers
#'   to a sub-pathway or reaction event, the parent pathway's diagram will be
#'   exported, with that reaction or sub-pathway's events highlighted.
#'   \cr Note that to export an image of reaction-like event separately, you
#'   should use \code{\link{rba_reactome_exporter_reaction}}.
#'   \cr If the function is called with create_document = TRUE:
#'   \cr A PDF document will contain an image of the event's diagram and the
#'   following information of that events: Summation, Literature references,
#'   Edit history type, location, compartments and diseases.
#'   note that if you call the function with "document level = 1", information
#'   of your provided event's children will also be included.
#'
#' @section Corresponding API Resources:
#'   "GET https://reactome.org/ContentService/exporter/diagram/{identifier}
#'   .{ext}"
#'   "GET https://reactome.org/ContentService/exporter/document/event/
#'   {identifier}.pdf"
#'
#' @param event_id Reactome event's identifier.
#' @param save_to NA or Character:\itemize{
#'   \item NA: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param create_document logical: Create PDF document instead of image? (
#'   default = FALSE)
#' @param resource The analysis resource for which the results will be
#'   overlaid on top of the given pathways overview,
#' @param diagram_profile Color profile of diagrams, should be either "Modern"
#'   (default) or "Standard".
#' @param analysis_profile Color profile of analysis, should be one of:
#'   "Standard" (default), "Strosobar" or "Copper Plus"
#' @param token The analysis Token for which the results will be overlaid on
#'   top of the given pathways overview. see:
#'   \code{\link{rba_reactome_analysis}}.
#' @param exp_column numeric: (only if token is provided) Specify the
#'   expression column for the overlay.
#' @param document_level numeric: (Only if "create_document" is TRUE) if 0
#' (default) the event's children will not be included in the PDF document.
#'  Set this to 1 to include event's children.
#' @param output_format (Only if "create_document" is FALSE) Image format
#'   of the saved diagram. Can be one of: png (default), jpeg, svg or gif.
#' @param image_quality Numeric: (Only if "create_document" is FALSE), a number
#'   ranging from 1 to 10. 1 is the lowest quality and 10 is the highest
#'   (default = 5).
#' @param flag_element (Only if "create_document" is FALSE) gene name, protein
#'   ID, chemical ID or Reactome ID of a diagram's element to be flagged.
#' @param flg_interactors Logical: (Only if "create_document" is FALSE) Should
#'   the interactor be considered when flagging a diagram element? (default
#'   = TRUE)
#' @param sel (Only if "create_document" is FALSE) CSV line for highlighting
#'   element(s) selection in the diagram.
#' @param title Logical: (Only if "create_document" is FALSE) Should the pathway
#'  name be displayed below the image? (default = TRUE)
#' @param margin Numeric: (Only if "create_document" is FALSE) A number ranging
#'   from 0 to 20 to set as the image's margin. (default = 15)
#' @param ehld logical: (Only if "create_document" is FALSE) Should
#'   "Enhanced High Level Diagram" be considered?
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return NULL, Based to the inputs, an image or PDF file will be saved to
#'   disk.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
                                          save_to = NA,
                                          create_document = FALSE,
                                          resource = "TOTAL",
                                          diagram_profile = "Modern",
                                          analysis_profile = "Standard",
                                          token = NA,
                                          exp_column = NA,
                                          document_level  = 1,
                                          output_format = "png",
                                          image_quality = 5,
                                          flag_element = NA,
                                          flg_interactors = TRUE,
                                          sel = NA,
                                          title = TRUE,
                                          margin = 15,
                                          ehld = FALSE,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "event_id",
                             class = "character"),
                        list(arg = "save_to",
                             class = "character"),
                        list(arg = "document_level",
                             class = "numeric",
                             val = c(0,1)),
                        list(arg = "output_format",
                             class = "character",
                             val = c("png",
                                     "jpeg",
                                     "svg",
                                     "gif")),
                        list(arg = "image_quality",
                             class = "numeric",
                             ran = c(1,10)),
                        list(arg = "flag_element",
                             class = "character"),
                        list(arg = "flg_interactors",
                             class = "logical"),
                        list(arg = "sel",
                             class = "character"),
                        list(arg = "title",
                             class = "logical"),
                        list(arg = "margin",
                             class = "numeric",
                             ran = c(0,20)),
                        list(arg = "ehld",
                             class = "logical"),
                        list(arg = "diagram_profile",
                             class = "character",
                             val = c("Modern",
                                     "Standard")),
                        list(arg = "token",
                             class = "character"),
                        list(arg = "resource",
                             class = "character"),
                        list(arg = "analysis_profile",
                             class = "character",
                             val = c("Standard",
                                     "Strosobar",
                                     "Copper Plus")),
                        list(arg = "exp_column",
                             class = "numeric")),
            cond = list(list(quote(!is.na(exp_column) && is.na(token)),
                             "You cannot specify expression column without providing a token.")))

  ## Build Function-Specific Call
  call_query <- .rba_query(init = list(),
                           list("resource",
                                resource != "TOTAL",
                                resource),
                           list("diagramProfile",
                                !is.na(diagram_profile),
                                diagram_profile),
                           list("analysisProfile",
                                !is.na(analysis_profile),
                                analysis_profile),
                           list("token",
                                !is.na(token),
                                token),
                           list("expColumn",
                                !is.na(exp_column),
                                exp_column))

  if (isTRUE(create_document)) {
    .msg("Retrieving a PDF document of event %s details.", event_id)
    ## Build Function-Specific Call
    call_query <- .rba_query(init = call_query,
                             list("level",
                                  document_level != 1,
                                  document_level))

    accept_input <- "application/pdf"
    output_format <- "pdf"
    path_input <- sprintf("%sexporter/document/event/%s.pdf",
                          .rba_stg("reactome", "pth", "content"),
                          event_id)
  } else {
    .msg("Retrieving event %s diagram's image in %s format.",
         event_id, output_format)
    ## Build Function-Specific Call
    call_query <- .rba_query(init = call_query,
                             list("quality",
                                  image_quality != 5,
                                  image_quality),
                             list("flg",
                                  !is.na(flag_element),
                                  flag_element),
                             list("flgInteractors",
                                  !flg_interactors,
                                  "false"),
                             list("sel",
                                  !is.na(sel),
                                  sel),
                             list("title",
                                  !title,
                                  "false"),
                             list("margin",
                                  margin != 15,
                                  as.integer(margin)),
                             list("ehld",
                                  !ehld,
                                  "false"))

    accept_input <- ifelse(output_format == "svg",
                           yes = "image/svg+xml",
                           no = paste0("image/", output_format))
    path_input <- sprintf("%sexporter/diagram/%s.%s",
                          .rba_stg("reactome", "pth", "content"),
                          event_id, output_format)
  }
  # create file_path
  save_to <- .rba_file(file = paste0(event_id, ".", output_format),
                       save_to = ifelse(is.na(save_to),
                                        yes = TRUE,
                                        no = save_to))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          query = call_query,
                          accpet = accept_input,
                          save_to = save_to,
                          parser = NULL)
  ## Call API
  invisible(.rba_skeleton(input_call))
}

#' Exports A Reactome Event to SBGN or SBML
#'
#' This function will export a provided Reactome Event (Pathway or Reaction)
#'   to a SBGN (Systems Biology Graphical Notation) or SBML (Systems Biology
#'   Markup Language)
#'
#' @param event_id Reactome event's database IDs (DbId) or Stable IDs (StId).
#' @param output_format Either "sbgn" or "sbml".
#' @param save_to NA or Character:\itemize{
#'   \item NA: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService//exporter/event/
#'  {identifier}.sbgn"
#'  "GET https://reactome.org/ContentService//exporter/event/
#'  {identifier}.sbml"
#'
#' @return NULL, According to the inputs, a SBGN or SBML file will be saved to
#'   disk.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
                                        save_to = NA,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "event_id",
                             class = "character"),
                        list(arg = "output_format",
                             class = "character",
                             val = c("sbgn",
                                     "sbml")),
                        list(arg = "save_to",
                             class = "character")))

  .msg("Exporting event %s as a %s file.",
       event_id, output_format)
  ## Build Function-Specific Call
  # create file_path
  save_to <- .rba_file(file = paste0(event_id, ".", output_format),
                       save_to = ifelse(is.na(save_to),
                                        yes = TRUE,
                                        no = save_to))
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sexporter/event/%s.%s",
                                         .rba_stg("reactome", "pth", "content"),
                                         event_id,
                                         output_format),
                          save_to = save_to,
                          parser = NULL)

  ## Call API
  invisible(.rba_skeleton(input_call))
}

#' Get a Reactome Pathway Overview
#'
#' This function will Save a Pathway Overview of the provided specie
#'   as an image file.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/exporter/fireworks/{species}.{ext}"
#'
#' @param species Numeric or Character: NCBI Taxonomy identifier (Human Taxonomy
#'    ID is 9606.) or species name (e.g. "Homo sapiens"). Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param output_format Images format, Can be one of: png (default), jpeg,
#'   svg or gif.
#' @param save_to NA or Character:\itemize{
#'   \item NA: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param image_quality Numeric: A number  ranging from 1 to 10. 1 is the
#'   lowest quality and 10 is the highest (default = 5).
#' @param flag_element Gene name, protein ID, chemical ID or Reactome ID of a
#'   diagram's element to be flagged.
#' @param flg_interactors Logical: Should the interactor be considered when
#'   flagging a diagram element? (default = TRUE)
#' @param sel CSV line for highlighting element(s) selection in the diagram.
#' @param title Logical: Should the pathway name be displayed below the image?
#'   (default = TRUE)
#' @param margin Numeric: A number ranging from 0 to 20 to set as the image's
#'   margin. (default = 15)
#' @param diagram_profile Color profile of diagrams, should be one of "Copper"
#'   (default), "Copper Plus", "Barium Lithium" or "calcium salts".
#' @param token The analysis Token for which the results will be overlaid on
#'   top of the given pathways overview. see:
#'   \code{\link{rba_reactome_analysis}}.
#' @param resource The analysis resource for which the results will be
#'   overlaid on top of the given pathways overview.
#' @param exp_column numeric: (only if token is provided) Specify the
#'   expression column for the overlay.
#' @param coverage Logical: Should the analysis coverage values be overlaid?
#'   (default = FALSE)
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return NULL, Based to the inputs, an image file will be saved to disk.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
                                           save_to = NA,
                                           image_quality = 5,
                                           flag_element = NA,
                                           flg_interactors = TRUE,
                                           sel = NA,
                                           title = TRUE,
                                           margin = 15,
                                           diagram_profile = "Copper",
                                           token = NA,
                                           resource = "TOTAL",
                                           exp_column = NA,
                                           coverage = FALSE,
                                           ...) {

  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "species",
                             class = c("character",
                                       "numeric")),
                        list(arg = "output_format",
                             class = "character",
                             val = c("png",
                                     "jpeg",
                                     "svg",
                                     "gif")),
                        list(arg = "save_to",
                             class = "character"),
                        list(arg = "quality",
                             class = "numeric",
                             ran = c(1,10)),
                        list(arg = "flg",
                             class = "character"),
                        list(arg = "flg_interactors",
                             class = "logical"),
                        list(arg = "sel",
                             class = "character"),
                        list(arg = "title",
                             class = "logical"),
                        list(arg = "margin",
                             class = "numeric",
                             ran = c(0,20)),
                        list(arg = "diagram_profile",
                             class = "character",
                             val = c("Copper",
                                     "Copper Plus",
                                     "Barium Lithium",
                                     "calcium salts")),
                        list(arg = "token",
                             class = "character"),
                        list(arg = "resource",
                             class = "character"),
                        list(arg = "exp_column",
                             class = "character"),
                        list(arg = "coverage",
                             class = "logical")),
            cond = list(list(quote(!is.na(exp_column) && is.na(token)),
                             "You cannot specify expression column without providing a token.")))

  .msg("Retrieving specie %s pathway overview image in %s format.",
       species, output_format)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("quality",
                                image_quality != 5,
                                image_quality),
                           list("flg",
                                !is.na(flag_element),
                                flag_element),
                           list("flgInteractors",
                                !flg_interactors,
                                "false"),
                           list("sel",
                                !is.na(sel),
                                sel),
                           list("title",
                                !title,
                                "false"),
                           list("margin",
                                margin != 15,
                                as.integer(margin)),
                           list("diagramProfile",
                                diagram_profile != "Copper",
                                diagram_profile),
                           list("token",
                                !is.na(token),
                                token),
                           list("resource",
                                resource != "TOTAL",
                                resource),
                           list("expColumn",
                                !is.na(exp_column),
                                exp_column),
                           list("coverage",
                                coverage,
                                "true"))

  ## Build Function-Specific Call
  if (output_format == "svg") {
    accept_input <- "image/svg+xml"
  } else {
    accept_input <- paste0("image/", output_format)
  }

  # create file_path
  save_to <- .rba_file(file = paste0(species, ".", output_format),
                       save_to = ifelse(is.na(save_to),
                                        yes = TRUE,
                                        no = save_to))

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sexporter/fireworks/%s.%s",
                                         .rba_stg("reactome", "pth", "content"),
                                         gsub(" ", "%20",species),
                                         output_format),
                          query = call_query,
                          accpet = accept_input,
                          save_to = save_to,
                          parser = NULL)
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
#'  "GET https://reactome.org/ContentService//exporter/reaction/
#'  {identifier}.{ext}"
#'
#' @param event_id Reactome
#' \href{https://reactome.org/content/schema/ReactionLikeEvent}{Reaction-like
#' event}'s identifier.
#' @param output_format Images format, Can be one of: png (default), jpeg,
#'   svg or gif.
#' @param save_to NA or Character:\itemize{
#'   \item NA: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param image_quality Numeric: A number  ranging from 1 to 10. 1 is the
#'   lowest quality and 10 is the highest (default = 5).
#' @param flag_element Gene name, protein ID, chemical ID or Reactome ID of a
#'   diagram's element to be flagged.
#' @param flg_interactors Logical: Should the interactor be considered when
#'   flagging a diagram element? (default = TRUE)
#' @param sel CSV line for highlighting element(s) selection in the diagram.
#' @param title Logical: Should the pathway name be displayed below the image?
#'   (default = TRUE)
#' @param margin Numeric: A number ranging from 0 to 20 to set as the image's
#'   margin. (default = 15)
#' @param diagram_profile Color profile of diagrams, should be one of "Copper"
#'   (default), "Copper Plus", "Barium Lithium" or "calcium salts".
#' @param token The analysis Token for which the results will be overlaid on
#'   top of the given pathways overview. see:
#'   \code{\link{rba_reactome_analysis}}.
#' @param analysis_profile Color profile of analysis, should be one of:
#' "Standard" (default), "Strosobar" or "Copper Plus".
#' @param resource The analysis resource for which the results will be
#'   overlaid on top of the given pathways overview.
#' @param exp_column numeric: (only if token is provided) Specify the
#'   expression column for the overlay.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return NULL, Based to the inputs, an image file will be saved to disk.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
                                           save_to = NA,
                                           output_format = "png",
                                           resource = "TOTAL",
                                           diagram_profile = "Modern",
                                           analysis_profile = "Standard",
                                           token = NA,
                                           exp_column = NA,
                                           image_quality = 5,
                                           flag_element = NA,
                                           flg_interactors = TRUE,
                                           sel = NA,
                                           title = TRUE,
                                           margin = 15,
                                           ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "event_id",
                             class = "character"),
                        list(arg = "save_to",
                             class = "character"),
                        list(arg = "output_format",
                             class = "character",
                             val = c("png",
                                     "jpeg",
                                     "svg",
                                     "gif")),
                        list(arg = "image_quality",
                             class = "numeric",
                             ran = c(1,10)),
                        list(arg = "flag_element",
                             class = "character"),
                        list(arg = "flg_interactors",
                             class = "logical"),
                        list(arg = "sel",
                             class = "character"),
                        list(arg = "title",
                             class = "logical"),
                        list(arg = "margin",
                             class = "numeric",
                             ran = c(0,20)),
                        list(arg = "diagram_profile",
                             class = "character",
                             val = c("Modern",
                                     "Standard")),
                        list(arg = "token",
                             class = "character"),
                        list(arg = "resource",
                             class = "character"),
                        list(arg = "analysis_profile",
                             class = "character",
                             val = c("Standard",
                                     "Strosobar",
                                     "Copper Plus")),
                        list(arg = "exp_column",
                             class = "numeric")),
            cond = list(list(quote(!is.na(exp_column) && is.na(token)),
                             "You cannot specify expression column without providing a token.")))

  .msg("Retrieving Reaction-like event %s image in %s format.",
       event_id, output_format)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("quality",
                                image_quality != 5,
                                image_quality),
                           list("flg",
                                !is.na(flag_element),
                                flag_element),
                           list("flgInteractors",
                                !flg_interactors,
                                "false"),
                           list("sel",
                                !is.na(sel),
                                sel),
                           list("title",
                                !title,
                                "false"),
                           list("margin",
                                margin != 15,
                                as.integer(margin)),
                           list("diagramProfile",
                                diagram_profile != "Copper",
                                diagram_profile),
                           list("analysisProfile",
                                !is.na(analysis_profile),
                                analysis_profile),
                           list("token",
                                !is.na(token),
                                token),
                           list("resource",
                                resource != "TOTAL",
                                resource),
                           list("expColumn",
                                !is.na(exp_column),
                                exp_column))

  ## Build Function-Specific Call
  if (output_format == "svg") {
    accept_input <- "image/svg+xml"
  } else {
    accept_input <- paste0("image/", output_format)
  }

  # create file_path
  save_to <- .rba_file(file = paste0(event_id, ".", output_format),
                       save_to = ifelse(is.na(save_to),
                                        yes = TRUE,
                                        no = save_to))

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sexporter/reaction/%s.%s",
                                         .rba_stg("reactome", "pth", "content"),
                                         event_id,
                                         output_format),
                          query = call_query,
                          accpet = accept_input,
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
#'    {resource}/details"
#'  "POST https://reactome.org/ContentService/interactors/psicquic/molecules/
#'    {resource}/summary"
#'  "GET https://reactome.org/ContentService/interactors/psicquic/resources"
#'
#' @param proteins Proteins to retrieve PSICQUIC interactors.
#' @param resource The PSICQUIC resource for yout provided proteins. Call
#'   rba_reactome_interactors_psicquic() without argument to get the available
#'   options.
#' @param details Logical: If TRUE (default) a detailed list of interactors
#'   will be returned. If FALSE, only a summary of available interactors will
#'   be returned.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Depending your input, a list containg the detailed or summary of
#'   PSICQUIC interactions or a data frame of all regesiterd PSICQUIC
#'   resources.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
rba_reactome_interactors_psicquic <- function(proteins = NA,
                                              resource = NA,
                                              details = TRUE,
                                              ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "proteins",
                             class = c("character",
                                       "numeric"),
                             max_len = 1000),
                        list(arg = "resource",
                             class = "character"),
                        list(arg = "details",
                             class = "logical")),
            cond = list(list(quote(sum(!is.na(proteins), !is.na(resource))),
                             "You should provide 'proteins' and 'resource' togeather.")))
  if (any(!is.na(proteins))) {
    details <- ifelse(isTRUE(details), yes = "details", no = "summary")
    .msg("Retrieving %s of clustered interactions of %s ptoteins(s) from %s.",
         details,
         ifelse(length(proteins) == 1,
                yes = proteins, no = length(proteins)),
         resource)
    ## Build POST API Request's URL
    call_body <- paste(unique(proteins),collapse = "\n")
    input_call <- .rba_httr(httr = "post",
                            url = .rba_stg("reactome", "url"),
                            path = sprintf("%sinteractors/psicquic/molecules/%s/%s",
                                           .rba_stg("reactome", "pth", "content"),
                                           resource, details),
                            body = call_body,
                            accept = "application/json",
                            httr::content_type("text/plain"),
                            parser = "json->list",
                            save_to = .rba_file("reactome_interactors_psicquic.json"))
  } else {
    .msg("Retrieving a table of all Psicquic Registries services.")
    ## Build Function-Specific Call
    input_call <- .rba_httr(httr = "get",
                            url = .rba_stg("reactome", "url"),
                            path = paste0(.rba_stg("reactome", "pth", "content"),
                                          "/interactors/psicquic/resources"),
                            accept = "application/json",
                            parser = "json->df",
                            save_to = .rba_file("reactome_interactors_psicquic.json"))
  }
  ## Call API
  final_output <- .rba_skeleton(input_call)

  return(final_output)
}

#' Get Static(IntAct) Interaction Information of a Protein
#'
#' Reactome maintain a locally host a version of IntAct(Static) interactions
#'   database. Using this funtion, you can retrieve IntAct information of
#'   a prtotein(s) in two scenarios: \enumerate{
#'   \item If endpoint = "details" or "summary": Retrieve a detailed/summary
#'     information of your provided protein accession(s) from IntAct database.
#'   \item If endpoint = "pathway", Retrieve a list of Reactome pathways which
#'   include your provided protein accession. Pathways with the class
#'   "TopLevelPathway" will be excluded.}
#'
#' @section Corresponding API Resources:
#'  "POST https://reactome.org/ContentService/interactors/static/
#'  molecules/details"
#'  \cr "POST https://reactome.org/ContentService/interactors/static/
#'  molecules/summary"
#'  \cr "GET https://reactome.org/ContentService/interactors/static/
#'  molecules/pathways"
#'
#' @param proteins Uniprot proteins accession(s). If endpoint = "pathway",
#'   only a single protein accession can be provided.
#' @param endpoint Can be one of: \enumerate{
#'   \item "details": To return a detailed information of your provided
#'   protein(s) accession.
#'   \item "summary": To return a summary of your provided protein(s) accession
#'   \item "pathway": To return a list of pathways containg the interacting
#'   molecules (excluding TopLevelPathway class).}
#' @param only_diagrammed Logical: (only when "endpoint = "pathway")
#'   If TRUE, pathways without diagram will be excluded. (default = FALSE)
#' @param species Only when "endpoint = "pathway", The scientific name
#'   of the species to search for the pathways. Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List which it's content varies based on the  provided "endpoint"
#'   argument.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   }
#'
#' @examples
#' \donttest{
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
                                            species = NA,
                                            ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "proteins",
                             class = c("character",
                                       "numeric"),
                             max_len = 1000),
                        list(arg = "endpoint",
                             class = "character",
                             val = c("details",
                                     "summary",
                                     "pathways")),
                        list(arg = "only_diagrammed",
                             class = "logical"),
                        list(arg = "species",
                             class = "character")),
            cond = list(list(quote(endpoint == "pathways" && length(proteins) != 1),
                             "When 'endpoint = pathways', you can only provide one protein."),
                        list(quote(sum(!is.na(species), endpoint == "pathways") == 1),
                             "You should -and can only- provide species when endpoint is 'pathways'.")))



  if (endpoint == "pathways") {
    .msg("Retrieving pathways with the Static(IntAct) Interactors of protein %s.",
         proteins)
    call_query <- .rba_query(init = list("onlyDiagrammed" = ifelse(isTRUE(only_diagrammed),
                                                                   yes = "true",
                                                                   no = "false")),
                             list("species",
                                  !is.na(species),
                                  species))
    input_call <- .rba_httr(httr = "get",
                            url = .rba_stg("reactome", "url"),
                            path = sprintf("%sinteractors/static/molecule/%s/pathways",
                                           .rba_stg("reactome", "pth", "content"),
                                           proteins),
                            query = call_query,
                            accept = "application/json",
                            parser = "json->df",
                            save_to = .rba_file("reactome_interactors_static.json"))
  } else {
    ## Build POST API Request's URL
    .msg("Retrieving %s of Static(IntAct) Interactors of protein %s.",
         endpoint, proteins)
    call_body <- paste(unique(proteins),collapse = "\n")
    ## Build Function-Specific Call
    parser_input <- ifelse(endpoint == "details",
                           yes = "json->list", no = "json->list_simp")
    input_call <- .rba_httr(httr = "post",
                            url = .rba_stg("reactome", "url"),
                            path = paste0(.rba_stg("reactome", "pth", "content"),
                                          "interactors/static/molecules/",
                                          endpoint),
                            body = call_body,
                            accept = "application/json",
                            httr::content_type("text/plain"),
                            parser = parser_input,
                            save_to = .rba_file("reactome_interactors_static.json"))
  }

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Mapping Endpoints ####

#' Map External ID to Reactome Pathways/Reactions
#'
#' By providing an external identifier from a given resource, you can retrieve
#'   a list of pathways/reactions that include your provided ID.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/mapping/{resource}/
#'  {identifier}/pathways"
#'  \cr "GET https://reactome.org/ContentService/data/mapping/{resource}/
#'  {identifier}/reactions"
#'
#' @param id Molecule's external Identifier
#' @param resource What is the resource of your provided ID? see:
#' \href{https://reactome.org/content/schema/objects/ReferenceDatabase}{Reactome External
#' Identifiers}
#' @param species Numeric or Character: NCBI Taxonomy identifier (Human
#'   is 9606), species name (e.g. "Homo sapiens") or Reactome DbId (e.g
#'   Homo sapiens is 48887). Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param map_to Either "pathways" or "reactions".
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame where each row is a pathway/reaction and columns are
#'   pertinent information.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "id",
                             class = c("character",
                                       "numeric")),
                        list(arg = "resource",
                             class = "character"),
                        list(arg = "species",
                             class = c("character",
                                       "numeric")),
                        list(arg = "map_to",
                             class = "character",
                             val = c("pathways",
                                     "reactions"))))

  .msg("Retrieving Reactome %s that contain %s from %s resource.",
       map_to, id, resource)
  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("species",
                                !is.na(species),
                                species))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sdata/mapping/%s/%s/%s",
                                         .rba_stg("reactome", "pth", "content"),
                                         resource,
                                         id,
                                         map_to),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("reactome_mapping.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Orthology Endpoints ####

#' Get Orthologous (Computationally Inferred) Events
#'
#' Reactome incorporate manually curated human reactions and PANTHER's
#'   protein homology data to Computationally infer events in other euakaryotic
#'   species.
#'
#' In version 73 (11 June 2020), using an orthology-based approach,
#'   Homo sapiens events was projected to 18,654 orthologous pathways (with
#'   81,835 orthologous proteins) in 15 non-human species.
#'   \cr Refer to \href{https://reactome.org/documentation/inferred-events}{
#'   Reactome Computationally Inferred Events} for more information.
#'
#' @section Corresponding API Resources:
#'  "POST https://reactome.org/ContentService/data/orthologies/ids/
#'    species/{speciesId}"
#'
#' @param event_ids Human Reactome event ID(s) to retrieve their orthologous
#'   events.
#' @param species_dbid Reactome database ID (DbId) of the target species. (e.g
#'   Mus musculus is 48892). Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List containing found Orthologous event(s) in your provided species
#'   and their pertinent information.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "event_ids",
                             class = "character"),
                        list(arg = "species_dbid",
                             class = "numeric")))

  .msg("Retrieving orthologous Events of '%s' in the specie with DbId '%s'.",
       ifelse(length(event_ids) == 1,
              yes = event_ids, no = paste0(length(event_ids), " input events")),
       species_dbid)

  ## Build POST API Request's URL
  call_body <- paste(unique(event_ids),collapse = "\n")

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("reactome", "url"),
                          path = paste0(.rba_stg("reactome", "pth", "content"),
                                        "data/orthologies/ids/species/",
                                        species_dbid),
                          body = call_body,
                          accept = "application/json",
                          httr::content_type("text/plain"),
                          parser = "json->list_simp",
                          save_to = .rba_file("reactome_orthology.json"))

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
#'  both of the above-mentioned 'Physical Entities' refer to a 'Reference Entities'
#'  named \href{https://reactome.org/content/schema/instance/browser/57819}{
#'  "UniProt:P01834 IGKC}.
#'  \cr See \href{https://reactome.org/documentation/data-model}{Reactome
#'  Data Model} for more information about the data model and Physical
#'  Entities.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/participants/{id}"
#'  \cr "GET https://reactome.org/ContentService/data/participants/{id}/
#'  participatingPhysicalEntities"
#'  \cr "GET https://reactome.org/ContentService/data/participants/{id}/
#'  referenceEntities"
#'
#' @param event_id Reactome event's database ID (DbId) or Stable ID (StId).
#' @param only_physical_entities Logical: If TRUe, only participating
#'   'Physical Entities' will be returned.
#' @param only_reference_entities Logical: If TRUe, only participating
#'   'Reference Entities' will be returned.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List with the participant of your provided Event ID. A Data frame
#'  if only physical or 'Reference Entities' was requested.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "event_id",
                             class = c("character",
                                       "numeric")),
                        list(arg = "only_physical_entities",
                             class = "logical"),
                        list(arg = "only_reference_entities",
                             class = "logical")),
            cond = list(list(quote(sum(only_physical_entities, only_reference_entities) == 2),
                             "You can only set either only_reference_entities or only_reference_entities to TRUE in one function call.")))

  .msg("Retrieving %sParticipants of Reactome event %s.",
       ifelse(sum(only_physical_entities, only_reference_entities) == 0,
              yes = "",
              no = c("'Physical Entities' ",
                     "'Reference Entities' ")[c(only_physical_entities,
                                                only_reference_entities)]),
       event_id)

  ## Build Function-Specific Call
  path_input <- paste0(.rba_stg("reactome", "pth", "content"),
                       "data/participants/",
                       event_id)
  parser_input <- "json->list"
  if (isTRUE(only_physical_entities)) {
    path_input <- paste0(path_input, "/participatingPhysicalEntities")
    parser_input <- "json->df"
  } else if (isTRUE(only_reference_entities)) {
    path_input <- paste0(path_input, "/referenceEntities")
    parser_input <- "json->df"
  }

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("reactome_participants.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Pathways Endpoints ####

#' Get Events Contained in an Upstream Events
#'
#' A Reactome Event could be comprised of other events (meaning, a pathway that
#'   include other pathways itself). Use this function to recursively return
#'   all the events which reside downstream of your provided event ID (or
#'   an attribute of that events).
#'
#' By Reactome's definition, Events are the building blocks of biological
#'   processes and could be of two main classes: "Pathway" or
#'   "Reaction-like events". The events are organized in a hierarchical
#'   structure; and each event could be child or parent to another event; The
#'   hierarchy will always begin with a "Top level pathway" event. Also note
#'   that a given event could be part of more that one hierarchies.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/pathway/{id}/
#'  containedEvents"
#'  \cr "GET https://reactome.org/ContentService/data/pathway/{id}/
#'  containedEvents/{attributeName}"
#'
#' @param event_id Reactome event's database ID (DbId) or Stable ID (StId).
#' @param attribute_name An attribute of the events to be returned instead of
#'   the whole events. see \href{https://reactome.org/content/schema/Event}{
#'   Reactome Data Schema: Event} for available options.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame where each row is a contained event and columns are
#'   event's attributes. If an "attribute_name" argument was provided, a
#'   character vector will be returned.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_pathways_events(event_id = "R-HSA-5673001")
#' }
#' \donttest{
#' rba_reactome_pathways_events(event_id = "R-HSA-5673001",
#'     attribute_name = "displayName")
#' }
#'
#' @family "Reactome Content Service - Pathway Related Queries"
#' @export
rba_reactome_pathways_events <- function(event_id,
                                         attribute_name = NA,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "event_id",
                             class = c("numeric",
                                       "character")),
                        list(arg = "attribute_name",
                             class = "character")))

  .msg("Retrieving %s contained events under the event %s.",
       ifelse(is.na(attribute_name),
              yes = "all",
              no = sprintf("attribute '%s' of all",
                           attribute_name)),
       event_id)

  ## Build Function-Specific Call
  path_input <- sprintf("%sdata/pathway/%s/containedEvents",
                        .rba_stg("reactome", "pth", "content"),
                        event_id)
  accept_input <- "application/json"
  parser_input <- "json->df"
  file_ext <- "json"

  if (!is.na(attribute_name)) {
    path_input <- paste0(path_input, "/", attribute_name)
    accept_input <- "text/plain"
    parser_input <- function(x) {
      unlist(strsplit(x = gsub(pattern = "\\[|\\]",
                               replacement = "",
                               x = httr::content(x,
                                                 as = "text",
                                                 encoding = "UTF-8")),
                      split = ", "))
    }
    file_ext <- "txt"
  }
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          accpet = accept_input,
                          parser = parser_input,
                          save_to = .rba_file(paste0("reactome_pathways_participants",
                                                     ".", file_ext)))


  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get lower level pathways Containing a 'Physical Entity' or Event
#'
#' Use this function to search the event hierarcht and retireve a list of
#'   all lower level pathways (non TopLevelPathway class) that contain
#'   a given 'Physical Entity' or Event. See "Arguments section" on how to
#'   modify your search.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/pathways/low/entity/{id}"
#'  \cr "GET https://reactome.org/ContentService/data/pathways/low/diagram/
#'  entity/{id}"
#'  \cr "GET https://reactome.org/ContentService/data/pathways/low/diagram/
#'  entity/{id}/allForms"
#'
#' @param entity_id The entity that should exist in the pathways.
#' @param with_diagram Logical: only include pathways with diagram?
#' @param all_forms Logical: should other variants of your provided entity_id
#'   be considered? (e.g. same molecule but in different compartment,
#'   secretory form etc.) refer to \code{\link{rba_reactome_participants}}'s
#'   "Details section" to learn more about how Reactome classifies molecules.
#' @param species (optional) Numeric or Character: confine your search to a
#'   specific species by providing it's NCBI Taxonomy identifier
#'   (Human Taxonomy ID is 9606) or species name (e.g. "Homo sapiens").
#'   Refer to \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame where each row is a pathway that contains your provided
#'   entity and columns are pertinent information.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
                                      species = NA,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "entity_id",
                             class = "character"),
                        list(arg = "all_forms",
                             class = "logical"),
                        list(arg = "with_diagram",
                             class = "logical"),
                        list(arg = "species",
                             class = c("character",
                                       "numeric"))))

  .msg("Retrieving lower-level pathways that include %sentity %s%s.",
       ifelse(isTRUE(all_forms),
              yes = "any form of ", no = ""),
       entity_id,
       ifelse(isTRUE(with_diagram),
              yes = " and have diagram", no = ""))
  ## Build Function-Specific Call
  path_input <- sprintf("%sdata/pathways/%s/%s",
                        .rba_stg("reactome", "pth", "content"),
                        ifelse(isTRUE(with_diagram),
                               yes = "low/diagram/entity",
                               no = "low/entity"),
                        entity_id)
  if (isTRUE(all_forms)) {
    path_input <- paste0(path_input,
                         "/allForms")
  }
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("reactome_pathways_low.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Top Level Pathways in a Species
#'
#' This function will Return a list of all pathways with the class
#'   "TopLevelPathway" which are annotated in your provided species.
#'
#' Reactome's Events hierarchy for any specie will begin with pathways with
#'   class "TopLevelPathway" (e.g. "Immune System", "Metabolism of proteins").
#'   further down in the event's hierarchy tree, each TopLevelPathway has
#'   has other events itself (e.g. "Adaptive immune system", "Innate immune
#'   system"). Based on the chosen pathway, the hierarchy tree would typically
#'   goes further down.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/pathways/top/{species}"
#'
#' @param species Numeric or Character: NCBI Taxonomy identifier (Human Taxonomy
#'    ID is 9606.) or species name (e.g. "Homo sapiens"). Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame where each row is a Top Level Pathway and columns are
#'   pertinent information.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "species",
                             class = c("character",
                                       "numeric"))))

  .msg("GET /data/pathways/top/{species}",
       "All Reactome top level pathways")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("species",
                                !is.na(species),
                                species))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = paste0(.rba_stg("reactome", "pth", "content"),
                                        "data/pathways/top/",
                                        species),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("reactome_pathways_top.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Person Endpoints ####

#' Get Persons Information by Name
#'
#' Using this function you can query peaple by partially matching or exact
#'   name and retrieve a list of matching people in Reactome.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/data/people/name/{name}"
#'  \cr "GET https://reactome.org/ContentService/data/people/name/{name}/exact"
#'
#' @param person_name first and last name of the person
#' @param exact_match Logical: should the provided name be considered as
#'   an exact match? (default = FALSE)
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List where each element is a search hit contains the person's
#'   information.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "person_name",
                             class = "character"),
                        list(arg = "exact_match",
                             class = "logical")))

  .msg("/data/people/name/{name}",
       "A list of people with first or last name partly matching a given string",
       "/data/people/name/{name}/exact_match",
       "A list of people with first AND last name exactly matching a given string")

  ## Build Function-Specific Call
  path_input <- paste0(.rba_stg("reactome", "pth", "content"),
                       "data/people/name/",
                       gsub(" ", "%20", person_name))
  if (isTRUE(exact_match)) {
    path_input <- paste0(path_input, "/exact")
  }
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("reactome_people_name.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' A person by his identifiers
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService"
#'
#' @param person_id Reactome database ID (DbId) or ORCHID ID
#' @param authored_pathways Logical: Only return Pathway list authored by the
#'   person? (default = FALSE)
#' @param publications Logical: Only return publications list authored by the
#'   person? (Defalt = FALSE)
#' @param attribute_name (optional) A Reactome person attribute to retunt only.
#'   see \href{https://reactome.org/content/schema/Person}{Reactome Data
#'   Schema: person} for available options.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List containging the requested informations of your provided
#'   person.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
                                   attribute_name = NA,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "person_id",
                             class = "character"),
                        list(arg = "authored_pathways",
                             class = "logical"),
                        list(arg = "publications",
                             class = "logical"),
                        list(arg = "attribute_name",
                             class = "character")),
            cond = list(list(quote(sum(!is.na(attribute_name),
                                       isTRUE(authored_pathways),
                                       isTRUE(publications)) > 1),
                             "You can only use either attribute_name, authored_pathways or publications function call.")))

  .msg("Retrieving information of person with id: %s",
       person_id)

  ## Build Function-Specific Call
  path_input <- paste0(.rba_stg("reactome", "pth", "content"),
                       "data/person/",
                       person_id)
  accept_input <- "application/json"
  parser_type_input <- "json->list"
  file_ext <- "json"
  if (isTRUE(authored_pathways)) {
    path_input <- paste0(path_input, "/authoredPathways")
  } else if (isTRUE(publications)) {
    path_input <- paste0(path_input, "/publications")
  } else if (!is.na(attribute_name)) {
    path_input <- paste0(path_input, "/", attribute_name)
    accept_input <- "text/plain"
    parser_type_input <- "text->chr"
    file_ext <- "txt"
  }
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          accpet = accept_input,
                          parser = parser_type_input,
                          save_to = .rba_file(paste0("reactome_people_id",
                                                     ".",
                                                     file_ext)))
  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Query Endpoints ####

#' Query and Retrieve any Reactome knowledge-base Object
#'
#' Using this Comprehensive function, You can Retrieve any object from
#'   \href{https://reactome.org/content/schema/DatabaseObject}{Reactome
#'   knowledge-base}
#'
#' @section Corresponding API Resources:
#'  "POST https://reactome.org/ContentService/data/query/ids"
#'  \cr "POST https://reactome.org/ContentService/data/query/ids/map"
#'  \cr "GET https://reactome.org/ContentService/data/query/{id}"
#'  \cr s"GET https://reactome.org/ContentService//data/query/enhanced/{id}"
#'  \cr "GET https://reactome.org/ContentService/data/query/{id}/{attributeName}"
#'
#' @param ids A single or Multiple database IDs (DbId), Stable IDs (StId) or
#'   a mixture of both.
#' @param enhanced Logical: (Default = FALSE) If 'TRUE' more information on
#'   the provided entry will be returned. (You can set this argument to 'TRUE'
#'   Only when you provide a single ID).
#' @param map (Default = FALSE) Should the provided IDs be mapped? This
#'   argument will only be considered when you provide multiple IDs.
#' (e.g. when you provide previous version of stable identifiers.)
#' @param attribute_name (Optional) Only Return an Attribute of the provided
#'   Database Object. (You can use this argument Only when you
#'   provide a single ID)
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List containing your query outputs.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
                               attribute_name = NA,
                               ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "ids",
                             class = c("character",
                                       "numeric"),
                             max_len = 20),
                        list(arg = "enhanced",
                             class = "logical"),
                        list(arg = "map",
                             class = "logical"),
                        list(arg = "attribute_name",
                             class = "character")),
            cond = list(list(quote(length(ids) > 1 &&
                                     (isTRUE(enhanced) | !is.na(attribute_name))),
                             "You can only use 'enhnaced' or 'attribute_name' with a single ID not multiple IDs."),
                        list(quote(!is.na(attribute_name) && isTRUE(enhanced)),
                             "You can only provide 'attribute_name' when enhanced is 'FALSE'.")))

  if (length(ids) > 1) {
    #### use POST
    .msg("POST /data/query/ids/map",
         "A list of entries with their mapping to the provided identifiers")
    ## Build POST API Request's URL
    call_body <- paste(unique(ids),collapse = ",")
    path_input <- paste0(.rba_stg("reactome", "pth", "content"),
                         ifelse(isTRUE(map),
                                yes = "data/query/ids/map",
                                no = "data/query/ids"))
    ## Build Function-Specific Call
    input_call <- .rba_httr(httr = "post",
                            url = .rba_stg("reactome", "url"),
                            path = path_input,
                            body = call_body,
                            parser = "json->list",
                            accept = "application/json",
                            httr::content_type("text/plain"),
                            save_to = .rba_file("reactome_query.json"))
  } else {
    #### use GET
    ## Build Function-Specific Call
    path_input <- paste0(.rba_stg("reactome", "pth", "content"),
                         "data/query/",
                         ids)
    accept_input <- "application/json"
    parser_input <- "json->list"
    file_ext <- "json"
    if (!is.na(attribute_name)) {
      .msg("GET /data/query/{id}/{attributeName}",
           "A single property of an entry in Reactome knowledgebase")
      path_input <- paste0(path_input, "/", attribute_name)
      accept_input <- "text/plain"
      parser_input <- "text->chr"
      file_ext <- "txt"
    } else if (isTRUE(enhanced)) {
      .msg("GET /data/query/enhanced/{id}",
           "More information on an entry in Reactome knowledgebase")
      path_input <- sub("/query/", "/query/enhanced/", path_input)
    } else {
      .msg("GET /data/query/{id}",
           "An entry in Reactome knowledgebase")
    }
    input_call <- .rba_httr(httr = "get",
                            url = .rba_stg("reactome", "url"),
                            path = path_input,
                            parser = parser_input,
                            accept = accept_input,
                            save_to = .rba_file(paste0("reactome_query.",
                                                       file_ext)))

  }

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### References Endpoints ####

#' Map Cross References IDs to Reactome ReferenceEntity
#'
#' Use this function To retrieve a list of Reactome ReferenceEntity associated
#'   to your provided Cross Reference (i.e. External) ID.
#'
#' Reactome cross-references external database's identifiers to it's database
#'   Entries named ReferenceEntity, which resembles the invariant aspect of
#'   a molecule. Thus there is a one-to-many relationship between Reactome's
#'   ReferenceEntity object and the molecule's ID in external databases,
#'   which in Reactome's terms is called Cross Reference.
#'   \cr Refer to \code{\link{rba_reactome_participants}}'s "Details section"
#'   to learn more about how Reactome classifies molecules.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/ContentService/references/mapping/{identifier}"
#'
#' @param xref_id molecule's cross-reference (external) identifier.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List containing the ReferenceEntity corresponding to your
#'   provided cross-reference (external) ID.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "xref_id",
                             class = c("character",
                                       "numeric"))))

  .msg("Retrieving Reactome's ReferenceEntity that have a cross-reference to %s.",
       xref_id)

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = paste0(.rba_stg("reactome", "pth", "content"),
                                        "references/mapping/",
                                        xref_id),
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("reactome_xref.json"))

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
#' @param only_main Logical: If set to TRUE, will only return species which
#'   have either manually-curated or computationally inferred pathways.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Data frame where each row is a species and columns are pertinent
#'   information.
#'
#' @references \itemize{
#'   \item Jassal B, Matthews L, Viteri G, Gong C, Lorente P, Fabregat A,
#'   Sidiropoulos K, Cook J, Gillespie M, Haw R, Loney F, May B, Milacic M,
#'   Rothfels K, Sevilla C, Shamovsky V, Shorser S, Varusai T, Weiser J, Wu G,
#'   Stein L, Hermjakob H, D'Eustachio P. The reactome pathway knowledgebase.
#'   Nucleic Acids Res. 2020 Jan 8;48(D1):D498-D503. doi: 10.1093/nar/gkz1031.
#'   PubMed PMID: 31691815.
#'   \item \href{https://reactome.org/ContentService/}{Reactome Content
#'   Services API Documentation}
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
  .rba_args(cons = list(list(arg = "only_main",
                             class = "logical")))

  .msg("Retrieving %sspecies available in Reactome.",
       ifelse(isTRUE(only_main),
              yes = "main (i.e. with pathways) ",
              no = ""))

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = paste0(.rba_stg("reactome", "pth", "content"),
                                        "data/species/",
                                        ifelse(isTRUE(only_main),
                                               yes = "main", no = "all")),
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("reactome_species.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
