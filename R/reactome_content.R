#### database Endpoints ####
#' The version number of current database
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_version = function(...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args()

  v_msg("GET /data/database/version")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = paste0(rba_ba_stg("reactome", "pth", "content"),
                                         "data/database/version"),
                           accpet = "text/plain",
                           parser = "text->chr",
                           save_to = rba_ba_file("reactome_diseases.txt"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### diseases Endpoints ####

#' It retrieves the list of diseases annotated in Reactome OR
#' list of disease DOIDs annotated in Reactome
#'
#' @param doid
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_diseases = function(doid = FALSE,
                                 ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "doid",
                               class = "logical")))

  v_msg("GET data/diseases", "\r\n", "/data/diseases/doid")

  ## Build Function-Specific Call
  if (doid == FALSE) {
    path_input = paste0(rba_ba_stg("reactome", "pth", "content"),
                        "data/diseases")
    accept_input = "application/json"
    parser_input = "json->df"
    file_ext = "json"
  } else {
    path_input = paste0(rba_ba_stg("reactome", "pth", "content"),
                        "data/diseases/doid")
    accept_input = "text/plain"
    parser_input = "text->df"
    file_ext = "txt"
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           accpet = accept_input,
                           parser = parser_input,
                           save_to = rba_ba_file(paste0("reactome_diseases.",
                                                        file_ext)))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Entities Endpoints ####

#' Retrieves the list of subunits that constitute any given complex. In case
#' the complex comprises other complexes, this method recursively traverses
#' the content returning each contained PhysicalEntity. Contained complexes
#' and entity sets can be excluded setting the ‘excludeStructures’ optional
#' parameter to ‘true’
#'
#' @param complex_id
#' @param ...
#' @param exclude_structures
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_complex_subunits = function(complex_id,
                                         exclude_structures = FALSE,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "complex_id",
                               class = "character"),
                          list(arg = "exclude_structures",
                               class = "logical")))

  v_msg("GET /data/complex/{id}/subunits")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("excludeStructures",
                                 exclude_structures == TRUE,
                                 "true"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sdata/complex/%s/subunits",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          complex_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_complex_subunits.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Retrieves the list of complexes that contain a given (identifier, resource).
#' The method deconstructs the complexes into all its participants to do so.
#'
#' @param id
#' @param ...
#' @param resource
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_complex_list = function(id,
                                     resource,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = "character"),
                          list(arg = "resource",
                               class = "character")))

  v_msg("GET /data/complexes/{resource}/{identifier}")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sdata/complexes/%s/%s",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          resource, id),
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("reactome_complex_list.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Retrieves the list of structures (Complexes and Sets) that include the
#' given entity as their component. It should be mentioned that the list
#' includes only simplified entries (type, names, ids) and not full
#' information about each item.
#'
#' @param ...
#' @param entity_id
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_entity_component_of = function(entity_id,
                                            ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "entity_id",
                               class = "character")))

  v_msg("GET/data/entity/{id}/componentOf",
        "A list of larger structures containing the entity")
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sdata/entity/%s/componentOf",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          entity_id),
                           accept = "application/json",
                           save_to = rba_ba_file("reactome_entity_component_of.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Retrieves a list containing all other forms of the given PhysicalEntity.
#' These other forms are PhysicalEntities that share the same ReferenceEntity
#' identifier, e.g. PTEN H93R R-HSA-2318524 and PTEN C124R R-HSA-2317439
#' are two forms of PTEN.
#'
#' @param ...
#' @param entity_id
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_entity_other_forms = function(entity_id,
                                           ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "entity_id",
                               class = "character")))

  v_msg("GET data/entity/{id}/otherForms",
        "All other forms of a PhysicalEntity")
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sdata/entity/%s/otherForms",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          entity_id),
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_entity_other_forms.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Events Endpoints ####

#' The Reactome definition of events includes pathways and reactions. Although
#' events are organised in a hierarchical structure, a single event can be in
#' more than one location, i.e. a reaction can take part in different pathways
#' while, in the same way, a sub-pathway can take part in many pathways.
#' Therefore, this method retrieves a list of all possible paths from the
#' requested event to the top level pathway(s).
#'
#' @param ...
#' @param event_id
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_event_ancestors = function(event_id,
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "event_id",
                               class = "character")))

  v_msg("GET /data/event/{id}/ancestors",
        "The ancestors of a given event")
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sdata/event/%s/ancestors",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          event_id),
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_event_ancestors.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Events (pathways and reactions) in Reactome are organised in a hierarchical
#' structure for every species. By following all ‘hasEvent’ relationships,
#' this method retrieves the full event hierarchy for any given species.
#' The result is a list of tree structures, one for each TopLevelPathway.
#' Every event in these trees is represented by a PathwayBrowserNode.
#' The latter contains the stable identifier, the name, the species, the url,
#' the type, and the diagram of the particular event.
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_event_hierarchy = function(species,
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "event_id",
                               class = c("character",
                                         "numeric"))))

  v_msg("/data/eventsHierarchy/{species}",
        "The full event hierarchy for a given species")
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sdata/eventsHierarchy/%s",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          species),
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("reactome_event_hierarchy.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Exporter Endpoints ####

#' This method accepts identifiers for Event class instances.
#' When a diagrammed pathway is provided, the diagram is exported to the
#' specified format. When a subpathway is provided, the diagram for
#' the parent is exported and the events that are part of the subpathways
#' are selected. When a reaction is provided, the diagram containing the
#' reaction is exported and the reaction is selected.
#'
#' @param event_id
#' @param output_format
#' @param save_to
#' @param quality
#' @param flg
#' @param flg_interactors
#' @param sel
#' @param title
#' @param margin
#' @param ehld
#' @param diagram_profile
#' @param token
#' @param resource
#' @param analysis_profile
#' @param ...
#' @param exp_column
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_exporter_diagram = function(event_id,
                                         output_format = "png",
                                         save_to = NA,
                                         quality = 5,
                                         flg = NA,
                                         flg_interactors = TRUE,
                                         sel = NA,
                                         title = TRUE,
                                         margin = 15,
                                         ehld = TRUE,
                                         diagram_profile = "Modern",
                                         token = NA,
                                         resource = "TOTAL",
                                         analysis_profile = "Standard",
                                         exp_column = NA,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "event_id",
                               class = "character"),
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
                               class = "character")))

  v_msg("/exporter/diagram/{identifier}.{ext}",
        "Exports a given pathway diagram to the specified image format")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("quality",
                                 quality != 5,
                                 quality),
                            list("flg",
                                 !is.na(flg),
                                 flg),
                            list("flgInteractors",
                                 flg_interactors == FALSE,
                                 "false"),
                            list("sel",
                                 !is.na(sel),
                                 sel),
                            list("title",
                                 title == FALSE,
                                 "false"),
                            list("margin",
                                 margin != 15,
                                 as.integer(margin)),
                            list("ehld",
                                 ehld == FALSE,
                                 ehld),
                            list("diagramProfile",
                                 diagram_profile != "Modern",
                                 diagram_profile),
                            list("token",
                                 !is.na(token),
                                 token),
                            list("resource",
                                 resource != "TOTAL",
                                 resource),
                            list("analysisProfile",
                                 !is.na(analysis_profile),
                                 analysis_profile),
                            list("expColumn",
                                 !is.na(exp_column),
                                 exp_column))


  ## Build Function-Specific Call
  if (output_format == "svg") {
    accept_input = "image/svg+xml"
  } else {
    accept_input = paste0("image/", output_format)
  }
  # create file_path
  save_to = rba_ba_file(file = paste0(event_id, ".", output_format),
                        save_to = ifelse(is.na(save_to),
                                         yes = TRUE,
                                         no = save_to))
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sexporter/diagram/%s.%s",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          event_id, output_format),
                           query = call_query,
                           accpet = accept_input,
                           save_to = save_to,
                           parser = NULL)
  ## Call API
  invisible(rba_ba_skeleton(input_call))
}

#' This method accepts identifiers for Event class instances.
#' The generated document contains the details for the given event and,
#' optionally, its children (see level parameter). These details include:
#' - A diagram image
#' - Summation
#' - Literature references
#' - Edit history
#' - Other details: type, location, compartments, diseases
#'
#' @param event_id
#' @param save_to
#' @param level
#' @param diagram_profile
#' @param token
#' @param resource
#' @param analysis_profile
#' @param ...
#' @param exp_column
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_exporter_document = function(event_id,
                                          save_to = NA,
                                          level  = 1,
                                          diagram_profile = "Modern",
                                          token = NA,
                                          resource = "TOTAL",
                                          analysis_profile = "Standard",
                                          exp_column = NA,
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "event_id",
                               class = "character"),
                          list(arg = "save_to",
                               class = "character"),
                          list(arg = "level",
                               class = "numeric",
                               val = c(0,1)),
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
                               class = "character")))

  v_msg("/exporter/document/event/{identifier}.pdf",
        "Exports the content of a given event (pathway or reaction) to a PDF document")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("level",
                                 level != 1,
                                 level),
                            list("diagramProfile",
                                 diagram_profile != "Modern",
                                 diagram_profile),
                            list("token",
                                 !is.na(token),
                                 token),
                            list("resource",
                                 resource != "TOTAL",
                                 resource),
                            list("analysisProfile",
                                 !is.na(analysis_profile),
                                 analysis_profile),
                            list("expColumn",
                                 !is.na(exp_column),
                                 exp_column))

  ## Build Function-Specific Call
  # create file_path
  save_to = rba_ba_file(file = paste0(event_id, ".pdf"),
                        save_to = ifelse(is.na(save_to),
                                         yes = TRUE,
                                         no = save_to))
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sexporter/document/event/%s.pdf",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          event_id),
                           query = call_query,
                           accpet = "application/pdf",
                           save_to = save_to,
                           parser = NULL)
  ## Call API
  invisible(rba_ba_skeleton(input_call))
}

#' Exports a given pathway or reaction to SBGN or SBML
#'
#' @param event_id
#' @param output_format
#' @param ...
#' @param save_to
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_exporter_event = function(event_id,
                                       output_format,
                                       save_to = NA,
                                       ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "event_id",
                               class = "character"),
                          list(arg = "output_format",
                               class = "character",
                               val = c("sbgn",
                                       "sbml")),
                          list(arg = "save_to",
                               class = "character")))

  v_msg("/exporter/event/{identifier}.sbgn",
        "Exports a given pathway or reaction to SBGN or SBML")
  ## Build Function-Specific Call
  # create file_path
  save_to = rba_ba_file(file = paste0(event_id, ".", output_format),
                        save_to = ifelse(is.na(save_to),
                                         yes = TRUE,
                                         no = save_to))
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sexporter/event/%s.%s",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          event_id,
                                          output_format),
                           save_to = save_to,
                           parser = NULL)

  ## Call API
  invisible(rba_ba_skeleton(input_call))
}

#' Exports a given pathway overview of a specie to the specified image format
#'
#' @param species
#' @param output_format
#' @param save_to
#' @param quality
#' @param flg
#' @param flg_interactors
#' @param sel
#' @param title
#' @param margin
#' @param diagram_profile
#' @param token
#' @param resource
#' @param exp_column
#' @param ...
#' @param coverage
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_exporter_overview = function(species,
                                          output_format = "png",
                                          save_to = NA,
                                          quality = 5,
                                          flg = NA,
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
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
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
                               class = "logical")))

  v_msg("/exporter/fireworks/{species}.{ext}",
        "Exports a given pathway overview to the specified image format")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("quality",
                                 quality != 5,
                                 quality),
                            list("flg",
                                 !is.na(flg),
                                 flg),
                            list("flgInteractors",
                                 flg_interactors == FALSE,
                                 "false"),
                            list("sel",
                                 !is.na(sel),
                                 sel),
                            list("title",
                                 title == FALSE,
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
                                 coverage == TRUE,
                                 "true"))

  ## Build Function-Specific Call
  if (output_format == "svg") {
    accept_input = "image/svg+xml"
  } else {
    accept_input = paste0("image/", output_format)
  }

  # create file_path
  save_to = rba_ba_file(file = paste0(species, ".", output_format),
                        save_to = ifelse(is.na(save_to),
                                         yes = TRUE,
                                         no = save_to))

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sexporter/fireworks/%s.%s",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          gsub(" ", "%20",species),
                                          output_format),
                           query = call_query,
                           accpet = accept_input,
                           save_to = save_to,
                           parser = NULL)
  ## Call API
  invisible(rba_ba_skeleton(input_call))
}

#### Interactors Endpoints ####

#' Retrieve clustered interaction, sorted by score, of a given accession(s)
#' by resource.
#'
#' @param resource
#' @param details
#' @param ...
#' @param proteins
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_interactors_psicquic = function(resource,
                                             proteins,
                                             details = "details",
                                             ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "resource",
                               class = "character"),
                          list(arg = "proteins",
                               class = c("character",
                                         "numeric"),
                               max_len = 1000),
                          list(arg = "details",
                               class = "character",
                               val = c("details",
                                       "summary"))))

  v_msg("POST /interactors/psicquic/molecules/{resource}/details",
        "Retrieve clustered interaction, sorted by score, of a given accession(s) by resource.",
        "POST/interactors/psicquic/molecules/{resource}/summary",
        "Retrieve a summary of a given accession list by resource.")

  ## Build POST API Request's URL
  call_body = paste(unique(proteins),collapse = "\n")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sinteractors/psicquic/molecules/%s/%s",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          resource, details),
                           body = call_body,
                           accept = "application/json",
                           httr::content_type("text/plain"),
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("reactome_interactors_psicquic.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Retrieve a list of all Psicquic Registries services
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_interactors_resources = function(...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args()

  v_msg("GET /interactors/psicquic/resources",
        "Retrieve a list of all Psicquic Registries services")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = paste0(rba_ba_stg("reactome", "pth", "content"),
                                         "/interactors/psicquic/resources"),
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_interactors_resources.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Retrieve a interaction information of a given accession
#'
#' @param proteins
#' @param details
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_interactors_static = function(proteins,
                                           details = "details",
                                           ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "proteins",
                               class = c("character",
                                         "numeric"),
                               max_len = 1000),
                          list(arg = "details",
                               class = "character",
                               val = c("details",
                                       "summary"))))

  v_msg("POST /interactors/static/molecules/details",
        "Retrieve a detailed interaction information of a given accession",
        "POST/interactors/static/molecules/summary",
        "Retrieve a summary of a given accession list")

  ## Build POST API Request's URL
  call_body = paste(unique(proteins),collapse = "\n")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("reactome", "url"),
                           path = paste0(rba_ba_stg("reactome", "pth", "content"),
                                         "interactors/static/molecules/",
                                         details),
                           body = call_body,
                           accept = "application/json",
                           httr::content_type("text/plain"),
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("reactome_interactors_static.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Mapping Endpoints ####

#' Entities play different roles in reactions, and reactions are events that
#' conform a pathway. This method retrieves the pathways for which an
#' identifier plays a role within one or more of their events.
#'
#' @param id
#' @param resource
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_mapping_pathways = function(id,
                                         resource,
                                         species = NA,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = c("character",
                                         "numeric")),
                          list(arg = "resource",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET /data/mapping/{resource}/{identifier}/pathways",
        "The lower level pathways where an identifier can be mapped to")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("species",
                                 !is.na(species),
                                 species))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sdata/mapping/%s/%s/pathways",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          resource,
                                          id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_mapping_pathways.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Entities play different roles in reactions. This method retrieves the
#' reactions for which an identifier plays a role .
#'
#' @param id
#' @param resource
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_mapping_reactions = function(id,
                                          resource,
                                          species = NA,
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = c("character",
                                         "numeric")),
                          list(arg = "resource",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET /data/mapping/{resource}/{identifier}/reactions",
        "The reactions where an identifier can be mapped to")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("species",
                                 !is.na(species),
                                 species))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sdata/mapping/%s/%s/reactions",
                                          rba_ba_stg("reactome", "pth", "content"),
                                          resource,
                                          id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_mapping_reactions.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Orthology Endpoints ####

#' Reactome uses the set of manually curated human reactions to computationally
#' infer reactions in twenty evolutionarily divergent eukaryotic species for
#' which high-quality whole-genome sequence data are available, and hence a
#' comprehensive and high-quality set of protein predictions exists. Thus,
#' this method retrieves the orthology for any given event or entity in the
#' specified species. Here you can find more information about the
#' computationally inferred events.
#'
#' @param ids
#' @param ...
#' @param species_id
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_orthology = function(ids,
                                  species_id,
                                  ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character"),
                          list(arg = "species_id",
                               class = "numeric")))

  v_msg("POST /data/orthologies/ids/species/{speciesId}",
        "The orthologies of a given set of events or entities")

  ## Build POST API Request's URL
  call_body = paste(unique(ids),collapse = "\n")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("reactome", "url"),
                           path = paste0(rba_ba_stg("reactome", "pth", "content"),
                                         "data/orthologies/ids/species/",
                                         species_id),
                           body = call_body,
                           accept = "application/json",
                           httr::content_type("text/plain"),
                           parser = "json->list",
                           save_to = rba_ba_file("reactome_orthology.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Participants Endpoints ####

#' Participants contains a PhysicalEntity (dbId, displayName) and a
#' collection of ReferenceEntities (dbId, name, identifier, url)
#'
#' @param id
#' @param physical_entities
#' @param ...
#' @param reference_entities
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_participants = function(id,
                                     physical_entities = FALSE,
                                     reference_entities = FALSE,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = c("character",
                                         "numeric")),
                          list(arg = "physical_entities",
                               class = "logical"),
                          list(arg = "reference_entities",
                               class = "logical")),
              cond = list(list(quote(sum(physical_entities, reference_entities) == 2),
                               "You can only Request either physical_entities or reference_entities in one function call.")))

  v_msg("GET data/participants/{id}",
        "A list of participants /participating PhysicalEntities /participating ReferenceEntities for a given event")

  ## Build Function-Specific Call
  path_input = paste0(rba_ba_stg("reactome", "pth", "content"),
                      "data/participants/",
                      id)
  if (physical_entities == TRUE){
    path_input = paste0(path_input, "/participatingPhysicalEntities")
  } else if (reference_entities == TRUE) {
    path_input = paste0(path_input, "/referenceEntities")
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("reactome_participants.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Pathways Endpoints ####

#' Events are the building blocks used in Reactome to represent all biological
#' processes, and they include pathways and reactions. Typically, an event can
#' contain other events. For example, a pathway can contain smaller pathways
#' (subpathways) and reactions. This method recursively retrieves a single
#' attribute for each of the events contained in the given event.
#'
#' @param id
#' @param ...
#' @param attribute_name
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_pathways_participants = function(id,
                                              attribute_name = NA,
                                              ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = c("character",
                                         "numeric")),
                          list(arg = "attribute_name",
                               class = "character")))

  v_msg("GET /data/pathway/{id}/containedEvents",
        "All the events contained in the given event",
        "GET /data/pathway/{id}/containedEvents/{attributeName}",
        "A single property for each event contained in the given event")

  ## Build Function-Specific Call
  path_input = sprintf("%sdata/pathway/%s/containedEvents",
                       rba_ba_stg("reactome", "pth", "content"),
                       id)
  accept_input = "application/json"
  parser_input = "json->df"
  file_ext = "json"

  if (!is.na(attribute_name)){
    path_input = paste0(path_input, "/", attribute_name)
    accept_input = "text/plain"
    parser_input = quote(unlist(strsplit(x = gsub(pattern = "\\[|\\]",
                                                  replacement = "",
                                                  x = as.character(httr::content(response,
                                                                                 as = "text",
                                                                                 encoding = "UTF-8"))),
                                         split = ", ")))
    file_ext = "txt"
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           accpet = accept_input,
                           parser = parser_input,
                           save_to = rba_ba_file(paste0(reactome_pathways_participants,
                                                        ".", file_ext)))


  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' This method traverses the event hierarchy and retrieves the list of all
#' lower level pathways that have a diagram and contain the given
#' PhysicalEntity or Event.
#'
#' @param entity_id
#' @param with_diagram
#' @param all_forms
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_pathways_low = function(entity_id,
                                     with_diagram = FALSE,
                                     all_forms = FALSE,
                                     species = NA,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "entity_id",
                               class = "character"),
                          list(arg = "all_forms",
                               class = "logical"),
                          list(arg = "with_diagram",
                               class = "logical"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET /data/pathways/low/diagram/entity/{id}",
        "A list of lower level pathways with diagram containing a given entity or event",
        "GET /data/pathways/low/diagram/entity/{id}/allForms",
        "A list of lower level pathways with diagram containing any form of a given entity",
        "GET /data/pathways/low/entity/{id}",
        "A list of lower level pathways containing a given entity or event",
        "GET /data/pathways/low/entity/{id}/allForms",
        "A list of lower level pathways containing any form of a given entity")

  ## Build Function-Specific Call
  path_input = sprintf("%sdata/pathways/%s/%s",
                       rba_ba_stg("reactome", "pth", "content"),
                       ifelse(with_diagram == TRUE,
                              yes = "low/diagram/entity",
                              no = "low/entity"),
                       entity_id)
  if (all_forms == TRUE) {
    path_input = paste0(path_input,
                        "/allForms")
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_pathways_low.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' This method retrieves the list of top level pathways for the given species
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_pathways_top = function(species,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET /data/pathways/top/{species}",
        "All Reactome top level pathways")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("species",
                                 !is.na(species),
                                 species))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = paste0(rba_ba_stg("reactome", "pth", "content"),
                                         "data/pathways/top/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_pathways_top.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Person Endpoints ####

#' Retrieves a list of people in Reactome with either their first or last name
#' partly/exactly matching the given string.
#'
#' @param person_name
#' @param ...
#' @param exact
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_people_name = function(person_name,
                                    exact = FALSE,
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "person_name",
                               class = "character"),
                          list(arg = "exact",
                               class = "logical")))

  v_msg("/data/people/name/{name}",
        "A list of people with first or last name partly matching a given string",
        "/data/people/name/{name}/exact",
        "A list of people with first AND last name exactly matching a given string")

  ## Build Function-Specific Call
  path_input = paste0(rba_ba_stg("reactome", "pth", "content"),
                      "data/people/name/",
                      gsub(" ", "%20", person_name))
  if (exact == TRUE) {
    path_input = paste0(path_input, "/exact")
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("reactome_people_name.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' A person by his identifiers
#'
#' @param id
#' @param authored_pathways
#' @param publications
#' @param attribute_name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_people_id = function(id,
                                  authored_pathways = FALSE,
                                  publications = FALSE,
                                  attribute_name = NA,
                                  ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = "character"),
                          list(arg = "authored_pathways",
                               class = "logical"),
                          list(arg = "publications",
                               class = "logical"),
                          list(arg = "attribute_name",
                               class = "character")),
              cond = list(list(quote(sum(!is.na(attribute_name),
                                         authored_pathways == TRUE,
                                         publications == TRUE) > 1),
                               "You can only use either attribute_name, authored_pathways or publications function call.")))

  v_msg("GET /data/person/{id}",
        "A person by his/her identifier")

  ## Build Function-Specific Call
  path_input = paste0(rba_ba_stg("reactome", "pth", "content"),
                      "data/person/",
                      id)
  accept_input = "application/json"
  parser_type_input = "json->list_no_simp"
  file_ext = "json"

  if (authored_pathways == TRUE) {
    path_input = paste0(path_input, "/authoredPathways")
  } else if (publications == TRUE) {
    path_input = paste0(path_input, "/publications")
  } else if (!is.na(attribute_name)) {
    path_input = paste0(path_input, "/", attribute_name)
    accept_input = "text/plain"
    parser_type_input = "text->chr"
    file_ext = "txt"
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           accpet = accept_input,
                           parser = parser_type_input,
                           save_to = rba_ba_file(paste0(reactome_people_id,
                                                        ".",
                                                        file_ext)))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Query Endpoints ####

#' all in one: query Reactome Data: Common data retrieval
#'
#' @param ids
#' @param enhanced
#' @param map
#' @param ...
#' @param attribute_name
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_query = function(ids,
                              enhanced = FALSE,
                              map = FALSE,
                              attribute_name = NA,
                              ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 20),
                          list(arg = "enhanced",
                               class = "logical"),
                          list(arg = "map",
                               class = "logical"),
                          list(arg = "attribute_name",
                               class = "character")),
              cond = list(list(quote(length(ids) > 1 &
                                       (enhanced == TRUE | !is.na(attribute_name))),
                               "You can only use 'enhnaced' or 'attribute_name' with a single id not multiple ids"),
                          list(quote(!is.na(attribute_name) && enhanced == TRUE),
                               "You can only provide 'attribute_name' when enhanced is 'FALSE'")))

  if (length(ids) > 1) {
    #### use POST
    v_msg("POST /data/query/ids/map",
          "A list of entries with their mapping to the provided identifiers")
    ## Build POST API Request's URL
    call_body = paste(unique(ids),collapse = ",")
    path_input = paste0(rba_ba_stg("reactome", "pth", "content"),
                        ifelse(map == TRUE,
                               yes = "data/query/ids/map",
                               no = "data/query/ids"))
    ## Build Function-Specific Call
    input_call = rba_ba_httr(httr = "post",
                             url = rba_ba_stg("reactome", "url"),
                             path = path_input,
                             body = call_body,
                             parser = "json->list_no_simp",
                             accept = "application/json",
                             httr::content_type("text/plain"),
                             save_to = rba_ba_file("reactome_query.json"))
  } else {
    #### use GET
    ## Build Function-Specific Call
    path_input = paste0(rba_ba_stg("reactome", "pth", "content"),
                        "data/query/",
                        ids)
    accept_input = "application/json"
    parser_input = "json->list_no_simp"
    file_ext = "json"
    if (!is.na(attribute_name)) {
      v_msg("GET /data/query/{id}/{attributeName}",
            "A single property of an entry in Reactome knowledgebase")
      path_input = paste0(path_input, "/", attribute_name)
      accept_input = "text/plain"
      parser_input = "text->chr"
      file_ext = "txt"
    } else if (enhanced == TRUE){
      v_msg("GET /data/query/enhanced/{id}",
            "More information on an entry in Reactome knowledgebase")
      path_input = sub("/query/", "/query/enhanced/", path_input)
    } else {
      v_msg("GET /data/query/{id}",
            "An entry in Reactome knowledgebase")
    }
    input_call = rba_ba_httr(httr = "get",
                             url = rba_ba_stg("reactome", "url"),
                             path = path_input,
                             parser = parser_input,
                             accept = accept_input,
                             save_to = rba_ba_file(paste0("reactome_query.",
                                                          file_ext)))

  }

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Refrences Endpoints ####

#' Retrieves a list containing all the reference entities for a given identifier.
#'
#' @param ...
#' @param id
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_complex_subunits = function(id,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "id",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET /references/mapping/{identifier}",
        "All ReferenceEntities for a given identifier")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = paste0(rba_ba_stg("reactome", "pth", "content"),
                                         "references/mapping/",
                                         id),
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("reactome_complex_subunits.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### species Endpoints ####

#' This method retrieves the list of main species in Reactome knowledgebase,
#' sorted by name, but having ‘Homo sapiens’ as the first one. It should be
#' mentioned that for Reactome, main species are considered those have either
#' manually curated or computationally inferred pathways.
#'
#' @param ...
#' @param species_type
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_species = function(species_type = "all",
                                ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species_type",
                               class = "character",
                               val = c("all",
                                       "main"))))

  v_msg("GET /data/species/all",
        "The list of all species in Reactome",
        "GET /data/species/main",
        "The list of main species in Reactome")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = paste0(rba_ba_stg("reactome", "pth", "content"),
                                         "data/species/",
                                         species_type),
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("reactome_species.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
