#### database Endpoints ####
#' The version number of current database
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_version = function(verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/database/version")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/database/version"),
                                    httr::accept("text/plain")
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "text->chr",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

#### diseases Endpoints ####

#' It retrieves the list of diseases annotated in Reactome OR
#' list of disease DOIDs annotated in Reactome
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_diseases = function(doid = FALSE,
                                 verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = doid,
                                         name = "doid",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET data/diseases", "\r\n",
            "/data/diseases/doid")
  }

  ## make function-specific calls
  if (doid == FALSE) {
    path_input = paste0("ContentService/",
                        "data/diseases")
    accept_input = "application/json"
    parser_input = "json->df"
  } else {
    path_input = paste0("ContentService/",
                        "data/diseases/doid")
    accept_input = "text/plain"
    parser_input = "text->df"
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = path_input,
                                    httr::accept(accept_input)
  ))


  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = parser_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
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
#' @param exclude_structures
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_complex_subunits = function(complex_id,
                                         exclude_structures = FALSE,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = complex_id,
                                         name = "complex_id",
                                         class = "character"),
                                    list(arg = exclude_structures,
                                         name = "exclude_structures",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/complex/{id}/subunits")
  }
  ## build GET API request's query
  additional_pars = list(list(exclude_structures == TRUE,
                              list("excludeStructures" = "true")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/complex/",
                                                  complex_id,
                                                  "/subunits"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Retrieves the list of complexes that contain a given (identifier, resource).
#' The method deconstructs the complexes into all its participants to do so.
#'
#' @param id
#' @param resource
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_complex_list = function(id,
                                     resource,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = "character"),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/complexes/{resource}/{identifier}")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/complexes/",
                                                  resource, "/",
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

#' Retrieves the list of structures (Complexes and Sets) that include the
#' given entity as their component. It should be mentioned that the list
#' includes only simplified entries (type, names, ids) and not full
#' information about each item.
#'
#' @param entity_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_entity_component_of = function(entity_id,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = entity_id,
                                         name = "entity_id",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET/data/entity/{id}/componentOf",
            "A list of larger structures containing the entity")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/entity/",
                                                  entity_id,
                                                  "/componentOf"),
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

#' Retrieves a list containing all other forms of the given PhysicalEntity.
#' These other forms are PhysicalEntities that share the same ReferenceEntity
#' identifier, e.g. PTEN H93R[R-HSA-2318524] and PTEN C124R[R-HSA-2317439]
#' are two forms of PTEN.
#'
#' @param entity_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_entity_other_forms = function(entity_id,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = entity_id,
                                         name = "entity_id",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET data/entity/{id}/otherForms",
            "All other forms of a PhysicalEntity")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/entity/",
                                                  entity_id,
                                                  "/otherForms"),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

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
#' @param event_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_event_ancestors = function(event_id,
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = event_id,
                                         name = "event_id",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/event/{id}/ancestors",
            "The ancestors of a given event")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/event/",
                                                  event_id,
                                                  "/ancestors"),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

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
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_event_hierarchy = function(species,
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "event_id",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("/data/eventsHierarchy/{species}",
            "The full event hierarchy for a given species")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/eventsHierarchy/",
                                                  species),
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
#' @param exp_column
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = event_id,
                                         name = "event_id",
                                         class = "character"),
                                    list(arg = output_format,
                                         name = "output_format",
                                         class = "character",
                                         val = c("png",
                                                 "jpeg",
                                                 "svg",
                                                 "gif")),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character"),
                                    list(arg = quality,
                                         name = "quality",
                                         class = "numeric",
                                         ran = c(1,10)),
                                    list(arg = flg,
                                         name = "flg",
                                         class = "character"),
                                    list(arg = flg_interactors,
                                         name = "flg_interactors",
                                         class = "logical"),
                                    list(arg = sel,
                                         name = "sel",
                                         class = "character"),
                                    list(arg = title,
                                         name = "title",
                                         class = "logical"),
                                    list(arg = margin,
                                         name = "margin",
                                         class = "numeric",
                                         ran = c(0,20)),
                                    list(arg = ehld,
                                         name = "ehld",
                                         class = "logical"),
                                    list(arg = diagram_profile,
                                         name = "diagram_profile",
                                         class = "character",
                                         val = c("Modern",
                                                 "Standard")),
                                    list(arg = token,
                                         name = "token",
                                         class = "character"),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character"),
                                    list(arg = analysis_profile,
                                         name = "analysis_profile",
                                         class = "character",
                                         val = c("Standard",
                                                 "Strosobar",
                                                 "Copper Plus")),
                                    list(arg = exp_column,
                                         name = "exp_column",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("/exporter/diagram/{identifier}.{ext}",
            "Exports a given pathway diagram to the specified image format")
  }

  ## build GET API request's query
  additional_pars = list(list(quality != 5,
                              list("quality" = quality)),
                         list(!is.na(flg),
                              list("flg" = flg)),
                         list(flg_interactors == FALSE,
                              list("flgInteractors" = "false")),
                         list(!is.na(sel),
                              list("sel" = sel)),
                         list(title == FALSE,
                              list("title" = "false")),
                         list(margin != 15,
                              list("margin" = as.integer(margin))),
                         list(ehld == FALSE,
                              list("ehld" = ehld)),
                         list(diagram_profile != "Modern",
                              list("diagramProfile" = diagram_profile)),
                         list(!is.na(token),
                              list("token" = token)),
                         list(resource != "TOTAL",
                              list("resource" = resource)),
                         list(!is.na(analysis_profile),
                              list("analysisProfile" = analysis_profile)),
                         list(!is.na(exp_column),
                              list("expColumn" = exp_column)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)


  ## make function-specific calls
  if (output_format == "svg") {
    accept_input = "image/svg+xml"
  } else {
    accept_input = paste0("image/", output_format)
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "exporter/diagram/",
                                                  event_id, ".",
                                                  output_format),
                                    query = call_query,
                                    httr::accept(accept_input),
                                    httr::write_disk(save_to, overwrite = TRUE)
  ))

  # create file_path
  save_to = rba_ba_file_path(file_ext = output_format,
                             file_name = event_id,
                             dir_name = "rba_reactome",
                             save_to = save_to,
                             verbose = verbose,
                             diagnostics = diagnostics)

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  invisible()
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
#' @param exp_column
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = event_id,
                                         name = "event_id",
                                         class = "character"),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character"),
                                    list(arg = level,
                                         name = "level",
                                         class = "numeric",
                                         val = c(0,1)),
                                    list(arg = diagram_profile,
                                         name = "diagram_profile",
                                         class = "character",
                                         val = c("Modern",
                                                 "Standard")),
                                    list(arg = token,
                                         name = "token",
                                         class = "character"),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character"),
                                    list(arg = analysis_profile,
                                         name = "analysis_profile",
                                         class = "character",
                                         val = c("Standard",
                                                 "Strosobar",
                                                 "Copper Plus")),
                                    list(arg = exp_column,
                                         name = "exp_column",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("/exporter/document/event/{identifier}.pdf",
            "Exports the content of a given event (pathway or reaction) to a PDF document")
  }

  ## build GET API request's query
  additional_pars = list(list(level != 1,
                              list("level" = level)),
                         list(diagram_profile != "Modern",
                              list("diagramProfile" = diagram_profile)),
                         list(!is.na(token),
                              list("token" = token)),
                         list(resource != "TOTAL",
                              list("resource" = resource)),
                         list(!is.na(analysis_profile),
                              list("analysisProfile" = analysis_profile)),
                         list(!is.na(exp_column),
                              list("expColumn" = exp_column)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)


  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "exporter/document/event/",
                                                  event_id, ".pdf"),
                                    query = call_query,
                                    httr::accept("application/pdf"),
                                    httr::write_disk(save_to, overwrite = TRUE)
  ))

  # create file_path
  save_to = rba_ba_file_path(file_ext = "pdf",
                             file_name = event_id,
                             dir_name = "rba_reactome",
                             save_to = save_to,
                             verbose = verbose,
                             diagnostics = diagnostics)
  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  invisible()
}

#' Exports a given pathway or reaction to SBGN or SBML
#'
#' @param event_id
#' @param output_format
#' @param save_to
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_exporter_event = function(event_id,
                                       output_format,
                                       save_to = NA,
                                       verbose = TRUE,
                                       progress_bar = FALSE,
                                       diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = event_id,
                                         name = "event_id",
                                         class = "character"),
                                    list(arg = output_format,
                                         name = "output_format",
                                         class = "character",
                                         val = c("sbgn",
                                                 "sbml")),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("/exporter/event/{identifier}.sbgn",
            "Exports a given pathway or reaction to SBGN or SBML")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "exporter/event/",
                                                  event_id, ".",
                                                  output_format),
                                    httr::write_disk(save_to, overwrite = TRUE)
  ))

  # create file_path
  save_to = rba_ba_file_path(file_ext = output_format,
                             file_name = event_id,
                             dir_name = "rba_reactome",
                             save_to = save_to,
                             verbose = verbose,
                             diagnostics = diagnostics)

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  invisible()
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
#' @param coverage
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = output_format,
                                         name = "output_format",
                                         class = "character",
                                         val = c("png",
                                                 "jpeg",
                                                 "svg",
                                                 "gif")),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character"),
                                    list(arg = quality,
                                         name = "quality",
                                         class = "numeric",
                                         ran = c(1,10)),
                                    list(arg = flg,
                                         name = "flg",
                                         class = "character"),
                                    list(arg = flg_interactors,
                                         name = "flg_interactors",
                                         class = "logical"),
                                    list(arg = sel,
                                         name = "sel",
                                         class = "character"),
                                    list(arg = title,
                                         name = "title",
                                         class = "logical"),
                                    list(arg = margin,
                                         name = "margin",
                                         class = "numeric",
                                         ran = c(0,20)),
                                    list(arg = diagram_profile,
                                         name = "diagram_profile",
                                         class = "character",
                                         val = c("Copper",
                                                 "Copper Plus",
                                                 "Barium Lithium",
                                                 "calcium salts")),
                                    list(arg = token,
                                         name = "token",
                                         class = "character"),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character"),
                                    list(arg = exp_column,
                                         name = "exp_column",
                                         class = "character"),
                                    list(arg = coverage,
                                         name = "coverage",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("/exporter/fireworks/{species}.{ext}",
            "Exports a given pathway overview to the specified image format")
  }

  ## build GET API request's query
  additional_pars = list(list(quality != 5,
                              list("quality" = quality)),
                         list(!is.na(flg),
                              list("flg" = flg)),
                         list(flg_interactors == FALSE,
                              list("flgInteractors" = "false")),
                         list(!is.na(sel),
                              list("sel" = sel)),
                         list(title == FALSE,
                              list("title" = "false")),
                         list(margin != 15,
                              list("margin" = as.integer(margin))),
                         list(diagram_profile != "Copper",
                              list("diagramProfile" = diagram_profile)),
                         list(!is.na(token),
                              list("token" = token)),
                         list(resource != "TOTAL",
                              list("resource" = resource)),
                         list(!is.na(exp_column),
                              list("expColumn" = exp_column)),
                         list(coverage == TRUE,
                              list("coverage" = "true")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)


  ## make function-specific calls
  if (output_format == "svg") {
    accept_input = "image/svg+xml"
  } else {
    accept_input = paste0("image/", output_format)
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "exporter/fireworks/",
                                                  gsub(" ", "%20",species),
                                                  ".", output_format),
                                    query = call_query,
                                    httr::accept(accept_input),
                                    httr::write_disk(save_to, overwrite = TRUE)
  ))

  # create file_path
  save_to = rba_ba_file_path(file_ext = output_format,
                             file_name = species,
                             dir_name = "rba_reactome",
                             save_to = save_to,
                             verbose = verbose,
                             diagnostics = diagnostics)
  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  invisible()
}

#### Interactors Endpoints ####

#' Retrieve clustered interaction, sorted by score, of a given accession(s)
#' by resource.
#'
#' @param resource
#' @param proteins
#' @param level
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_interactors_psicquic = function(resource,
                                             proteins,
                                             details = "details",
                                             verbose = TRUE,
                                             progress_bar = FALSE,
                                             diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = resource,
                                         name = "resource",
                                         class = "character"),
                                    list(arg = proteins,
                                         name = "proteins",
                                         class = c("character",
                                                   "numeric"),
                                         max_len = 1000),
                                    list(arg = details,
                                         name = "details",
                                         class = "character",
                                         val = c("details",
                                                 "summary"))),

                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST /interactors/psicquic/molecules/{resource}/details",
            "Retrieve clustered interaction, sorted by score, of a given accession(s) by resource.",
            "POST/interactors/psicquic/molecules/{resource}/summary",
            "Retrieve a summary of a given accession list by resource.")
  }

  ## build POST API request's URL
  call_body = paste(unique(proteins),collapse = "\n")

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_reactome"),
                                     path = paste0("ContentService/",
                                                   "interactors/psicquic/molecules/",
                                                   resource, "/",
                                                   details),
                                     body = call_body,
                                     httr::accept_json(),
                                     httr::content_type("text/plain")
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

#' Retrieve a list of all Psicquic Registries services
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_interactors_resources = function(verbose = TRUE,
                                              progress_bar = FALSE,
                                              diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /interactors/psicquic/resources",
            "Retrieve a list of all Psicquic Registries services")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "/interactors/psicquic/resources"),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

rba_reactome_interactors_static = function(proteins,
                                             details = "details",
                                             verbose = TRUE,
                                             progress_bar = FALSE,
                                             diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = proteins,
                                         name = "proteins",
                                         class = c("character",
                                                   "numeric"),
                                         max_len = 1000),
                                    list(arg = details,
                                         name = "details",
                                         class = "character",
                                         val = c("details",
                                                 "summary"))),

                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST /interactors/static/molecules/details",
            "Retrieve a detailed interaction information of a given accession",
            "POST/interactors/static/molecules/summary",
            "Retrieve a summary of a given accession list")
  }

  ## build POST API request's URL
  call_body = paste(unique(proteins),collapse = "\n")

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_reactome"),
                                     path = paste0("ContentService/",
                                                   "interactors/static/molecules/",
                                                   details),
                                     body = call_body,
                                     httr::accept_json(),
                                     httr::content_type("text/plain")
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

#### Mapping Endpoints ####

#' Entities play different roles in reactions, and reactions are events that
#' conform a pathway. This method retrieves the pathways for which an
#' identifier plays a role within one or more of their events.
#'
#' @param id
#' @param resource
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_mapping_pathways = function(id,
                                         resource,
                                         species = NA,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/mapping/{resource}/{identifier}/pathways",
            "The lower level pathways where an identifier can be mapped to")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(species),
                              list("species" = species)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/mapping/",
                                                  resource, "/",
                                                  id, "/pathways"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Entities play different roles in reactions. This method retrieves the
#' reactions for which an identifier plays a role .
#'
#' @param id
#' @param resource
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_mapping_reactions = function(id,
                                          resource,
                                          species = NA,
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/mapping/{resource}/{identifier}/reactions",
            "The reactions where an identifier can be mapped to")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(species),
                              list("species" = species)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/mapping/",
                                                  resource, "/",
                                                  id, "/reactions"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

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
#' @param species_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_orthology = function(ids,
                                  species_id,
                                  verbose = TRUE,
                                  progress_bar = FALSE,
                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ids,
                                         name = "ids",
                                         class = "character"),
                                    list(arg = species_id,
                                         name = "species_id",
                                         class = "numeric")),

                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST /data/orthologies/ids/species/{speciesId}",
            "The orthologies of a given set of events or entities")
  }

  ## build POST API request's URL
  call_body = paste(unique(ids),collapse = "\n")

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_reactome"),
                                     path = paste0("ContentService/",
                                                   "data/orthologies/ids/species/",
                                                   species_id),
                                     body = call_body,
                                     httr::accept_json(),
                                     httr::content_type("text/plain")
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

#### Participants Endpoints ####

#' Participants contains a PhysicalEntity (dbId, displayName) and a
#' collection of ReferenceEntities (dbId, name, identifier, url)
#'
#' @param id
#' @param physical_entities
#' @param reference_entities
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_participants = function(id,
                                     physical_entities = FALSE,
                                     reference_entities = FALSE,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = physical_entities,
                                         name = "physical_entities",
                                         class = "logical"),
                                    list(arg = reference_entities,
                                         name = "reference_entities",
                                         class = "logical")),
                        cond = list(list(sum(physical_entities, reference_entities) == 2,
                                         "You can only Request either physical_entities or reference_entities in one function call.")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET data/participants/{id}",
            "A list of participants /participating PhysicalEntities /participating ReferenceEntities for a given event")
  }

  ## make function-specific calls
  path_input = paste0("ContentService/",
                      "data/participants/",
                      id)
  if (physical_entities == TRUE){
    path_input = paste0(path_input, "/participatingPhysicalEntities")
  } else if (reference_entities == TRUE) {
    path_input = paste0(path_input, "/referenceEntities")
  }

  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = path_input,
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

#### Pathways Endpoints ####

#' Events are the building blocks used in Reactome to represent all biological
#' processes, and they include pathways and reactions. Typically, an event can
#' contain other events. For example, a pathway can contain smaller pathways
#' (subpathways) and reactions. This method recursively retrieves a single
#' attribute for each of the events contained in the given event.
#'
#' @param id
#' @param attribute_name
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_pathways_participants = function(id,
                                              attribute_name = NA,
                                              verbose = TRUE,
                                              progress_bar = FALSE,
                                              diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = attribute_name,
                                         name = "attribute_name",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/pathway/{id}/containedEvents",
            "All the events contained in the given event",
            "GET /data/pathway/{id}/containedEvents/{attributeName}",
            "A single property for each event contained in the given event")
  }

  ## make function-specific calls
  path_input = paste0("ContentService/",
                      "data/pathway/",
                      id,
                      "/containedEvents")
  accept_input = "application/json"
  parser_input = "json->df"

  if (!is.na(attribute_name)){
    path_input = paste0(path_input, "/", attribute_name)
    accept_input = "text/plain"
    parser_input = quote(unlist(strsplit(x = gsub(pattern = "\\[|\\]",
                                                  replacement = "",
                                                  x = as.character(httr::content(response,
                                                                                 as = "text",
                                                                                 encoding = "UTF-8")),
    ),
    split = ", ", perl = TRUE)
    ))
  }

  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = path_input,
                                    httr::accept(accept_input)
  ))


  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = parser_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

#' This method traverses the event hierarchy and retrieves the list of all
#' lower level pathways that have a diagram and contain the given
#' PhysicalEntity or Event.
#'
#' @param entity_id
#' @param with_diagram
#' @param all_forms
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_pathways_low = function(entity_id,
                                     with_diagram = FALSE,
                                     all_forms = FALSE,
                                     species = NA,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = entity_id,
                                         name = "entity_id",
                                         class = "character"),
                                    list(arg = all_forms,
                                         name = "all_forms",
                                         class = "logical"),
                                    list(arg = with_diagram,
                                         name = "with_diagram",
                                         class = "logical"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/pathways/low/diagram/entity/{id}",
            "A list of lower level pathways with diagram containing a given entity or event",
            "GET /data/pathways/low/diagram/entity/{id}/allForms",
            "A list of lower level pathways with diagram containing any form of a given entity",
            "GET /data/pathways/low/entity/{id}",
            "A list of lower level pathways containing a given entity or event",
            "GET /data/pathways/low/entity/{id}/allForms",
            "A list of lower level pathways containing any form of a given entity")
  }

  ## make function-specific calls
  path_input = paste0("ContentService/",
                      "data/pathways/low/entity/",
                      entity_id)
  if (all_forms == TRUE) {
    path_input = paste0(path_input,
                        "/allForms")
  }
  if (with_diagram == TRUE) {
    path_input = sub("/low/entity/", "/low/diagram/entity/", path_input)
  }


  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = path_input,
                                    httr::accept_json()))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

#' This method retrieves the list of top level pathways for the given species
#'
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_pathways_top = function(species,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/pathways/top/{species}",
            "All Reactome top level pathways")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(species),
                              list("species" = species)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/pathways/top/",
                                                  species),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Person Endpoints ####

#' Retrieves a list of people in Reactome with either their first or last name
#' partly/exactly matching the given string.
#'
#' @param person_name
#' @param exact
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_people_name = function(person_name,
                                    exact = FALSE,
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = person_name,
                                         name = "person_name",
                                         class = "character"),
                                    list(arg = exact,
                                         name = "exact",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("/data/people/name/{name}",
            "A list of people with first or last name partly matching a given string",
            "/data/people/name/{name}/exact",
            "A list of people with first AND last name exactly matching a given string")
  }

  ## make function-specific calls
  path_input = paste0("ContentService/",
                      "data/people/name/",
                      gsub(" ", "%20", person_name))
  if (exact == TRUE) {
    path_input = paste0(path_input, "/exact")
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = path_input,
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

rba_reactome_people_id = function(id,
                                  authored_pathways = FALSE,
                                  publications = FALSE,
                                  attribute_name = NA,
                                  verbose = TRUE,
                                  progress_bar = FALSE,
                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = "character"),
                                    list(arg = authored_pathways,
                                         name = "authored_pathways",
                                         class = "logical"),
                                    list(arg = publications,
                                         name = "publications",
                                         class = "logical"),
                                    list(arg = attribute_name,
                                         name = "attribute_name",
                                         class = "character")),
                        cond = list(list(sum(!is.na(attribute_name),
                                             authored_pathways == TRUE,
                                             publications == TRUE) > 1,
                                         "You can only provide either attribute_name, authored_pathways or publications function call.")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/person/{id}",
            "A person by his/her identifier")
  }

  ## make function-specific calls
  path_input = paste0("ContentService/",
                      "data/person/",
                      id)
  accept_input = "application/json"
  parser_type_input = "json->list_no_simp"

  if (authored_pathways == TRUE) {
    path_input = paste0(path_input, "/authoredPathways")
  } else if (publications == TRUE) {
    path_input = paste0(path_input, "/publications")
  } else if (!is.na(attribute_name)) {
    path_input = paste0(path_input, "/", attribute_name)
    accept_input = "text/plain"
    parser_type_input = "text->chr"
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = path_input,
                                    httr::accept(accept_input)
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = parser_type_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Query Endpoints ####

#' all in one: query Reactome Data: Common data retrieval
#'
#' @param ids
#' @param enhanced
#' @param map
#' @param verbose
#' @param attribute_name
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_query = function(ids,
                              enhanced = FALSE,
                              map = FALSE,
                              verbose = TRUE,
                              attribute_name = NA,
                              progress_bar = FALSE,
                              diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ids,
                                         name = "ids",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = enhanced,
                                         name = "enhanced",
                                         class = "logical"),
                                    list(arg = map,
                                         name = "map",
                                         class = "logical"),
                                    list(arg = attribute_name,
                                         name = "attribute_name",
                                         class = "character")),
                        cond = list(list(length(ids) > 1 &
                                           (enhanced == TRUE | !is.na(attribute_name)),
                                         "You can only use 'enhnaced' or 'attribute_name' with a single id not multiple ids"),
                                    list(!is.na(attribute_name) && enhanced == TRUE,
                                         "You can only provide 'attribute_name' when enhanced is 'FALSE'")),
                        diagnostics = diagnostics))

  if (length(ids) > 1) {
    #### use POST
    ## build POST API request's URL
    call_body = paste(unique(ids),collapse = ",")
    parser_type_input = "json->list_no_simp"
    if (map == TRUE) {
      if (verbose == TRUE){
        message("POST /data/query/ids/map",
                "A list of entries with their mapping to the provided identifiers")}
      path_input = paste0("ContentService/",
                          "data/query/ids/map")
    } else {
      if (verbose == TRUE){
        message("POST /data/query/ids/map",
                "A list of entries with their mapping to the provided identifiers")}
      path_input = paste0("ContentService/",
                          "data/query/ids")
    }
    ## make function-specific calls
    call_func_input = quote(httr::POST(url = getOption("rba_url_reactome"),
                                       path = path_input,
                                       body = call_body,
                                       httr::accept_json(),
                                       httr::content_type("text/plain")
    ))
  } else {
    #### use GET
    ## make function-specific calls
    path_input = paste0("ContentService/",
                        "data/query/",
                        ids)
    accept_input = "application/json"
    parser_type_input = "json->list_no_simp"
    if (!is.na(attribute_name)) {
      if (verbose == TRUE){
        message("GET /data/query/{id}/{attributeName}",
                "A single property of an entry in Reactome knowledgebase")}
      path_input = paste0(path_input, "/", attribute_name)
      accept_input = "text/plain"
      parser_type_input = "text->chr"
    } else if (enhanced == TRUE){
      if (verbose == TRUE){
        message("GET /data/query/enhanced/{id}",
                "More information on an entry in Reactome knowledgebase")}
      path_input = sub("/query/", "/query/enhanced/", path_input)
    } else {
      if (verbose == TRUE){
        message("GET /data/query/{id}",
                "An entry in Reactome knowledgebase")}
    }
    call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                      path = path_input,
                                      httr::accept(accept_input)
    ))

  }

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = parser_type_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Refrences Endpoints ####

#' Retrieves a list containing all the reference entities for a given identifier.
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
rba_reactome_complex_subunits = function(id,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = id,
                                         name = "id",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /references/mapping/{identifier}",
            "All ReferenceEntities for a given identifier")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "references/mapping/",
                                                  id),
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

#### species Endpoints ####

#' This method retrieves the list of main species in Reactome knowledgebase,
#' sorted by name, but having ‘Homo sapiens’ as the first one. It should be
#' mentioned that for Reactome, main species are considered those have either
#' manually curated or computationally inferred pathways.
#'
#' @param species_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_species = function(species_type = "all",
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species_type,
                                         name = "species_type",
                                         class = "character",
                                         val = c("all",
                                                 "main"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /data/species/all",
            "The list of all species in Reactome",
            "GET /data/species/main",
            "The list of main species in Reactome")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("ContentService/",
                                                  "data/species/",
                                                  species_type),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->df",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}
