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
                                  response_parser = NULL,
                                  parser_type = "text->chr",
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
                                  response_parser = NULL,
                                  parser_type = parser_input,
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
                                  response_parser = NULL,
                                  parser_type = "json->df",
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
                                  response_parser = NULL,
                                  parser_type = "json->df",
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
rba_reactome_complex_other_forms = function(entity_id,
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
                                                  "/data/entity/",
                                                  entity_id,
                                                  "/otherForms"),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->df",
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
                                                  "/data/event/",
                                                  event_id,
                                                  "/ancestors"),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->df",
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
rba_reactome_event_ancestors = function(species,
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
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

