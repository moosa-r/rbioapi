#### Internal functions ####
#' internal function to feed input to Reactome analysis
#'
#' @param input
#' @param type
#'
#' @return
#' @export
#' @examples
rba_ba_reactome_input = function(input,
                                 type = NA,
                                 handle = TRUE,
                                 verbose = TRUE,
                                 diagnostics = FALSE){
  ### 1 identify input
  if (is.na(type)) {
    if (is.data.frame(input) |
        is.matrix(input)) {
      type = "table"
    } else if (is.vector(input) &&
               length(input) > 1) {
      type = "vector"
    } else if (is.character(input) &&
               length(input) == 1) {
      if (file.exists(input) |
          grepl(pattern = "^[a-zA-z]:|^\\\\\\w|^/|^\\w+\\.\\w+$",
                x = input)
      ) {
        type = "file"
      } else if (grepl(pattern = "^http\\:|^https\\:|^ftp\\:|^ftps\\:|^\\w+\\.\\w+/\\w",
                       x = input)) {
        type = "url"
      } else {
        stop("Couldn't identify your input format. Please specify it using 'type' argument.",
             call. = diagnostics)
      }
    } else {
      stop("Couldn't identify your input format. Please specify it using 'type' argument.",
           call. = diagnostics)
    }
  }
  ### 2 handle input
  if (handle == FALSE) {
    return(type)
  } else {
    if (type == "table") {
      input = as.data.frame(input,
                            stringsAsFactors = FALSE)
      #make sure that colnames start with #
      inproper_colnames = !grepl("^#", colnames(input))
      if (any(inproper_colnames)) {
        colnames(input)[inproper_colnames] = paste0("#",
                                                    colnames(input)[inproper_colnames])
      }
      temp_file = tempfile(pattern = "rba", fileext = ".txt")
      utils::write.table(x = input,
                  file = temp_file,
                  sep = "\t",
                  row.names = FALSE,
                  col.names = TRUE)
      return(list(type = type,
                  file = temp_file))
    } else if (type == "vector") {
      temp_file = tempfile(pattern = "rba", fileext = ".txt")
      writeLines(text = input,
                 con = temp_file,
                 sep = "\n")
      return(list(type = type,
                  file = temp_file))
    } else if (type == "file" |
               type == "url") {
      return(list(type = type,
                  file = input))
    }
  }
}
#### Identifiers Endpoints ####

#' identifiers Queries for multiple identifiers
#'
#' @param input
#' @param input_type
#' @param species
#' @param projection
#' @param interactors
#' @param sort_by
#' @param order
#' @param resource
#' @param p_value
#' @param include_disease
#' @param min
#' @param max
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis = function(input,
                                 input_type = NA,
                                 projection = FALSE,
                                 interactors = FALSE,
                                 species = NA,
                                 sort_by = "ENTITIES_PVALUE",
                                 order = "ASC",
                                 resource = "TOTAL",
                                 p_value = NA,
                                 include_disease = TRUE,
                                 min = NA,
                                 max = NA,
                                 verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE) {

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = input,
                                         name = "input",
                                         class = c("character",
                                                   "numeric",
                                                   "data.frame",
                                                   "matrix")),
                                    list(arg = input_type,
                                         name = "input_type",
                                         class = "character",
                                         val = c("table",
                                                 "vector",
                                                 "file",
                                                 "url")),
                                    list(arg = projection,
                                         name = "projection",
                                         class = "logical"),
                                    list(arg = interactors,
                                         name = "interactors",
                                         class = "logical"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = sort_by,
                                         name = "sort_by",
                                         class = "character",
                                         val = c("NAME",
                                                 "TOTAL_ENTITIES",
                                                 "TOTAL_INTERACTORS",
                                                 "TOTAL_REACTIONS",
                                                 "FOUND_ENTITIES",
                                                 "FOUND_INTERACTORS",
                                                 "FOUND_REACTIONS",
                                                 "ENTITIES_RATIO",
                                                 "ENTITIES_PVALUE",
                                                 "ENTITIES_FDR",
                                                 "REACTIONS_RATIO")),
                                    list(arg = order,
                                         name = "order",
                                         class = "character",
                                         val = c("ASC",
                                                 "DESC")),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character",
                                         val = c("TOTAL",
                                                 "UNIPROT",
                                                 "ENSEMBL",
                                                 "CHEBI",
                                                 "IUPHAR",
                                                 "MIRBASE",
                                                 "NCBI_PROTEIN",
                                                 "EMBL",
                                                 "COMPOUND",
                                                 "ENTITIES_FDR",
                                                 "PUBCHEM_COMPOUND")),
                                    list(arg = p_value,
                                         name = "p_value",
                                         class = "numeric"),
                                    list(arg = include_disease,
                                         name = "include_disease",
                                         class = "logical"),
                                    list(arg = min,
                                         name = "min",
                                         class = "numeric"),
                                    list(arg = max,
                                         name = "max",
                                         class = "numeric")),
                        cond = list(list(sum(projection, !is.na(species)) == 2,
                                         "You cannot Provide 'species' when 'projection' argument is TRUE")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST /identifiers/form",
            "Analyse the identifiers in the file over the different species",
            "POST /identifiers/url",
            "Analyse the identifiers contained in the provided url over the different species",
            "POST/identifiers/form/projection",
            "Analyse the identifiers in the file over the different species and projects the result to Homo Sapiens",
            "POST /identifiers/url/projection",
            "Analyse the identifiers contained in the provided url over the different species and projects the result to Homo Sapiens")
  }

  ## handle provided input
  input = rba_ba_reactome_input(input = input,
                                type = input_type,
                                handle = TRUE,
                                verbose = verbose,
                                diagnostics = diagnostics)

  ## build POST API request's query
  call_query = list("interactors" = ifelse(interactors, "true", "false"),
                    "sortBy" = sort_by,
                    "order" = order,
                    "resource" = resource,
                    "includeDisease" = ifelse(include_disease, "true", "false"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(p_value),
                              list("pValue" = p_value)),
                         list(!is.na(min),
                              list("min" = min)),
                         list(!is.na(max),
                              list("max" = max)))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## build POST API request's URL
  call_body = list(file = httr::upload_file(path = input$file,
                                            type = "text/plain"))

  ## make function-specific calls
  if (input$type == "url") {
    path_input = paste0("AnalysisService/",
                        "identifiers/url")
  } else {
    path_input = paste0("AnalysisService/",
                        "identifiers/form")
  }
  if (projection == TRUE) {
    path_input = paste0(path_input, "/projection")
  }
  call_func_input = quote(httr::POST(url = getOption("rba_url_reactome"),
                                     path = path_input,
                                     body = call_body,
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

#### Report Endpoints ####
#' This method provides a report for a given pathway analysis result in a PDF
#' document. This document contains data about the analysis itself followed by
#' the pathways overview and the most significant pathways overlaid with the
#' analysis result. Users can download and store this information in a
#' convenient format to be checked in the future when the ‘token’ is not
#' longer available.
#'
#' @param token
#' @param species
#' @param save_to
#' @param number
#' @param resource
#' @param diagram_profile
#' @param analysis_profile
#' @param fireworks_profile
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis_pdf = function(token,
                                     species,
                                     save_to = NA,
                                     number  = 25,
                                     resource = "TOTAL",
                                     diagram_profile = "Modern",
                                     analysis_profile = "Standard",
                                     fireworks_profile = "Barium Lithium",
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE){
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = token,
                                         name = "input",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character"),
                                    list(arg = number,
                                         name = "number",
                                         class = "numeric"),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character",
                                         val = c("TOTAL",
                                                 "UNIPROT",
                                                 "ENSEMBL",
                                                 "CHEBI",
                                                 "IUPHAR",
                                                 "MIRBASE",
                                                 "NCBI_PROTEIN",
                                                 "EMBL",
                                                 "COMPOUND",
                                                 "ENTITIES_FDR",
                                                 "PUBCHEM_COMPOUND")),
                                    list(arg = diagram_profile,
                                         name = "diagram_profile",
                                         class = "character",
                                         val = c("Modern",
                                                 "Standard")),
                                    list(arg = analysis_profile,
                                         name = "analysis_profile",
                                         class = "character",
                                         val = c("Standard",
                                                 "Strosobar",
                                                 "Copper Plus")),
                                    list(arg = fireworks_profile,
                                         name = "fireworks_profile",
                                         class = "character",
                                         val = c("Copper",
                                                 "Copper Plus",
                                                 "Barium Lithium",
                                                 "calcium salts"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /report/{token}/{species}/{filename}.pdf",
            "Downloads a report for a given pathway analysis result")
  }

  ## build GET API request's query
  additional_pars = list(list(number != 25,
                              list("number" = number)),
                         list(resource != "TOTAL",
                              list("resource" = resource)),
                         list(!is.na(token),
                              list("token" = token)),
                         list(resource != "TOTAL",
                              list("resource" = resource)),
                         list(diagram_profile != "Modern",
                              list("diagram_profile" = diagram_profile)),
                         list(fireworks_profile != "Barium Lithium",
                              list("fireworks_profile" = fireworks_profile)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  # create file_path
  save_to = rba_ba_file_path(file_ext = "pdf",
                             file_name = token,
                             dir_name = "rba_reactome",
                             save_to = save_to,
                             verbose = verbose,
                             diagnostics = diagnostics)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("AnalysisService/",
                                                  "report/",
                                                  token, "/",
                                                  species, "/",
                                                  token, ".pdf"),
                                    query = call_query,
                                    httr::accept("application/pdf"),
                                    httr::write_disk(save_to, overwrite = TRUE)
  ))
  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  invisible()
}
#### Download Endpoints ####
#' download Methods to download different views of a result
#'
#' @param token
#' @param request
#' @param save_to
#' @param resource
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis_download = function(token,
                                          request,
                                          save_to = NA,
                                          resource = "TOTAL",
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE){
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = token,
                                         name = "input",
                                         class = "character"),
                                    list(arg = request,
                                         name = "request",
                                         class = "character",
                                         val = c("found_ids",
                                                 "not_found_ids",
                                                 "pathways",
                                                 "results",
                                                 "results_gz")),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character"),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character",
                                         val = c("TOTAL",
                                                 "UNIPROT",
                                                 "ENSEMBL",
                                                 "CHEBI",
                                                 "IUPHAR",
                                                 "MIRBASE",
                                                 "NCBI_PROTEIN",
                                                 "EMBL",
                                                 "COMPOUND",
                                                 "ENTITIES_FDR",
                                                 "PUBCHEM_COMPOUND"))),
                        cond = list(list(grepl("^results|^not_found_ids$", request) &
                                           resource != "TOTAL",
                                         c("You cannot provide 'resource' with ",
                                           request, " request. ignoring resource."))),
                        cond_warning = TRUE,
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("/exporter/diagram/{identifier}.{ext}",
            "Exports a given pathway diagram to the specified image format")
  }

  ## make function-specific calls
  path_input =  paste0("AnalysisService/",
                       "download/",
                       token, "/")
  if (request == "found_ids") {
    output_format = "csv"
    accept_input = "text/csv"
    path_input = paste0(path_input,
                        "entities/found/",
                        resource,  "/",
                        token,
                        ".csv")
  } else if (request == "not_found_ids") {
    output_format = "csv"
    accept_input = "text/csv"
    path_input = paste0(path_input,
                        "entities/notfound/",
                        token,
                        ".csv")
  } else if (request == "pathways") {
    output_format = "csv"
    accept_input = "text/csv"
    path_input = paste0(path_input,
                        "pathways/",
                        resource,  "/",
                        token,
                        ".csv")
  } else if (request == "results") {
    output_format = "json"
    accept_input = "application/json"
    path_input = paste0(path_input,
                        "result.json")
  } else if (request == "results_gz") {
    output_format = "json.gz"
    accept_input = "application/x-gzip"
    path_input = paste0(path_input,
                        "result.json.gz")
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = path_input,
                                    httr::accept(accept_input),
                                    httr::write_disk(save_to, overwrite = TRUE)
  ))

  # create file_path
  save_to = rba_ba_file_path(file_ext = output_format,
                             file_name = paste0(request, "_", token),
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

#### Import Endpoints ####
#' The accepted format is the same as provided by the method
#' /#/download/{token}/result.json. Note: The submitted file can be gzipped.
#'
#' @param input
#' @param input_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis_import = function(input,
                                        input_type = NA,
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = input,
                                         name = "input",
                                         class = "character"),
                                    list(arg = input_type,
                                         name = "input_type",
                                         class = "character",
                                         val = c("file",
                                                 "url"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST /import/form", "Imports the posted json file into the service")
  }
  if (is.na(input_type)) {
    input_type = rba_ba_reactome_input(input = input,
                                       type = input_type,
                                       handle = FALSE,
                                       verbose = verbose,
                                       diagnostics = diagnostics)
  }

  ## make function-specific calls
  ## build POST API request's URL
  if (input_type == "url") {
    path_input = paste0("AnalysisService/",
                        "import/url")
    call_body = input
  } else {
    path_input = paste0("AnalysisService/",
                        "import/form")
    call_body = list(file = httr::upload_file(path = input,
                                              type = "application/json"))
  }
  call_func_input = quote(httr::POST(url = getOption("rba_url_reactome"),
                                     path = path_input,
                                     body = call_body,
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

#### Mapping Endpoints ####
rba_reactome_analysis_mapping = function(input,
                                         input_type = NA,
                                         projection = FALSE,
                                         interactors = FALSE,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = input,
                                         name = "input",
                                         class = c("character",
                                                   "numeric",
                                                   "data.frame",
                                                   "matrix")),
                                    list(arg = input_type,
                                         name = "input_type",
                                         class = "character",
                                         val = c("table",
                                                 "vector",
                                                 "file",
                                                 "url")),
                                    list(arg = projection,
                                         name = "projection",
                                         class = "logical"),
                                    list(arg = interactors,
                                         name = "interactors",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST /mapping/form",
            "Maps the identifiers in the file over the different species",
            "POST /mapping/form/projection",
            "Maps the identifiers in the file over the different species and projects the result to Homo Sapiens")
  }

  ## handle provided input
  input = rba_ba_reactome_input(input = input,
                                type = input_type,
                                handle = TRUE,
                                verbose = verbose,
                                diagnostics = diagnostics)

  ## build POST API request's query
  call_query = list("interactors" = ifelse(interactors, "true", "false"))

  ## build POST API request's URL
  call_body = list(file = httr::upload_file(path = input$file,
                                            type = "text/plain"))

  ## make function-specific calls
  if (input$type == "url") {
    path_input = paste0("AnalysisService/",
                        "mapping/url")
  } else {
    path_input = paste0("AnalysisService/",
                        "mapping/form")
  }
  if (projection == TRUE) {
    path_input = paste0(path_input, "/projection")
  }
  call_func_input = quote(httr::POST(url = getOption("rba_url_reactome"),
                                     path = path_input,
                                     body = call_body,
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

#### Species Endpoints ####
#' Use page and pageSize to reduce the amount of data retrieved. Use sortBy
#' and order to sort the result by your preferred option. The resource field
#' will filter the results to show only those corresponding to the preferred
#' molecule type (TOTAL includes all the different molecules type)
#'
#' @param species_dbid
#' @param sort_by
#' @param order
#' @param resource
#' @param p_value
#' @param min
#' @param max
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis_species = function(species_dbid,
                                         sort_by = "ENTITIES_PVALUE",
                                         order = "ASC",
                                         resource = "TOTAL",
                                         p_value = NA,
                                         min = NA,
                                         max = NA,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species_dbid,
                                         name = "species_dbid",
                                         class = "numeric"),
                                    list(arg = sort_by,
                                         name = "sort_by",
                                         class = "character",
                                         val = c("NAME",
                                                 "TOTAL_ENTITIES",
                                                 "TOTAL_INTERACTORS",
                                                 "TOTAL_REACTIONS",
                                                 "FOUND_ENTITIES",
                                                 "FOUND_INTERACTORS",
                                                 "FOUND_REACTIONS",
                                                 "ENTITIES_RATIO",
                                                 "ENTITIES_PVALUE",
                                                 "ENTITIES_FDR",
                                                 "REACTIONS_RATIO")),
                                    list(arg = order,
                                         name = "order",
                                         class = "character",
                                         val = c("ASC",
                                                 "DESC")),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character",
                                         val = c("TOTAL",
                                                 "UNIPROT",
                                                 "ENSEMBL",
                                                 "CHEBI",
                                                 "IUPHAR",
                                                 "MIRBASE",
                                                 "NCBI_PROTEIN",
                                                 "EMBL",
                                                 "COMPOUND",
                                                 "ENTITIES_FDR",
                                                 "PUBCHEM_COMPOUND")),
                                    list(arg = p_value,
                                         name = "p_value",
                                         class = "numeric"),
                                    list(arg = min,
                                         name = "min",
                                         class = "numeric"),
                                    list(arg = max,
                                         name = "max",
                                         class = "numeric")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /species/homoSapiens/{species}",
            "Compares Homo sapiens to the specified species")
  }
  ## build POST API request's query
  call_query = list("sortBy" = sort_by,
                    "order" = order,
                    "resource" = resource)

  additional_pars = list(list(!is.na(p_value),
                              list("pValue" = p_value)),
                         list(!is.na(min),
                              list("min" = min)),
                         list(!is.na(max),
                              list("max" = max)))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("AnalysisService/",
                                                  "species/homoSapiens/",
                                                  species_dbid),
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

#### Token Endpoints ####
#' Use page and pageSize to reduce the amount of data retrieved. Use
#' sortBy and order to sort the result by your preferred option. The
#' resource field will filter the results to show only those corresponding
#' to the preferred molecule type (TOTAL includes all the different molecules
#' type)
#'
#' @param token
#' @param species
#' @param sort_by
#' @param order
#' @param resource
#' @param p_value
#' @param include_disease
#' @param min
#' @param max
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis_token = function(token,
                                       species,
                                       sort_by = "ENTITIES_PVALUE",
                                       order = "ASC",
                                       resource = "TOTAL",
                                       p_value = NA,
                                       include_disease = TRUE,
                                       min = NA,
                                       max = NA,
                                       verbose = TRUE,
                                       progress_bar = FALSE,
                                       diagnostics = FALSE) {

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = token,
                                         name = "token",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species_dbid",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = sort_by,
                                         name = "sort_by",
                                         class = "character",
                                         val = c("NAME",
                                                 "TOTAL_ENTITIES",
                                                 "TOTAL_INTERACTORS",
                                                 "TOTAL_REACTIONS",
                                                 "FOUND_ENTITIES",
                                                 "FOUND_INTERACTORS",
                                                 "FOUND_REACTIONS",
                                                 "ENTITIES_RATIO",
                                                 "ENTITIES_PVALUE",
                                                 "ENTITIES_FDR",
                                                 "REACTIONS_RATIO")),
                                    list(arg = order,
                                         name = "order",
                                         class = "character",
                                         val = c("ASC",
                                                 "DESC")),
                                    list(arg = resource,
                                         name = "resource",
                                         class = "character",
                                         val = c("TOTAL",
                                                 "UNIPROT",
                                                 "ENSEMBL",
                                                 "CHEBI",
                                                 "IUPHAR",
                                                 "MIRBASE",
                                                 "NCBI_PROTEIN",
                                                 "EMBL",
                                                 "COMPOUND",
                                                 "ENTITIES_FDR",
                                                 "PUBCHEM_COMPOUND")),
                                    list(arg = p_value,
                                         name = "p_value",
                                         class = "numeric"),
                                    list(arg = include_disease,
                                         name = "include_disease",
                                         class = "logical"),
                                    list(arg = min,
                                         name = "min",
                                         class = "numeric"),
                                    list(arg = max,
                                         name = "max",
                                         class = "numeric")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /token/{token}",
            "Returns the result associated with the token")
  }
  ## build POST API request's query
  call_query = list("sortBy" = sort_by,
                    "order" = order,
                    "resource" = resource,
                    "includeDisease" = ifelse(include_disease, "true", "false"))

  additional_pars = list(list(!is.na(p_value),
                              list("pValue" = p_value)),
                         list(!is.na(min),
                              list("min" = min)),
                         list(!is.na(max),
                              list("max" = max)))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_reactome"),
                                    path = paste0("AnalysisService/",
                                                  "token/",
                                                  token),
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
