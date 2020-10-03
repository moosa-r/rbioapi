#### Internal functions ####
#' internal function to feed input to Reactome analysis
#'
#' @param input
#' @param type
#' @param handle
#'
#' @return
#' @export
#' @examples
rba_ba_reactome_input = function(input,
                                 type = NA,
                                 handle = TRUE){
  diagnostics = ifelse(exists("diagnostics", envir = parent.frame(1)),
                       eval(parse(text = "diagnostics"), envir = parent.frame(1)),
                       getOption("rba_diagnostics"))
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
          grepl(pattern = "^[a-zA-z]:|^\\\\\\w|^/|\\w+\\.\\w+$",
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
#' @param ...
#' @param max
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
                                 ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character",
                                         "numeric",
                                         "data.frame",
                                         "matrix")),
                          list(arg = "input_type",
                               class = "character",
                               val = c("table",
                                       "vector",
                                       "file",
                                       "url")),
                          list(arg = "projection",
                               class = "logical"),
                          list(arg = "interactors",
                               class = "logical"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "sort_by",
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
                          list(arg = "order",
                               class = "character",
                               val = c("ASC",
                                       "DESC")),
                          list(arg = "resource",
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
                          list(arg = "p_value",
                               class = "numeric"),
                          list(arg = "include_disease",
                               class = "logical"),
                          list(arg = "min",
                               class = "numeric"),
                          list(arg = "max",
                               class = "numeric")),
              cond = list(list("sum(projection, !is.na(species)) == 2",
                               "You cannot Provide 'species' when 'projection' argument is TRUE"))
  )

  v_msg("POST /identifiers/form",
        "Analyse the identifiers in the file over the different species",
        "POST /identifiers/url",
        "Analyse the identifiers contained in the provided url over the different species",
        "POST/identifiers/form/projection",
        "Analyse the identifiers in the file over the different species and projects the result to Homo Sapiens",
        "POST /identifiers/url/projection",
        "Analyse the identifiers contained in the provided url over the different species and projects the result to Homo Sapiens")

  ## handle provided input
  input = rba_ba_reactome_input(input = input,
                                type = input_type,
                                handle = TRUE)

  ## Build POST API Request's query
  call_query = list("interactors" = ifelse(interactors, "true", "false"),
                    "sortBy" = sort_by,
                    "order" = order,
                    "resource" = resource,
                    "includeDisease" = ifelse(include_disease, "true", "false"))

  call_query = rba_ba_query(init = call_query,
                            list("species",
                                 !is.na(species),
                                 species),
                            list("pValue",
                                 !is.na(p_value),
                                 p_value),
                            list("min",
                                 !is.na(min),
                                 min),
                            list("max",
                                 !is.na(max),
                                 max))
  ## Build POST API Request's URL
  call_body = list(file = httr::upload_file(path = input$file,
                                            type = "text/plain"))

  ## Build Function-Specific Call
  path_input = sprintf("%sidentifiers/%s",
                       rba_ba_stg("reactome", "pth", "analysis"),
                       ifelse(input$type == "url",
                              yes = "url",
                              no = "form"))

  if (projection == TRUE) {
    path_input = paste0(path_input, "/projection")
  }

  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           body = call_body,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("reactome_analysis.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param fireworks_profile
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
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "token",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "save_to",
                               class = "character"),
                          list(arg = "number",
                               class = "numeric"),
                          list(arg = "resource",
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
                          list(arg = "diagram_profile",
                               class = "character",
                               val = c("Modern",
                                       "Standard")),
                          list(arg = "analysis_profile",
                               class = "character",
                               val = c("Standard",
                                       "Strosobar",
                                       "Copper Plus")),
                          list(arg = "fireworks_profile",
                               class = "character",
                               val = c("Copper",
                                       "Copper Plus",
                                       "Barium Lithium",
                                       "calcium salts"))))

  v_msg("GET /report/{token}/{species}/{filename}.pdf",
        "Downloads a report for a given pathway analysis result")


  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("number",
                                 number != 25,
                                 number),
                            list("resource",
                                 resource != "TOTAL",
                                 resource),
                            list("token",
                                 !is.na(token),
                                 token),
                            list("diagramProfile",
                                 diagram_profile != "Modern",
                                 diagram_profile),
                            list("analysisProfile",
                                 analysis_profile != "Standard",
                                 analysis_profile),
                            list("fireworksProfile",
                                 fireworks_profile != "Barium Lithium",
                                 fireworks_profile))

  # create file_path
  save_to = rba_ba_file(file = paste0(token, ".pdf"),
                        save_to = ifelse(is.na(save_to),
                                         yes = TRUE,
                                         no = save_to))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = sprintf("%sreport/%s/%s/%s.pdf",
                                          rba_ba_stg("reactome", "pth", "analysis"),
                                          token, species, token),
                           query = call_query,
                           accept = "application/pdf",
                           parser = NULL,
                           save_to = save_to)
  ## Call API
  invisible(rba_ba_skeleton(input_call))
}

#### Download Endpoints ####
#' download Methods to download different views of a result
#'
#' @param token
#' @param request
#' @param save_to
#' @param ...
#' @param resource
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis_download = function(token,
                                          request,
                                          save_to = NA,
                                          resource = "TOTAL",
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "token",
                               class = "character"),
                          list(arg = "request",
                               class = "character",
                               val = c("found_ids",
                                       "not_found_ids",
                                       "pathways",
                                       "results",
                                       "results_gz")),
                          list(arg = "save_to",
                               class = "character"),
                          list(arg = "resource",
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
              cond = list(list('grepl("^results|^not_found_ids$", request) & resource != "TOTAL"',
                               c("You cannot provide 'resource' with ",
                                 request, " request. ignoring resource."))),
              cond_warning = TRUE)

  v_msg("/exporter/diagram/{identifier}.{ext}",
        "Exports a given pathway diagram to the specified image format")

  ## Build Function-Specific Call
  path_input = sprintf("%sdownload/%s/",
                       rba_ba_stg("reactome", "pth", "analysis"),
                       token)
  output_format = "csv"
  accept_input = "text/csv"

  if (request == "found_ids") {
    path_input = sprintf("%sentities/found/%s/%s.csv",
                         path_input, resource, token)
  } else if (request == "not_found_ids") {
    path_input = sprintf("%sentities/notfound/%s.csv",
                         path_input, token)
  } else if (request == "pathways") {
    path_input = sprintf("%sentities/pathways/%s/%s.csv",
                         path_input, resource, token)

  } else if (request == "results") {
    output_format = "json"
    accept_input = "application/json"
    path_input = paste0(path_input, "result.json")
  } else if (request == "results_gz") {
    output_format = "json.gz"
    accept_input = "application/x-gzip"
    path_input = paste0(path_input, "result.json.gz")
  }

  # create file_path
  save_to = rba_ba_file(file = paste0(request, "_", token, ".", output_format),
                        save_to = ifelse(is.na(save_to),
                                         yes = TRUE,
                                         no = save_to))

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           accept = accept_input,
                           save_to = save_to,
                           parser = NULL)

  ## Call API
  invisible(rba_ba_skeleton(input_call))
}

#### Import Endpoints ####
#' The accepted format is the same as provided by the method
#' /#/download/{token}/result.json. Note: The submitted file can be gzipped.
#'
#' @param input
#' @param ...
#' @param input_type
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis_import = function(input,
                                        input_type = NA,
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = "character"),
                          list(arg = "input_type",
                               class = "character",
                               val = c("file",
                                       "url"))))

  v_msg("POST /import/form",
        "Imports the posted json file into the service")

  input_type = rba_ba_reactome_input(input = input,
                                     type = input_type,
                                     handle = FALSE)

  ## Build Function-Specific Call
  ## Build POST API Request's URL
  if (input_type == "url") {
    path_input = paste0(rba_ba_stg("reactome", "pth", "analysis"),
                        "import/url")
    call_body = input
  } else {
    path_input = paste0(rba_ba_stg("reactome", "pth", "analysis"),
                        "import/form")
    call_body = list(file = httr::upload_file(path = input,
                                              type = "application/json"))
  }
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           body = call_body,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("reactome_analysis_import.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Mapping Endpoints ####
#' Maps the identifiers over the different species
#'
#' @param input
#' @param input_type
#' @param projection
#' @param interactors
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_reactome_analysis_mapping = function(input,
                                         input_type = NA,
                                         projection = FALSE,
                                         interactors = FALSE,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character",
                                         "numeric",
                                         "data.frame",
                                         "matrix")),
                          list(arg = "input_type",
                               class = "character",
                               val = c("table",
                                       "vector",
                                       "file",
                                       "url")),
                          list(arg = "projection",
                               class = "logical"),
                          list(arg = "interactors",
                               class = "logical")))

  v_msg("POST /mapping/form",
        "Maps the identifiers in the file over the different species",
        "POST /mapping/form/projection",
        "Maps the identifiers in the file over the different species and projects the result to Homo Sapiens")


  ## handle provided input
  input = rba_ba_reactome_input(input = input,
                                type = input_type,
                                handle = TRUE)

  ## Build POST API Request's query
  call_query = list("interactors" = ifelse(interactors, "true", "false"))

  ## Build POST API Request's URL
  call_body = list(file = httr::upload_file(path = input$file,
                                            type = "text/plain"))

  ## Build Function-Specific Call
  path_input = sprintf("%smapping/%s",
                       rba_ba_stg("reactome", "pth", "analysis"),
                       ifelse(input$type == "url",
                              yes = "url",
                              no = "form"))
  if (projection == TRUE) {
    path_input = paste0(path_input, "/projection")
  }

  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("reactome", "url"),
                           path = path_input,
                           body = call_body,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("reactome_analysis_mapping.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
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
#' @param ...
#' @param max
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
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species_dbid",
                               class = "numeric"),
                          list(arg = "sort_by",
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
                          list(arg = "order",
                               class = "character",
                               val = c("ASC",
                                       "DESC")),
                          list(arg = "resource",
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
                          list(arg = "p_value",
                               class = "numeric"),
                          list(arg = "min",
                               class = "numeric"),
                          list(arg = "max",
                               class = "numeric")))

  v_msg("GET /species/homoSapiens/{species}",
        "Compares Homo sapiens to the specified species")

  ## Build POST API Request's query
  call_query = list("sortBy" = sort_by,
                    "order" = order,
                    "resource" = resource)

  call_query = rba_ba_query(init = call_query,
                            list("pValue",
                                 !is.na(p_value),
                                 p_value),
                            list("min",
                                 !is.na(min),
                                 min),
                            list("max",
                                 !is.na(max),
                                 max))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("reactome", "url"),
                           path = paste0(rba_ba_stg("reactome", "pth", "analysis"),
                                         "species/homoSapiens/",
                                         species_dbid),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("reactome_analysis_species.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
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
#' @param ...
#' @param max
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
                                       ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "token",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "sort_by",
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
                          list(arg = "order",
                               class = "character",
                               val = c("ASC",
                                       "DESC")),
                          list(arg = "resource",
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
                          list(arg = "p_value",
                               class = "numeric"),
                          list(arg = "include_disease",
                               class = "logical"),
                          list(arg = "min",
                               class = "numeric"),
                          list(arg = "max",
                               class = "numeric")))

  v_msg("GET /token/{token}",
        "Returns the result associated with the token")

  ## Build POST API Request's query
  call_query = list("sortBy" = sort_by,
                    "order" = order,
                    "resource" = resource,
                    "includeDisease" = ifelse(include_disease, "true", "false"))

  call_query = rba_ba_query(init = call_query,
                            list("pValue",
                                 !is.na(p_value),
                                 p_value),
                            list("min",
                                 !is.na(min),
                                 min),
                            list("max",
                                 !is.na(max),
                                 max))

  ## Build Function-Specific Call
  input_call =  rba_ba_httr(httr = "get",
                            url = rba_ba_stg("reactome", "url"),
                            path = paste0(rba_ba_stg("reactome", "pth", "analysis"),
                                          "token/",
                                          token),
                            query = call_query,
                            accept = "application/json",
                            parser = "json->list",
                            save_to = rba_ba_file("reactome_analysis_token.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
