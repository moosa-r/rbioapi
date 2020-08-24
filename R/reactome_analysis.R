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
                                 verbose = TRUE,
                                 progress_bar = FALSE,
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
    write.table(x = input,
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
                                 species = NA,
                                 projection = FALSE,
                                 interactors = FALSE,
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
                                  response_parser = NULL,
                                  parser_type = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}
