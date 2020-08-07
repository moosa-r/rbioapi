#### information endpoints in Ensembl API


#' List the names of analyses involved in generating Ensembl data
#'
#' @param species
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_analysis = function(species,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("List the names of analyses involved in generating Ensembl data")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/info/analysis/",
                                                  species),
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

#' List the currently available assemblies for a species, along with toplevel
#' sequences, chromosomes and cytogenetic bands.
#'
#' @param species
#' @param bands
#' @param synonyms
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_aassembly = function(species,
                                      bands = FALSE,
                                      synonyms = FALSE,
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = bands,
                                         name = "bands",
                                         class = "logical"),
                                    list(arg = synonyms,
                                         name = "synonyms",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/assembly/:species")
  }

  ## build GET API request's query
  additional_pars = list(list(bands == TRUE,
                              list("bands" = "1")),
                         list(synonyms == TRUE,
                              list("synonyms" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/info/assembly/",
                                                  species),
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

#' Returns information about the specified toplevel sequence region for the
#' given species.
#'
#' @param species
#' @param region_name
#' @param bands
#' @param synonyms
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_assembly_region_name = function(species,
                                                 region_name,
                                                 bands = FALSE,
                                                 synonyms = FALSE,
                                                 verbose = TRUE,
                                                 progress_bar = FALSE,
                                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = region_name,
                                         name = "region_name",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = bands,
                                         name = "bands",
                                         class = "logical"),
                                    list(arg = synonyms,
                                         name = "synonyms",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/assembly/:species/:region_name")
  }

  ## build GET API request's query
  additional_pars = list(list(bands == TRUE,
                              list("bands" = "1")),
                         list(synonyms == TRUE,
                              list("synonyms" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/info/assembly/",
                                                  species,
                                                  "/",
                                                  region_name),
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

#'  List the functional classifications of gene models that Ensembl associates
#'  with a particular species. Useful for restricting the type of
#'  genes/transcripts retrieved by other endpoints.
#'
#' @param species
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes = function(species,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/biotypes/:species")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/info/biotypes/",
                                                  species),
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

#' Without argument the list of available biotype groups is returned. With
#' :group argument provided, list the properties of biotypes within that group.
#' Object type (gene or transcript) can be provided for filtering.
#'
#' @param group
#' @param object_type
#' @param verbose
#' @param progress_bar
#' @param store_in_options
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes_groups = function(group = NA,
                                            object_type = NA,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = group,
                                         name = "group",
                                         class = "character",
                                         len = 1),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character",
                                         val = c("gene",
                                                 "transcript"))),
                        cond = list(list(!is.na(object_type) &&
                                           is.na(group),
                                         c("You can't provide ",
                                           "'object_type' without ",
                                           "providing 'group'."))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/biotypes/groups/:group/:object_type")
  }

  ## make function-specific calls
  #make path and specify parser type
  if (is.na(group)) {
    parser_type_input = "json->list"
    path_input = "/info/biotypes/groups/"
  } else {
    parser_type_input = "json->df"
    if (is.na(object_type)) {
      path_input = paste0("/info/biotypes/groups/", group)
    } else {
      path_input = paste0("/info/biotypes/groups/", group, "/", object_type)
    }
  }

  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = path_input,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = parser_type_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' List the properties of biotypes with a given name. Object type
#' (gene or transcript) can be provided for filtering.
#'
#' @param name
#' @param object_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes_names = function(name = NA,
                                           object_type = NA,
                                           verbose = TRUE,
                                           progress_bar = FALSE,
                                           diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = name,
                                         name = "name",
                                         class = "character",
                                         len = 1),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character",
                                         val = c("gene",
                                                 "transcript"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/biotypes/name/:name/:object_type")
  }

  ## make function-specific calls
  #make path and specify parser type
  if (is.na(object_type)) {
    path_input = paste0("/info/biotypes/name/", name)
  } else {
    path_input = paste0("/info/biotypes/name/", name, "/", object_type)
  }

  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = path_input,
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

#' List all compara analyses available (an analysis defines the type of
#' comparative data).
#'
#' @param class
#' @param compara
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_compara_methods = function(class = NA,
                                            compara = NA,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = class,
                                         name = "class",
                                         class = "character",
                                         len = 1),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/compara/methods")
  }

  ## build GET API request's query
  additional_pars = list(list(!is.na(class),
                              list("class" = class)),
                         list(!is.na(compara),
                              list("compara" = compara)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/compara/methods/",
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

#' List all collections of species analysed with the specified compara method.
#'
#' @param method
#' @param compara
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_compara_methods = function(method,
                                            compara = NA,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = method,
                                         name = "method",
                                         class = "character",
                                         len = 1),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/compara/species_sets/:method")
  }

  ## build GET API request's query
  additional_pars = list(list(!is.na(compara),
                              list("compara" = compara)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/compara/species_sets/",
                                                  method),
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

#' Lists all available comparative genomics databases and their data release.
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_comparas = function(verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/comparas")
  }

  ## build GET API request's query

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/comparas",
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

#' Shows the data releases available on this REST server. May return more than
#' one release (unfrequent non-standard Ensembl configuration).
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_data = function(verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/data")
  }

  ## build GET API request's query

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/data",
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

#' Returns the Ensembl Genomes version of the databases backing this service
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_eg_version = function(verbose = TRUE,
                                       progress_bar = FALSE,
                                       diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/eg_version")
  }

  ## build GET API request's query

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/eg_version",
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

#' Lists all available external sources for a species.
#'
#' @param species
#' @param filter
#' @param feature
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_external_dbs = function(species,
                                         filter = NA,
                                         feature = NA,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = filter,
                                         name = "filter",
                                         class = "character",
                                         len = 1),
                                    list(arg = feature,
                                         name = "compara",
                                         class = "character",
                                         len = 1,
                                         val = c("dna_align_feature",
                                                 "protein_align_feature",
                                                 "unmapped_object",
                                                 "xref",
                                                 "seq_region_synonym"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/external_dbs/:species")
  }

  ## build GET API request's query
  additional_pars = list(list(!is.na(filter),
                              list("filter" = filter)),
                         list(!is.na(feature),
                              list("feature" = feature)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/external_dbs/",
                                                  species),
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

#' Get list of all Ensembl divisions for which information is available
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_divisions = function(verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/divisions")
  }

  ## build GET API request's query

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/divisions",
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->chr",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Find information about a given genome
#'
#' @param genome_name
#' @param expand
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes = function(genome_name,
                                    expand = FALSE,
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = genome_name,
                                         name = "genome_name",
                                         class = "character",
                                         len = 1),
                                    list(arg = expand,
                                         name = "expand",
                                         class = "logical",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/genomes/:genome_name")
  }

  ## build GET API request's query
  additional_pars = list(list(expand == TRUE,
                              list("expand" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/genomes/",
                                                  genome_name),
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

#' Find information about genomes containing a specified INSDC accession
#'
#' @param accession
#' @param expand
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_accession = function(accession,
                                              expand = FALSE,
                                              verbose = TRUE,
                                              progress_bar = FALSE,
                                              diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         len = 1),
                                    list(arg = expand,
                                         name = "expand",
                                         class = "logical",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/genomes/accession/:accession")
  }

  ## build GET API request's query
  additional_pars = list(list(expand == TRUE,
                              list("expand" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/genomes/accession/",
                                                  accession),
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

#' Find information about a genome with a specified assembly
#'
#' @param assembly_id
#' @param expand
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_assembly = function(assembly_id,
                                             expand = FALSE,
                                             verbose = TRUE,
                                             progress_bar = FALSE,
                                             diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = assembly_id,
                                         name = "assembly_id",
                                         class = "character",
                                         len = 1),
                                    list(arg = expand,
                                         name = "expand",
                                         class = "logical",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/genomes/assembly/:assembly_id")
  }

  ## build GET API request's query
  additional_pars = list(list(expand == TRUE,
                              list("expand" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/genomes/assembly/",
                                                  assembly_id),
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

#' Find information about all genomes in a given division.
#'
#' @param division_name
#' @param expand
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_division = function(division_name,
                                             expand = FALSE,
                                             verbose = TRUE,
                                             progress_bar = FALSE,
                                             diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = division_name,
                                         name = "division_name",
                                         class = "character",
                                         len = 1),
                                    list(arg = expand,
                                         name = "expand",
                                         class = "logical",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/genomes/division/:division_name")
  }

  ## build GET API request's query
  additional_pars = list(list(expand == TRUE,
                              list("expand" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/genomes/division/",
                                                  division_name),
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

#' Find information about all genomes beneath a given node of the taxonomy
#'
#' @param taxon_name
#' @param expand
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_taxonomy = function(taxon_name,
                                             expand = FALSE,
                                             verbose = TRUE,
                                             progress_bar = FALSE,
                                             diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = taxon_name,
                                         name = "taxon_name",
                                         class = c("character",
                                                   "numeric"),
                                         len = 1),
                                    list(arg = expand,
                                         name = "expand",
                                         class = "logical",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/genomes/taxonomy/:taxon_name")
  }

  ## build GET API request's query
  additional_pars = list(list(expand == TRUE,
                              list("expand" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/genomes/taxonomy/",
                                                  taxon_name),
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

#'  Shows the current version of the Ensembl REST API.
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_rest = function(verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/rest")
  }

  ## build GET API request's query

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/rest",
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

#' Shows the current version of the Ensembl API used by the REST server.
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_software = function(verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/software")
  }

  ## build GET API request's query

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/software",
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

#' Lists all available species, their aliases, available adaptor groups and data release.
#'
#' @param division
#' @param hide_strain_info
#' @param strain_collection
#' @param expand
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_species = function(division = NA,
                                    hide_strain_info = FALSE,
                                    strain_collection = NA,
                                    expand = FALSE,
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = division,
                                         name = "expand",
                                         class = "character",
                                         len = 1),
                                    list(arg = hide_strain_info,
                                         name = "hide_strain_info",
                                         class = "logical"),
                                    list(arg = strain_collection,
                                         name = "strain_collection",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/species")
  }

  ## build GET API request's query
  additional_pars = list(list(!is.na(division),
                              list("division" = division)),
                         list(hide_strain_info == TRUE,
                              list("hide_strain_info" = 1)),
                         list(!is.na(strain_collection),
                              list("strain_collection" = strain_collection)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/species",
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

#' List the variation sources used in Ensembl for a species.
#'
#' @param species
#' @param filter
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_species = function(species,
                                              filter = NA,
                                              verbose = TRUE,
                                              progress_bar = FALSE,
                                              diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"),
                                         len = 1),
                                    list(arg = filter,
                                         name = "filter",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/variation/:species")
  }

  ## build GET API request's query
  additional_pars = list(list(!(is.na(filter)),
                              list("filter" = filter)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/variation/",
                                                  species),
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

#' Lists all variant consequence types.
#'
#' @param rank
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_consequence_types = function(rank = FALSE,
                                                        verbose = TRUE,
                                                        progress_bar = FALSE,
                                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = rank,
                                         name = "rank",
                                         class = "logical",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/variation/consequence_types")
  }

  ## build GET API request's query
  additional_pars = list(list(rank == TRUE,
                              list("rank" = 1)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = "info/variation/consequence_types",
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

#' List all populations for a species
#'
#' @param species
#' @param filter
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_populations = function(species,
                                                  filter = NA,
                                                  verbose = TRUE,
                                                  progress_bar = FALSE,
                                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"),
                                         len = 1),
                                    list(arg = filter,
                                         name = "filter",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/variation/populations/:species")
  }

  ## build GET API request's query
  additional_pars = list(list(!(is.na(filter)),
                              list("filter" = filter)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/variation/populations/",
                                                  species),
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

#' List all individuals for a population from a species
#'
#' @param species
#' @param population_name
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_populations_species = function(species,
                                                          population_name,
                                                          verbose = TRUE,
                                                          progress_bar = FALSE,
                                                          diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"),
                                         len = 1),
                                    list(arg = population_name,
                                         name = "population_name",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET info/variation/populations/:species:/:population_name")
  }

  ## build GET API request's query

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("info/variation/populations/",
                                                  species, "/",
                                                  population_name),
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

#' A wrapper around all info/variation/... endpoints
#'
#' @param species
#' @param consequence_types
#' @param consequence_rank
#' @param populations
#' @param species_filter
#' @param populations_filter
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation = function(species = NA,
                                      consequence_types = FALSE,
                                      consequence_rank = FALSE,
                                      populations = FALSE,
                                      species_filter = NA,
                                      populations_filter = NA,
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"),
                                         len = 1),
                                    list(arg = consequence_types,
                                         name = "consequence_types",
                                         class = "logical",
                                         len = 1),
                                    list(arg = consequence_rank,
                                         name = "consequence_rank",
                                         class = "logical",
                                         len = 1),
                                    list(arg = populations,
                                         name = "populations",
                                         class = c("character",
                                                   "logical"),
                                         len = 1),
                                    list(arg = species_filter,
                                         name = "species_filter",
                                         class = "character",
                                         len = 1),
                                    list(arg = populations_filter,
                                         name = "populations_filter",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))
  ## Decide which function to call
  if (consequence_types == TRUE) {
    if (verbose == TRUE && !is.na(species)) {
      message("Ignoring: 'species' & 'populations' arguments.")}
    final_output = rba_ensembl_info_variation_consequence_types(rank = consequence_rank,
                                                                verbose = verbose,
                                                                progress_bar = progress_bar,
                                                                diagnostics = diagnostics)
  } else {
    if (!is.na(species) & populations == FALSE) {
      final_output = rba_ensembl_info_variation_species(species = species,
                                                        filter = species_filter,
                                                        verbose = verbose,
                                                        progress_bar = progress_bar,
                                                        diagnostics = diagnostics)
    } else if (!is.na(species) & populations == TRUE) {
      final_output = rba_ensembl_info_variation_populations(species = species,
                                                            filter = populations_filter,
                                                            verbose = verbose,
                                                            progress_bar = progress_bar,
                                                            diagnostics = diagnostics)
    } else if (!is.na(species) & class(populations) == "character") {
      final_output = rba_ensembl_info_variation_populations_species(species = species,
                                                                    population_name = populations,
                                                                    verbose = verbose,
                                                                    progress_bar = progress_bar,
                                                                    diagnostics = diagnostics)
    } else {
      stop("Wrong combination of provided arguments, ",
           "Consult this function's documentations.", call. = diagnostics)
    }
  }
  return(final_output)


}
