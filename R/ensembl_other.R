#### Archive Endpoints ####

#' Retrieve the latest version for a set of identifiers
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
rba_ensembl_archive = function(ids,
                               verbose = TRUE,
                               progress_bar = FALSE,
                               diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ids,
                                         name = "ids",
                                         class = "character",
                                         max_len = 1000)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST archive/id")
  }

  ## build POST API request's URL
  call_body = jsonlite::toJSON(list("id" = as.array(ids)))

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_ensembl"),
                                     path = "archive/id",
                                     body = call_body,
                                     httr::accept_json(),
                                     httr::content_type("application/json")
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

#### Cross References Endpoints ####

#' Looks up an external symbol and returns all Ensembl objects linked to it.
#' This can be a display name for a gene/transcript/translation, a synonym or
#' an externally linked reference. If a gene's transcript is linked to the
#' supplied symbol the service will return both gene and transcript
#' (it supports transient links).
#'
#' @param species
#' @param db_type
#' @param external_db
#' @param object_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#' @param external_symbol
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_symbol = function(external_symbol,
                                    species,
                                    db_type = NA,
                                    external_db = NA,
                                    object_type = NA,
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = external_symbol,
                                         name = "external_symbol",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET xrefs/symbol/:species/:symbol")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("xrefs/symbol/",
                                                  species, "/",
                                                  external_symbol),
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

#' Perform lookups of Ensembl Identifiers and retrieve their external
#' references in other databases
#'
#' @param ensembl_id
#' @param species
#' @param all_levels
#' @param db_type
#' @param external_db
#' @param object_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_id = function(ensembl_id,
                                species = NA,
                                all_levels = FALSE,
                                db_type = NA,
                                external_db = NA,
                                object_type = NA,
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character",
                                         len = 1),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = all_levels,
                                         name = "all_levels",
                                         class = "logical"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET xrefs/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(all_levels == TRUE,
                              list("all_levels" = "1")),
                         list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("xrefs/id/",
                                                  ensembl_id),
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

#' Performs a lookup based upon the primary accession or display label of an
#' external reference and returning the information we hold about the entry
#'
#' @param name
#' @param species
#' @param all_levels
#' @param db_type
#' @param external_db
#' @param object_type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_name = function(name,
                                  species,
                                  all_levels = FALSE,
                                  db_type = NA,
                                  external_db = NA,
                                  object_type = NA,
                                  verbose = TRUE,
                                  progress_bar = FALSE,
                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = name,
                                         name = "name",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"),
                                         len = 1),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET xrefs/name/:species/:name")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("xrefs/name/",
                                                  species, "/",
                                                  name),
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

#### EQTL Endpoints ####

#' Returns all tissues currently available in the DB
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
rba_ensembl_eqtl_tissue = function(species,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET eqtl/tissue/:species/")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("eqtl/tissue/",
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

#' Returns the p-value for each SNP in a given gene
#'
#' @param stable_id
#' @param species
#' @param statistic
#' @param tissue
#' @param variant_name
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_eqtl_gene = function(stable_id,
                                 species,
                                 statistic = NA,
                                 tissue = NA,
                                 variant_name = NA,
                                 verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = stable_id,
                                         name = "stable_id",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = statistic,
                                         name = "statistic",
                                         class = "character"),
                                    list(arg = tissue,
                                         name = "tissue",
                                         class = "character"),
                                    list(arg = variant_name,
                                         name = "variant_name",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET eqtl/stable_id/:species/:stable_id")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(statistic),
                              list("statistic" = statistic)),
                         list(!is.na(tissue),
                              list("tissue" = tissue)),
                         list(!is.na(variant_name),
                              list("variant_name" = variant_name)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("eqtl/id/",
                                                  species, "/", stable_id),
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

#' Returns the p-values for a SNP (e.g. rs123)
#'
#' @param variant_name
#' @param species
#' @param stable_id
#' @param statistic
#' @param tissue
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_eqtl_variant_name = function(variant_name,
                                         species,
                                         stable_id = NA,
                                         statistic = NA,
                                         tissue = NA,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = variant_name,
                                         name = "variant_name",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = stable_id,
                                         name = "stable_id",
                                         class = "character"),
                                    list(arg = statistic,
                                         name = "statistic",
                                         class = "character"),
                                    list(arg = tissue,
                                         name = "tissue",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET eqtl/variant_name/:species/:variant_name")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(stable_id),
                              list("stable_id" = stable_id)),
                         list(!is.na(statistic),
                              list("statistic" = statistic)),
                         list(!is.na(tissue),
                              list("tissue" = tissue)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("eqtl/variant_name/",
                                                  species, "/", variant_name),
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

#### Linkage Disequilibrium Endpoints ####

#' Computes and returns LD values between the given variant and all other
#' variants in a window centered around the given variant. The window size
#' is set to 500 kb.
#'
#' @param variant_id
#' @param species
#' @param population_name
#' @param attribs
#' @param d_prime
#' @param r2
#' @param window_size
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ld_variants = function(variant_id,
                                   species,
                                   population_name,
                                   attribs = FALSE,
                                   d_prime = NA,
                                   r2 = NA,
                                   window_size = 500,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = variant_id,
                                         name = "variant_id",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = population_name,
                                         name = "population_name",
                                         class = "character"),
                                    list(arg = attribs,
                                         name = "attribs",
                                         class = "logical"),
                                    list(arg = d_prime,
                                         name = "d_prime",
                                         class = "numeric"),
                                    list(arg = r2,
                                         name = "r2",
                                         class = "numeric"),
                                    list(arg = window_size,
                                         name = "window_size",
                                         class = "numeric")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET ld/:species/:id/:population_name")
  }
  ## build GET API request's query
  additional_pars = list(list(attribs == TRUE,
                              list("attribs" = 1)),
                         list(!is.na(d_prime),
                              list("d_prime" = d_prime)),
                         list(!is.na(r2),
                              list("r2" = r2)),
                         list(window_size != 500,
                              list("window_size" = as.integer(window_size))))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("ld/",
                                                  species, "/",
                                                  variant_id, "/",
                                                  population_name),
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

#' Computes and returns LD values between the given variants
#'
#' @param variant_id_1
#' @param variant_id_2
#' @param species
#' @param population_name
#' @param d_prime
#' @param r2
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ld_pairwise = function(variant_id_1,
                                   variant_id_2,
                                   species,
                                   population_name = NA,
                                   d_prime = NA,
                                   r2 = NA,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = variant_id_1,
                                         name = "variant_id_1",
                                         class = "character"),
                                    list(arg = variant_id_2,
                                         name = "variant_id_2",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = population_name,
                                         name = "population_name",
                                         class = "character"),
                                    list(arg = d_prime,
                                         name = "d_prime",
                                         class = "numeric"),
                                    list(arg = r2,
                                         name = "r2",
                                         class = "numeric")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET ld/:species/pairwise/:id1/:id2")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(population_name),
                              list("population_name" = population_name)),
                         list(!is.na(d_prime),
                              list("d_prime" = d_prime)),
                         list(!is.na(r2),
                              list("r2" = r2)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("ld/",
                                                  species, "/pairwise/",
                                                  variant_id_1, "/",
                                                  variant_id_2),
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

#' Computes and returns LD values between all pairs of variants in the defined
#' region
#'
#' @param region
#' @param population_name
#' @param species
#' @param d_prime
#' @param r2
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ld_region = function(region,
                                 population_name,
                                 species,
                                 d_prime = NA,
                                 r2 = NA,
                                 verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = region,
                                         name = "region",
                                         class = "character"),
                                    list(arg = population_name,
                                         name = "population_name",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = d_prime,
                                         name = "d_prime",
                                         class = "numeric"),
                                    list(arg = r2,
                                         name = "r2",
                                         class = "numeric")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET ld/:species/region/:region/:population_name")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(d_prime),
                              list("d_prime" = d_prime)),
                         list(!is.na(r2),
                              list("r2" = r2)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("ld/",
                                                  species, "/region/",
                                                  region, "/",
                                                  population_name),
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

#### Lookup Endpoints ####

#' Find the species and database for several identifiers.
#' IDs that are not found are returned with no data
#'
#' @param ids
#' @param db_type
#' @param expand
#' @param format
#' @param object_type
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_lookup_id = function(ids,
                         db_type = NA,
                         expand = FALSE,
                         format = NA,
                         object_type = NA,
                         species = NA,
                         verbose = TRUE,
                         progress_bar = FALSE,
                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ids,
                                         name = "ids",
                                         class = "character",
                                         max_len = 1000),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = expand,
                                         name = "expand",
                                         class = "logical"),
                                    list(arg = format,
                                         name = "format",
                                         class = "character"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"))),

                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST lookup/id")
  }

  ## build POST API request's query
  additional_pars = list(list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(expand == TRUE,
                              list("expand" = "1")),
                         list(!is.na(format),
                              list("format" = format)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)),
                         list(!is.na(species),
                              list("species" = species)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## build POST API request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_ensembl"),
                                     path = "lookup/id",
                                     body = call_body,
                                     query = call_query,
                                     httr::accept_json(),
                                     httr::content_type("application/json")
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

#' Find the species and database for a set of symbols in a linked external
#' database. Unknown symbols are omitted from the response
#'
#' @param symbols
#' @param species
#' @param expand
#' @param format
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_lookup_symbol = function(symbols,
                             species,
                             expand = FALSE,
                             format = NA,
                             verbose = TRUE,
                             progress_bar = FALSE,
                             diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = symbols,
                                         name = "symbols",
                                         class = "character",
                                         max_len = 1000),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = expand,
                                         name = "expand",
                                         class = "logical"),
                                    list(arg = format,
                                         name = "format",
                                         class = "character")),

                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST lookup/symbol/:species/:symbol")
  }

  ## build POST API request's query
  additional_pars = list(list(expand == TRUE,
                              list("expand" = "1")),
                         list(!is.na(format),
                              list("format" = format)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## build POST API request's URL
  call_body = jsonlite::toJSON(list("symbols" = as.array(symbols)))

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_ensembl"),
                                     path = paste0("/lookup/symbol/",
                                                   species),
                                     body = call_body,
                                     query = call_query,
                                     httr::accept_json(),
                                     httr::content_type("application/json")
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

#### Mapping Endpoints ####

#' Convert from cDNA coordinates to genomic coordinates. Output reflects
#' forward orientation coordinates as returned from the Ensembl API
#'
#' @param ensembl_id
#' @param region
#' @param species
#' @param include_original_region
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_map_cdna = function(ensembl_id,
                                region,
                                species = NA,
                                include_original_region = FALSE,
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character"),
                                    list(arg = region,
                                         name = "region",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = include_original_region,
                                         name = "include_original_region",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET map/cdna/:id/:region")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(include_original_region = TRUE,
                              list("include_original_region" = include_original_region)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("map/cdna/",
                                                  ensembl_id, "/",
                                                  region),
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

#' Convert from CDS coordinates to genomic coordinates. Output reflects
#' forward orientation coordinates as returned from the Ensembl API.
#'
#' @param ensembl_id
#' @param region
#' @param species
#' @param include_original_region
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_map_cds = function(ensembl_id,
                               region,
                               species = NA,
                               include_original_region = FALSE,
                               verbose = TRUE,
                               progress_bar = FALSE,
                               diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character"),
                                    list(arg = region,
                                         name = "region",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = include_original_region,
                                         name = "include_original_region",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET map/cds/:id/:region")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(include_original_region = TRUE,
                              list("include_original_region" = include_original_region)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("map/cds/",
                                                  ensembl_id, "/",
                                                  region),
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

#' Convert the co-ordinates of one assembly to another
#'
#' @param asm_one
#' @param asm_two
#' @param region
#' @param species
#' @param coord_system
#' @param target_coord_system
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_map_assembly = function(asm_one,
                                    asm_two,
                                    region,
                                    species,
                                    coord_system = NA,
                                    target_coord_system = NA,
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = asm_one,
                                         name = "asm_one",
                                         class = "character"),
                                    list(arg = asm_two,
                                         name = "asm_two",
                                         class = "character"),
                                    list(arg = region,
                                         name = "region",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = coord_system,
                                         name = "coord_system",
                                         class = "character"),
                                    list(arg = target_coord_system,
                                         name = "target_coord_system",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET map/:species/:asm_one/:region/:asm_two")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(coord_system),
                              list("coord_system" = coord_system)),
                         list(target_coord_system = TRUE,
                              list("include_original_region" = target_coord_system)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("map/",
                                                  species, "/",
                                                  asm_one, "/",
                                                  region, "/",
                                                  asm_two),
                                    query = call_query,
                                    httr::accept_json()
  ))

  resp_parser = quote(as.list(jsonlite::fromJSON(httr::content(response,
                                                               as = "text",
                                                               encoding = "UTF-8"),
                                                 simplifyVector = FALSE)
  ))
  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = resp_parser,
                                  parser_type = NA,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Convert from protein (translation) coordinates to genomic coordinates.
#' Output reflects forward orientation coordinates as returned from the Ensembl API
#'
#' @param ensembl_id
#' @param region
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_map_translation = function(ensembl_id,
                                       region,
                                       species = NA,
                                       verbose = TRUE,
                                       progress_bar = FALSE,
                                       diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character"),
                                    list(arg = region,
                                         name = "region",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("numeric",
                                                   "character"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET map/translation/:id/:region")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(species),
                              list("species" = species)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("map/translation/",
                                                  ensembl_id, "/",
                                                  region),
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

#### Ontologies Endpoints ####

#' Reconstruct the entire ancestry of a term from is_a and part_of relationships
#'
#' @param term_id
#' @param chart
#' @param ontology
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_ancestors = function(term_id,
                                          chart = FALSE,
                                          ontology = NA,
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = term_id,
                                         name = "term_id",
                                         class = "character"),
                                    list(arg = chart,
                                         name = "chart",
                                         class = "logical"),
                                    list(arg = ontology,
                                         name = "ontology",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET ontology/ancestors/chart/:id \r\n",
            "GET ontology/ancestors/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(ontology),
                              list("ontology" = ontology)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  if (chart == TRUE) {
    path_input = paste0("ontology/ancestors/chart/",
                        term_id)
    parser_input = "json->list"
  } else {
    path_input = paste0("ontology/ancestors/",
                        term_id)
    parser_input = "json->df"
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = path_input,
                                    query = call_query,
                                    httr::accept_json()
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

#' Find all the terms descended from a given term. By default searches are
#' conducted within the namespace of the given identifier
#'
#' @param term_id
#' @param closest_term
#' @param ontology
#' @param subset
#' @param zero_distance
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_descendants = function(term_id,
                                            closest_term = FALSE,
                                            ontology = NA,
                                            subset = NA,
                                            zero_distance = NA,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = term_id,
                                         name = "term_id",
                                         class = "character"),
                                    list(arg = closest_term,
                                         name = "closest_term",
                                         class = "logical"),
                                    list(arg = ontology,
                                         name = "ontology",
                                         class = "character"),
                                    list(arg = subset,
                                         name = "subset",
                                         class = "character"),
                                    list(arg = zero_distance,
                                         name = "zero_distance",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET ontology/descendants/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(closest_term = TRUE,
                              list("closest_term" = "1")),
                         list(!is.na(ontology),
                              list("ontology" = ontology)),
                         list(!is.na(subset),
                              list("subset" = subset)),
                         list(zero_distance = TRUE,
                              list("zero_distance" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("ontology/descendants/",
                                                  term_id),
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

#' Search for an ontological term by its namespaced identifier
#'
#' @param term_id
#' @param relation
#' @param simple
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_id = function(term_id,
                                   relation = NA,
                                   simple = FALSE,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = term_id,
                                         name = "term_id",
                                         class = "character"),
                                    list(arg = relation,
                                         name = "relation",
                                         class = "character"),
                                    list(arg = simple,
                                         name = "simple",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET ontology/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(relation),
                              list("relation" = relation)),
                         list(simple = TRUE,
                              list("simple" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("ontology/id/",
                                                  term_id),
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

#' Search for a list of ontological terms by their name
#'
#' @param name
#' @param ontology
#' @param relation
#' @param simple
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_name = function(name,
                                     ontology = NA,
                                     relation = NA,
                                     simple = FALSE,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = name,
                                         name = "name",
                                         class = "character"),
                                    list(arg = ontology,
                                         name = "ontology",
                                         class = "character"),
                                    list(arg = relation,
                                         name = "relation",
                                         class = "character"),
                                    list(arg = simple,
                                         name = "simple",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET ontology/name/:name")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(ontology),
                              list("ontology" = ontology)),
                         list(!is.na(relation),
                              list("relation" = relation)),
                         list(simple = TRUE,
                              list("simple" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("ontology/name/",
                                                  name),
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

#### Taxonomy Endpoints ####

#' Return the taxonomic classification of a taxon node
#'
#' @param taxon_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_classification = function(taxon_id,
                                               verbose = TRUE,
                                               progress_bar = FALSE,
                                               diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = taxon_id,
                                         name = "taxon_id",
                                         class = c("numeric",
                                                   "character"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET taxonomy/classification/:id")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("taxonomy/classification/",
                                                  taxon_id),
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

#' Search for a taxonomic term by its identifier or name
#'
#' @param taxon_id
#' @param simple
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_id = function(taxon_id,
                                   simple = FALSE,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = taxon_id,
                                         name = "taxon_id",
                                         class = c("numeric",
                                                   "character")),
                                    list(arg = simple,
                                         name = "simple",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET taxonomy/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(simple = TRUE,
                              list("simple" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("taxonomy/id/",
                                                  taxon_id),
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

#' Search for a taxonomic id by a non-scientific name
#'
#' @param name
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_name = function(name,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = name,
                                         name = "name",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET taxonomy/name/:name")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("taxonomy/name/",
                                                  name),
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

#### Overlap Endpoints ####

#' Retrieves features (e.g. genes, transcripts, variants and more) that overlap
#' a region defined by the given identifier.
#'
#' @param ensembl_id
#' @param feature
#' @param biotype
#' @param db_type
#' @param logic_name
#' @param misc_set
#' @param object_type
#' @param so_term
#' @param species
#' @param species_set
#' @param variant_set
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_overlap_id = function(ensembl_id,
                                  feature,
                                  biotype = NA,
                                  db_type = NA,
                                  logic_name = NA,
                                  misc_set = NA,
                                  object_type = NA,
                                  so_term = NA,
                                  species = NA,
                                  species_set = "mammals",
                                  variant_set = NA,
                                  verbose = TRUE,
                                  progress_bar = FALSE,
                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character"),
                                    list(arg = feature,
                                         name = "feature",
                                         class = "character",
                                         val = c("band",
                                                 "gene",
                                                 "transcript",
                                                 "cds",
                                                 "exon",
                                                 "repeat",
                                                 "simple",
                                                 "misc",
                                                 "variation",
                                                 "somatic_variation",
                                                 "structural_variation",
                                                 "somatic_structural_variation",
                                                 "constrained",
                                                 "regulatory",
                                                 "motif",
                                                 "chipseq",
                                                 "array_probe")),
                                    list(arg = biotype,
                                         name = "biotype",
                                         class = "character"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = logic_name,
                                         name = "logic_name",
                                         class = "character"),
                                    list(arg = misc_set,
                                         name = "misc_set",
                                         class = "character"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character"),
                                    list(arg = so_term,
                                         name = "so_term",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = species_set,
                                         name = "species_set",
                                         class = "character"),
                                    list(arg = variant_set,
                                         name = "variant_set",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET overlap/id/:id")
  }
  ## build GET API request's query
  call_query = list("feature" = feature)
  additional_pars = list(list(!is.na(biotype),
                              list("biotype" = biotype)),
                         list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(logic_name),
                              list("logic_name" = logic_name)),
                         list(!is.na(misc_set),
                              list("misc_set" = misc_set)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)),
                         list(!is.na(so_term),
                              list("so_term" = so_term)),
                         list(!is.na(species),
                              list("species" = species)),
                         list(species_set != "mammals",
                              list("species_set" = species_set)),
                         list(!is.na(variant_set),
                              list("variant_set" = variant_set)))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("overlap/id/",
                                                  ensembl_id),
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

#' Retrieves features (e.g. genes, transcripts, variants and more) that
#' overlap a given region
#'
#' @param region
#' @param feature
#' @param species
#' @param biotype
#' @param db_type
#' @param logic_name
#' @param misc_set
#' @param so_term
#' @param species_set
#' @param variant_set
#' @param trim_downstream
#' @param trim_upstream
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_overlap_region = function(region,
                                      feature,
                                      species,
                                      biotype = NA,
                                      db_type = NA,
                                      logic_name = NA,
                                      misc_set = NA,
                                      so_term = NA,
                                      species_set = "mammals",
                                      variant_set = NA,
                                      trim_downstream = FALSE,
                                      trim_upstream = FALSE,
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = region,
                                         name = "region",
                                         class = "character"),
                                    list(arg = feature,
                                         name = "feature",
                                         class = "character",
                                         val = c("band",
                                                 "gene",
                                                 "transcript",
                                                 "cds",
                                                 "exon",
                                                 "repeat",
                                                 "simple",
                                                 "misc",
                                                 "variation",
                                                 "somatic_variation",
                                                 "structural_variation",
                                                 "somatic_structural_variation",
                                                 "constrained",
                                                 "regulatory",
                                                 "motif",
                                                 "chipseq",
                                                 "array_probe")),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = biotype,
                                         name = "biotype",
                                         class = "character"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = logic_name,
                                         name = "logic_name",
                                         class = "character"),
                                    list(arg = misc_set,
                                         name = "misc_set",
                                         class = "character"),
                                    list(arg = so_term,
                                         name = "so_term",
                                         class = "character"),
                                    list(arg = species_set,
                                         name = "species_set",
                                         class = "character"),
                                    list(arg = variant_set,
                                         name = "variant_set",
                                         class = "character"),
                                    list(arg = trim_downstream,
                                         name = "trim_downstream",
                                         class = "logical"),
                                    list(arg = trim_upstream,
                                         name = "trim_upstream",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET overlap/region/:species/:region")
  }
  ## build GET API request's query
  ## feture can accept more than 1 argument:
  call_query = list()
  for (i in seq_along(feature)) {
    call_query = append(call_query, list("feature" = feature[[i]]))
  }

  additional_pars = list(list(!is.na(biotype),
                              list("biotype" = biotype)),
                         list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(logic_name),
                              list("logic_name" = logic_name)),
                         list(!is.na(misc_set),
                              list("misc_set" = misc_set)),
                         list(!is.na(so_term),
                              list("so_term" = so_term)),
                         list(species_set != "mammals",
                              list("species_set" = species_set)),
                         list(!is.na(variant_set),
                              list("variant_set" = variant_set)),
                         list(trim_downstream == TRUE,
                              list("trim_downstream" = "1")),
                         list(trim_upstream == TRUE,
                              list("trim_upstream" = "1")))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("overlap/region/",
                                                  species, "/",
                                                  region),
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

#' Retrieve features related to a specific Translation as described by
#' its stable ID (e.g. domains, variants)
#'
#' @param ensembl_id
#' @param db_type
#' @param feature
#' @param so_term
#' @param species
#' @param type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_overlap_translation = function(ensembl_id,
                                           db_type = NA,
                                           feature = "protein_feature",
                                           so_term = NA,
                                           species = NA,
                                           type = NA,
                                           verbose = TRUE,
                                           progress_bar = FALSE,
                                           diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = feature,
                                         name = "feature",
                                         class = "character",
                                         val = c("transcript_variation",
                                                 "protein_feature",
                                                 "residue_overlap",
                                                 "translation_exon",
                                                 "somatic_transcript_variation"
                                         )),
                                    list(arg = so_term,
                                         name = "so_term",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = type,
                                         name = "type",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET overlap/translation/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(feature != "protein_feature",
                              list("feature" = feature)),
                         list(!is.na(so_term),
                              list("so_term" = so_term)),
                         list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(so_term),
                              list("so_term" = so_term)),
                         list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(type),
                              list("type" = type)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("overlap/translation/",
                                                  ensembl_id),
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

#### 	Phenotype annotations Endpoints ####

#' Return phenotype annotations for genomic features given a phenotype
#' ontology accession
#'
#' @param accession
#' @param species
#' @param include_children
#' @param include_pubmed_id
#' @param include_review_status
#' @param source
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_phenotype_accession = function(accession,
                                           species,
                                           include_children = FALSE,
                                           include_pubmed_id = FALSE,
                                           include_review_status = FALSE,
                                           source = "undef",
                                           verbose = TRUE,
                                           progress_bar = FALSE,
                                           diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = include_children,
                                         name = "include_children",
                                         class = "logical"),
                                    list(arg = include_pubmed_id,
                                         name = "include_pubmed_id",
                                         class = "logical"),
                                    list(arg = include_review_status,
                                         name = "include_review_status",
                                         class = "logical"),
                                    list(arg = source,
                                         name = "source",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /phenotype/accession/:species/:accession")
  }
  ## build GET API request's query
  additional_pars = list(list(include_children == TRUE,
                              list("include_children" = 1)),
                         list(include_pubmed_id == TRUE,
                              list("include_pubmed_id" = 1)),
                         list(include_review_status == TRUE,
                              list("include_review_status" = 1)),
                         list(source != "undef",
                              list("source" = source)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/phenotype/accession/",
                                                  species, "/",
                                                  accession),
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

#' Return phenotype annotations for a given gene
#'
#' @param gene
#' @param species
#' @param include_associated
#' @param include_overlap
#' @param include_pubmed_id
#' @param include_review_status
#' @param include_submitter
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_phenotype_gene = function(gene,
                                      species,
                                      include_associated = FALSE,
                                      include_overlap = FALSE,
                                      include_pubmed_id = FALSE,
                                      include_review_status = FALSE,
                                      include_submitter = FALSE,
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = gene,
                                         name = "gene",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = include_associated,
                                         name = "include_associated",
                                         class = "logical"),
                                    list(arg = include_overlap,
                                         name = "include_overlap",
                                         class = "logical"),
                                    list(arg = include_pubmed_id,
                                         name = "include_pubmed_id",
                                         class = "logical"),
                                    list(arg = include_review_status,
                                         name = "include_review_status",
                                         class = "logical"),
                                    list(arg = include_submitter,
                                         name = "include_submitter",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /phenotype/gene/:species/:gene")
  }
  ## build GET API request's query
  additional_pars = list(list(include_associated == TRUE,
                              list("include_associated" = 1)),
                         list(include_overlap == TRUE,
                              list("include_overlap" = 1)),
                         list(include_pubmed_id == TRUE,
                              list("include_pubmed_id" = 1)),
                         list(include_review_status == TRUE,
                              list("include_review_status" = 1)),
                         list(include_submitter == TRUE,
                              list("include_submitter" = 1)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/phenotype/gene/",
                                                  species, "/",
                                                  gene),
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

#' Return phenotype annotations that overlap a given genomic region
#'
#' @param region
#' @param species
#' @param feature_type
#' @param include_pubmed_id
#' @param include_review_status
#' @param include_submitter
#' @param only_phenotypes
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_phenotype_region = function(region,
                                        species,
                                        feature_type = NA,
                                        include_pubmed_id = FALSE,
                                        include_review_status = FALSE,
                                        include_submitter = FALSE,
                                        only_phenotypes = FALSE,
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = region,
                                         name = "region",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = feature_type,
                                         name = "feature_type",
                                         class = "character"),
                                    list(arg = include_pubmed_id,
                                         name = "include_pubmed_id",
                                         class = "logical"),
                                    list(arg = include_review_status,
                                         name = "include_review_status",
                                         class = "logical"),
                                    list(arg = include_submitter,
                                         name = "include_submitter",
                                         class = "logical"),
                                    list(arg = only_phenotypes,
                                         name = "only_phenotypes",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /phenotype/region/:species/:region")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(feature_type),
                              list("feature_type" = feature_type)),
                         list(include_pubmed_id == TRUE,
                              list("include_pubmed_id" = 1)),
                         list(include_review_status == TRUE,
                              list("include_review_status" = 1)),
                         list(include_submitter == TRUE,
                              list("include_submitter" = 1)),
                         list(only_phenotypes == TRUE,
                              list("only_phenotypes" = 1)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/phenotype/region/",
                                                  species, "/",
                                                  region),
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

#' Return phenotype annotations for genomic features given a phenotype
#' ontology term
#'
#' @param term
#' @param species
#' @param include_children
#' @param include_pubmed_id
#' @param include_review_status
#' @param source
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_phenotype_term = function(term,
                                           species,
                                           include_children = FALSE,
                                           include_pubmed_id = FALSE,
                                           include_review_status = FALSE,
                                           source = "undef",
                                           verbose = TRUE,
                                           progress_bar = FALSE,
                                           diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = term,
                                         name = "term",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = include_children,
                                         name = "include_children",
                                         class = "logical"),
                                    list(arg = include_pubmed_id,
                                         name = "include_pubmed_id",
                                         class = "logical"),
                                    list(arg = include_review_status,
                                         name = "include_review_status",
                                         class = "logical"),
                                    list(arg = source,
                                         name = "source",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET /phenotype/term/:species/:term")
  }
  ## build GET API request's query
  additional_pars = list(list(include_children == TRUE,
                              list("include_children" = 1)),
                         list(include_pubmed_id == TRUE,
                              list("include_pubmed_id" = 1)),
                         list(include_review_status == TRUE,
                              list("include_review_status" = 1)),
                         list(source != "undef",
                              list("source" = source)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)

  ## make function-specific calls
  term = gsub(" ", "%20", term) #replace 'space' in term to %20
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("/phenotype/term/",
                                                  species, "/",
                                                  term),
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
