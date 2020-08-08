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

#### 	Cross References Endpoints ####

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
