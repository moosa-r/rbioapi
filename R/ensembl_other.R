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

#### Comparative Genomics Endpoints ####

#' Retrieves a cafe tree of the gene tree using the gene tree stable identifier
#'
#' @param genetree_id
#' @param compara
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_cafe_genetree  = function(genetree_id,
                                      compara = "vertebrates",
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = genetree_id,
                                         name = "genetree_id",
                                         class = "character"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET cafe/genetree/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(compara != "vertebrates",
                              list("compara" = compara)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("cafe/genetree/id/",
                                                  genetree_id),
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

rba_ensembl_cafe_genetree_id  = function(genetree_id,
                                         compara = "vertebrates",
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = genetree_id,
                                         name = "genetree_id",
                                         class = "character"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET cafe/genetree/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(compara != "vertebrates",
                              list("compara" = compara)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("cafe/genetree/id/",
                                                  genetree_id),
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

#' Retrieves the cafe tree of the gene tree that contains the gene /
#' transcript / translation stable identifier
#'
#' @param ensembl_id
#' @param compara
#' @param db_type
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
rba_ensembl_cafe_genetree_member_id  = function(ensembl_id,
                                                compara = "vertebrates",
                                                db_type = NA,
                                                object_type = NA,
                                                species = NA,
                                                verbose = TRUE,
                                                progress_bar = FALSE,
                                                diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET cafe/genetree/member/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)),
                         list(!is.na(species),
                              list("species" = species)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("cafe/genetree/member/id/",
                                                  ensembl_id),
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

#' Retrieves the cafe tree of the gene tree that contains the gene
#' identified by a symbol
#'
#' @param gene_symbol
#' @param species
#' @param compara
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
rba_ensembl_cafe_genetree_member_symbol  = function(gene_symbol,
                                                    species,
                                                    compara = "vertebrates",
                                                    db_type = "core",
                                                    external_db = NA,
                                                    object_type = NA,
                                                    verbose = TRUE,
                                                    progress_bar = FALSE,
                                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = gene_symbol,
                                         name = "gene_symbol",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
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
    message("GET cafe/genetree/member/symbol/:species/:symbol")
  }
  ## build GET API request's query
  additional_pars = list(list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(db_type != "core",
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("cafe/genetree/member/symbol/",
                                                  species, "/",
                                                  gene_symbol),
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

#' Retrieves a family information using the family stable identifier
#'
#' @param familiy_id
#' @param aligned
#' @param compara
#' @param member_source
#' @param sequence
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_family_id  = function(familiy_id,
                                  aligned = TRUE,
                                  compara = "vertebrates",
                                  member_source = "all",
                                  sequence = "protein",
                                  verbose = TRUE,
                                  progress_bar = FALSE,
                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = familiy_id,
                                         name = "familiy_id",
                                         class = "character"),
                                    list(arg = aligned,
                                         name = "aligned",
                                         class = "logical"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = member_source,
                                         name = "member_source",
                                         class = "character",
                                         val = c("all",
                                                 "ensembl",
                                                 "uniprot")),
                                    list(arg = sequence,
                                         name = "sequence",
                                         class = "character",
                                         val = c("none",
                                                 "cdna",
                                                 "protein"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET family/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned == FALSE,
                              list("aligned" = "0")),
                         list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(member_source != "all",
                              list("member_source" = member_source)),
                         list(sequence != "protein",
                              list("sequence" = sequence)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("family/id/",
                                                  familiy_id),
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

#' Retrieves the information for all the families that contains the gene /
#' transcript / translation stable identifier
#'
#' @param familiy_id
#' @param aligned
#' @param compara
#' @param member_source
#' @param sequence
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_familiy_member_id  = function(ensembl_id,
                                          aligned = TRUE,
                                          compara = "vertebrates",
                                          member_source = "all",
                                          sequence = "protein",
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character"),
                                    list(arg = aligned,
                                         name = "aligned",
                                         class = "logical"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = member_source,
                                         name = "member_source",
                                         class = "character",
                                         val = c("all",
                                                 "ensembl",
                                                 "uniprot")),
                                    list(arg = sequence,
                                         name = "sequence",
                                         class = "character",
                                         val = c("none",
                                                 "cdna",
                                                 "protein"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET family/member/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned == FALSE,
                              list("aligned" = "0")),
                         list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(member_source != "all",
                              list("member_source" = member_source)),
                         list(sequence != "protein",
                              list("sequence" = sequence)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("family/member/id/",
                                                  ensembl_id),
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

#' Retrieves the information for all the families that contains the gene
#' identified by a symbol
#'
#' @param gene_symbol
#' @param species
#' @param aligned
#' @param compara
#' @param db_type
#' @param external_db
#' @param member_source
#' @param object_type
#' @param sequence
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_familiy_member_symbol  = function(gene_symbol,
                                              species,
                                              aligned = TRUE,
                                              compara = "vertebrates",
                                              db_type = "core",
                                              external_db = NA,
                                              member_source = "all",
                                              object_type = NA,
                                              sequence = "protein",
                                              verbose = TRUE,
                                              progress_bar = FALSE,
                                              diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = gene_symbol,
                                         name = "gene_symbol",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = aligned,
                                         name = "aligned",
                                         class = "logical"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character"),
                                    list(arg = member_source,
                                         name = "member_source",
                                         class = "character",
                                         val = c("all",
                                                 "ensembl",
                                                 "uniprot")),
                                    list(arg = sequence,
                                         name = "sequence",
                                         class = "character",
                                         val = c("none",
                                                 "cdna",
                                                 "protein"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET family/member/symbol/:species/:symbol")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned == FALSE,
                              list("aligned" = "0")),
                         list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(db_type != "core",
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)),
                         list(member_source != "all",
                              list("member_source" = member_source)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)),
                         list(sequence != "protein",
                              list("sequence" = sequence)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("family/member/symbol/",
                                                  species, "/",
                                                  gene_symbol),
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

#' Retrieves a gene tree for a gene tree stable identifier
#'
#' @param genetree_id
#' @param aligned
#' @param cigar_line
#' @param cluterset_id
#' @param compara
#' @param prune_species
#' @param prune_taxon
#' @param sequence
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_genetree_id  = function(genetree_id,
                                    aligned = FALSE,
                                    cigar_line = FALSE,
                                    cluterset_id = NA,
                                    compara = "vertebrates",
                                    prune_species = NA,
                                    prune_taxon = NA,
                                    sequence = "protein",
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = genetree_id,
                                         name = "genetree_id",
                                         class = "character"),
                                    list(arg = aligned,
                                         name = "aligned",
                                         class = "logical"),
                                    list(arg = cigar_line,
                                         name = "cigar_line",
                                         class = "logical"),
                                    list(arg = cluterset_id,
                                         name = "cluterset_id",
                                         class = "character"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = prune_species,
                                         name = "prune_species",
                                         class = "character"),
                                    list(arg = prune_taxon,
                                         name = "prune_taxon",
                                         class = "character"),
                                    list(arg = sequence,
                                         name = "sequence",
                                         class = "character",
                                         val = c("none",
                                                 "cdna",
                                                 "protein"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET genetree/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned == TRUE,
                              list("aligned" = "1")),
                         list(cigar_line == TRUE,
                              list("cigar_line" = "1")),
                         list(!is.na(cluterset_id),
                              list("cluterset_id" = cluterset_id)),
                         list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(!is.na(prune_species),
                              list("prune_species" = prune_species)),
                         list(!is.na(prune_taxon),
                              list("prune_taxon" = prune_taxon)),
                         list(sequence != "protein",
                              list("sequence" = sequence)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("genetree/id/",
                                                  genetree_id),
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

#' Retrieves the gene tree that contains the gene identified by a symbol
#'
#' @param gene_symbol
#' @param species
#' @param aligned
#' @param cigar_line
#' @param cluterset_id
#' @param compara
#' @param db_type
#' @param external_db
#' @param object_type
#' @param prune_species
#' @param prune_taxon
#' @param sequence
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_genetree_member_symbol  = function(gene_symbol,
                                               species,
                                               aligned = FALSE,
                                               cigar_line = FALSE,
                                               cluterset_id = NA,
                                               compara = "vertebrates",
                                               db_type = "core",
                                               external_db = NA,
                                               object_type = NA,
                                               prune_species = NA,
                                               prune_taxon = NA,
                                               sequence = "protein",
                                               verbose = TRUE,
                                               progress_bar = FALSE,
                                               diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = gene_symbol,
                                         name = "gene_symbol",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = aligned,
                                         name = "aligned",
                                         class = "logical"),
                                    list(arg = cigar_line,
                                         name = "cigar_line",
                                         class = "logical"),
                                    list(arg = cluterset_id,
                                         name = "cluterset_id",
                                         class = "character"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character"),
                                    list(arg = prune_species,
                                         name = "prune_species",
                                         class = "character"),
                                    list(arg = prune_taxon,
                                         name = "prune_taxon",
                                         class = "character"),
                                    list(arg = sequence,
                                         name = "sequence",
                                         class = "character",
                                         val = c("none",
                                                 "cdna",
                                                 "protein"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET genetree/member/symbol/:species/:symbol")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned == TRUE,
                              list("aligned" = "1")),
                         list(cigar_line == TRUE,
                              list("cigar_line" = "1")),
                         list(!is.na(cluterset_id),
                              list("cluterset_id" = cluterset_id)),
                         list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(db_type != "core",
                              list("db_type" = db_type)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)),
                         list(!is.na(object_type),
                              list("object_type" = object_type)),
                         list(!is.na(prune_species),
                              list("prune_species" = prune_species)),
                         list(!is.na(prune_taxon),
                              list("prune_taxon" = prune_taxon)),
                         list(sequence != "protein",
                              list("sequence" = sequence)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("genetree/member/symbol/",
                                                  species, "/",
                                                  gene_symbol),
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

#' Retrieves genomic alignments as separate blocks based on a region and species
#'
#' @param region
#' @param species
#' @param aligned
#' @param compact
#' @param compara
#' @param display_species_set
#' @param mask
#' @param method
#' @param species_set
#' @param species_set_group
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_alignment_region  = function(region,
                                         species,
                                         aligned = TRUE,
                                         compact = TRUE,
                                         compara = "vertebrates",
                                         display_species_set = NA,
                                         mask = NA,
                                         method = "EPO",
                                         species_set = NA,
                                         species_set_group = "mammals",
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
                                    list(arg = aligned,
                                         name = "aligned",
                                         class = "logical"),
                                    list(arg = compact,
                                         name = "compact",
                                         class = "logical"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = display_species_set,
                                         name = "display_species_set",
                                         class = "character"),
                                    list(arg = mask,
                                         name = "mask",
                                         class = "character",
                                         val = c("hard",
                                                 "soft")),
                                    list(arg = method,
                                         name = "method",
                                         class = "character",
                                         val = c("EPO",
                                                 "EPO_LOW_COVERAGE",
                                                 "PECAN",
                                                 "LASTZ_NET",
                                                 "BLASTZ_NET",
                                                 "TRANSLATED_BLAT_NET",
                                                 "CACTUS_HAL",
                                                 "CACTUS_HAL_PW")),
                                    list(arg = species_set,
                                         name = "species_set",
                                         class = "character"),
                                    list(arg = species_set_group,
                                         name = "species_set_group",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET alignment/region/:species/:region")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned == FALSE,
                              list("aligned" = "0")),
                         list(compact == FALSE,
                              list("compact" = "0")),
                         list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(!is.na(display_species_set),
                              list("display_species_set" = display_species_set)),
                         list(!is.na(mask),
                              list("mask" = mask)),
                         list(method != "EPO",
                              list("method" = method)),
                         list(!is.na(species_set),
                              list("species_set" = species_set)),
                         list(!is.na(species_set_group),
                              list("species_set_group" = species_set_group)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("alignment/region/",
                                                  species, "/",
                                                  region),
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

#' Retrieves homology information (orthologs) by Ensembl gene id
#'
#' @param ensemble_id
#' @param aligned
#' @param cigar_line
#' @param compara
#' @param format
#' @param sequence
#' @param target_species
#' @param target_taxon
#' @param type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_homology_id  = function(ensemble_id,
                                    aligned = TRUE,
                                    cigar_line = TRUE,
                                    compara = "vertebrates",
                                    format = "full",
                                    sequence = "protein",
                                    target_species = NA,
                                    target_taxon = NA,
                                    type = "all",
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ensemble_id,
                                         name = "ensemble_id",
                                         class = "character"),
                                    list(arg = aligned,
                                         name = "aligned",
                                         class = "logical"),
                                    list(arg = cigar_line,
                                         name = "cigar_line",
                                         class = "logical"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = format,
                                         name = "format",
                                         class = "character",
                                         val = c("full",
                                                 "condensed")),
                                    list(arg = sequence,
                                         name = "sequence",
                                         class = "character",
                                         val = c("none",
                                                 "cdna",
                                                 "protein")),
                                    list(arg = target_species,
                                         name = "target_species",
                                         class = "character"),
                                    list(arg = target_taxon,
                                         name = "target_taxon",
                                         class = "numeric"),
                                    list(arg = type,
                                         name = "type",
                                         class = "character",
                                         val = c("orthologues",
                                                 "paralogues",
                                                 "projections",
                                                 "all"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET homology/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned == FALSE,
                              list("aligned" = "0")),
                         list(cigar_line == FALSE,
                              list("cigar_line" = "0")),
                         list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(format != "full",
                              list("format" = format)),
                         list(sequence != "protein",
                              list("protein" = "protein")),
                         list(!is.na(target_species),
                              list("target_species" = target_species)),
                         list(!is.na( target_taxon ),
                              list(" target_taxon " =  target_taxon )),
                         list(type != "all",
                              list("type" = type)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("homology/id/",
                                                  ensemble_id),
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

#' Retrieves homology information (orthologs) by symbol
#'
#' @param gene_symbol
#' @param species
#' @param aligned
#' @param cigar_line
#' @param compara
#' @param external_db
#' @param format
#' @param sequence
#' @param target_species
#' @param target_taxon
#' @param type
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_homology_symbol  = function(gene_symbol,
                                        species,
                                        aligned = TRUE,
                                        cigar_line = TRUE,
                                        compara = "vertebrates",
                                        external_db = NA,
                                        format = "full",
                                        sequence = "protein",
                                        target_species = NA,
                                        target_taxon = NA,
                                        type = "all",
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = gene_symbol,
                                         name = "gene_symbol",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = aligned,
                                         name = "aligned",
                                         class = "logical"),
                                    list(arg = cigar_line,
                                         name = "cigar_line",
                                         class = "logical"),
                                    list(arg = compara,
                                         name = "compara",
                                         class = "character"),
                                    list(arg = external_db,
                                         name = "external_db",
                                         class = "character"),
                                    list(arg = format,
                                         name = "format",
                                         class = "character",
                                         val = c("full",
                                                 "condensed")),
                                    list(arg = sequence,
                                         name = "sequence",
                                         class = "character",
                                         val = c("none",
                                                 "cdna",
                                                 "protein")),
                                    list(arg = target_species,
                                         name = "target_species",
                                         class = "character"),
                                    list(arg = target_taxon,
                                         name = "target_taxon",
                                         class = "numeric"),
                                    list(arg = type,
                                         name = "type",
                                         class = "character",
                                         val = c("orthologues",
                                                 "paralogues",
                                                 "projections",
                                                 "all"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET homology/symbol/:species/:symbol")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned == FALSE,
                              list("aligned" = "0")),
                         list(cigar_line == FALSE,
                              list("cigar_line" = "0")),
                         list(compara != "vertebrates",
                              list("compara" = compara)),
                         list(!is.na(external_db),
                              list("external_db" = external_db)),
                         list(format != "full",
                              list("format" = format)),
                         list(sequence != "protein",
                              list("protein" = "protein")),
                         list(!is.na(target_species),
                              list("target_species" = target_species)),
                         list(!is.na( target_taxon ),
                              list(" target_taxon " =  target_taxon )),
                         list(type != "all",
                              list("type" = type)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("homology/symbol/",
                                                  species, "/",
                                                  gene_symbol),
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

#### Phenotype annotations Endpoints ####

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

#### Regulation Endpoints ####

#' Returns information about a specific microarray
#'
#' @param microarray
#' @param vendor
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_vendor = function(microarray,
                                         vendor,
                                         species,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = microarray,
                                         name = "microarray",
                                         class = "character"),
                                    list(arg = vendor,
                                         name = "vendor",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET regulatory/species/:species/microarray/:microarray/vendor/:vendor")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("regulatory/species/",
                                                  species,
                                                  "/microarray/",
                                                  microarray,
                                                  "/vendor/",
                                                  vendor),
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

#' Returns information about all microarrays available for the given species
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
rba_ensembl_microarray_species = function(species,
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
    message("GET regulatory/species/:species/microarray")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("regulatory/species/",
                                                  species,
                                                  "/microarray"),
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

#' Returns information about a specific probe from a microarray
#'
#' @param microarray
#' @param probe
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_probe = function(microarray,
                                        probe,
                                        species,
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = microarray,
                                         name = "microarray",
                                         class = "character"),
                                    list(arg = probe,
                                         name = "probe",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET regulatory/species/:species/microarray/:microarray/probe/:probe")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("regulatory/species/",
                                                  species,
                                                  "/microarray/",
                                                  microarray,
                                                  "/probe/",
                                                  probe),
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

#' Returns information about a specific probe_set from a microarray
#'
#' @param microarray
#' @param probe_set
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_probe_set = function(microarray,
                                            probe_set,
                                            species,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = microarray,
                                         name = "microarray",
                                         class = "character"),
                                    list(arg = probe_set,
                                         name = "probe_set",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET regulatory/species/:species/microarray/:microarray/probe_set/:probe_set")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("regulatory/species/",
                                                  species,
                                                  "/microarray/",
                                                  microarray,
                                                  "/probe_set/",
                                                  probe_set),
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

#' Returns information about all epigenomes available for the given species
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
rba_ensembl_microarray_epigenome = function(species,
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
    message("GET regulatory/species/:species/epigenome")
  }

  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("regulatory/species/",
                                                  species,
                                                  "/epigenome"),
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

#' Return the specified binding matrix
#'
#' @param binding_matrix_id
#' @param species
#' @param unit
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_binding_matrix = function(binding_matrix_id,
                                                 species,
                                                 unit = "frequencies",
                                                 verbose = TRUE,
                                                 progress_bar = FALSE,
                                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = binding_matrix_id,
                                         name = "binding_matrix_id",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = unit,
                                         name = "unit",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET species/:species/binding_matrix/:binding_matrix_stable_id/")
  }
  ## build GET API request's query
  additional_pars = list(list(!is.na(unit),
                              list("unit" = unit)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("species/",
                                                  species,
                                                  "/binding_matrix/",
                                                  binding_matrix_id),
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

#' Returns a RegulatoryFeature given its stable ID
#'
#' @param regulatory_feature_id
#' @param species
#' @param activity
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_regulatory_feature  = function(regulatory_feature_id,
                                                      species,
                                                      activity = FALSE,
                                                      verbose = TRUE,
                                                      progress_bar = FALSE,
                                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = regulatory_feature_id,
                                         name = "regulatory_feature_id",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = activity,
                                         name = "activity",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET regulatory/species/:species/id/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(activity == TRUE,
                              list("activity" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("regulatory/species/",
                                                  species,
                                                  "/id/",
                                                  regulatory_feature_id),
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

#### Sequence Endpoints ####

#' Request multiple types of sequence by a stable identifier list
#'
#' @param ids
#' @param db_type
#' @param start
#' @param end
#' @param expand_3prime
#' @param expand_5prime
#' @param format
#' @param mask
#' @param mask_feature
#' @param object_type
#' @param type
#' @param species
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_sequence_id = function(ids,
                                   db_type = NA,
                                   start = NA,
                                   end = NA,
                                   expand_3prime = NA,
                                   expand_5prime = NA,
                                   format = NA,
                                   mask = NA,
                                   mask_feature = FALSE,
                                   object_type = NA,
                                   type = NA,
                                   species = NA,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = ids,
                                         name = "ids",
                                         class = "character",
                                         max_len = 50),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = start,
                                         name = "start",
                                         class = "numeric"),
                                    list(arg = end,
                                         name = "end",
                                         class = "numeric"),
                                    list(arg = expand_3prime,
                                         name = "expand_3prime",
                                         class = "numeric"),
                                    list(arg = expand_5prime,
                                         name = "expand_5prime",
                                         class = "numeric"),
                                    list(arg = format,
                                         name = "format",
                                         class = "character"),
                                    list(arg = mask,
                                         name = "mask",
                                         class = "character",
                                         val = c("hard",
                                                 "soft")),
                                    list(arg = mask_feature,
                                         name = "mask_feature",
                                         class = "logical"),
                                    list(arg = object_type,
                                         name = "object_type",
                                         class = "character"),
                                    list(arg = type,
                                         name = "type",
                                         class = "character",
                                         val = c("genomic",
                                                 "cds",
                                                 "cdna",
                                                 "protein")),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric"))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST sequence/id")
  }
  ## build POST API request's query
  additional_pars = list(list(!is.na(db_type),
                              list("db_type" = db_type)),
                         list(!is.na(start),
                              list("start" = as.integer(start))),
                         list(!is.na(end),
                              list("end" = as.integer(end))),
                         list(!is.na(expand_3prime),
                              list("expand_3prime" = as.integer(expand_3prime))),
                         list(!is.na(expand_5prime),
                              list("expand_5prime" = as.integer(expand_5prime))),
                         list(!is.na(format),
                              list("format" = format)),
                         list(!is.na(mask),
                              list("mask" = mask)),
                         list(mask_feature == TRUE,
                              list("mask_feature" = "1")),
                         list(!is.na(object_type),
                              list("object_type" = object_type)),
                         list(!is.na(type),
                              list("type" = type)),
                         list(!is.na(species),
                              list("species" = species)))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## build POST API request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_ensembl"),
                                     path = "sequence/id",
                                     body = call_body,
                                     query = call_query,
                                     httr::accept_json(),
                                     httr::content_type("application/json")
  ))

  ## call API
  parser = quote(as.list(jsonlite::fromJSON(httr::content(response,
                                                          as = "text",
                                                          encoding = "UTF-8"),
                                            simplifyVector = FALSE)))
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = parser,
                                  parser_type = NA,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Request multiple types of sequence by a list of regions
#'
#' @param regions
#' @param species
#' @param coord_system
#' @param coord_system_version
#' @param expand_3prime
#' @param expand_5prime
#' @param format
#' @param mask
#' @param mask_feature
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_sequence_region = function(regions,
                                       species,
                                       coord_system = NA,
                                       coord_system_version = NA,
                                       expand_3prime = NA,
                                       expand_5prime = NA,
                                       format = NA,
                                       mask = NA,
                                       mask_feature = FALSE,
                                       verbose = TRUE,
                                       progress_bar = FALSE,
                                       diagnostics = FALSE) {

  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = regions,
                                         name = "region",
                                         class = "character",
                                         max_len = 50),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = coord_system,
                                         name = "coord_system",
                                         class = "character"),
                                    list(arg = coord_system_version,
                                         name = "coord_system_version",
                                         class = "character"),
                                    list(arg = expand_3prime,
                                         name = "expand_3prime",
                                         class = "numeric"),
                                    list(arg = expand_5prime,
                                         name = "expand_5prime",
                                         class = "numeric"),
                                    list(arg = format,
                                         name = "format",
                                         class = "character"),
                                    list(arg = mask,
                                         name = "mask",
                                         class = "character",
                                         val = c("hard",
                                                 "soft")),
                                    list(arg = mask_feature,
                                         name = "mask_feature",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("POST sequence/region/:species")
  }
  ## build POST API request's query
  additional_pars = list(list(!is.na(coord_system),
                              list("coord_system" = coord_system)),
                         list(!is.na(coord_system_version),
                              list("coord_system_version" = coord_system_version)),
                         list(!is.na(expand_3prime),
                              list("expand_3prime" = as.integer(expand_3prime))),
                         list(!is.na(expand_5prime),
                              list("expand_5prime" = as.integer(expand_5prime))),
                         list(!is.na(format),
                              list("format" = format)),
                         list(!is.na(mask),
                              list("mask" = mask)),
                         list(mask_feature == TRUE,
                              list("mask_feature" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## build POST API request's URL
  call_body = jsonlite::toJSON(list("regions" = as.array(regions)))

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_ensembl"),
                                     path = paste0("sequence/region/",
                                                   species),
                                     body = call_body,
                                     query = call_query,
                                     httr::accept_json(),
                                     httr::content_type("application/json")
  ))

  ## call API
  parser = quote(as.list(jsonlite::fromJSON(httr::content(response,
                                                          as = "text",
                                                          encoding = "UTF-8"),
                                            simplifyVector = FALSE)))
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = parser,
                                  parser_type = NA,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Transcript Haplotypes Endpoints ####

#' Computes observed transcript haplotype sequences based on phased genotype data
#'
#' @param transcript_id
#' @param species
#' @param aligned_sequences
#' @param samples
#' @param sequence
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_transcript_haplotypes  = function(transcript_id,
                                              species,
                                              aligned_sequences = FALSE,
                                              samples = FALSE,
                                              sequence = FALSE,
                                              verbose = TRUE,
                                              progress_bar = FALSE,
                                              diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = transcript_id,
                                         name = "transcript_id",
                                         class = "character"),
                                    list(arg = species,
                                         name = "species",
                                         class = c("character",
                                                   "numeric")),
                                    list(arg = aligned_sequences,
                                         name = "aligned_sequences",
                                         class = "logical"),
                                    list(arg = samples,
                                         name = "samples",
                                         class = "logical"),
                                    list(arg = sequence,
                                         name = "sequence",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("GET transcript_haplotypes/:species/:id")
  }
  ## build GET API request's query
  additional_pars = list(list(aligned_sequences == TRUE,
                              list("aligned_sequences" = "1")),
                         list(samples == TRUE,
                              list("samples" = "1")),
                         list(sequence == TRUE,
                              list("sequence" = "1")))

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_ensembl"),
                                    path = paste0("transcript_haplotypes/",
                                                  species, "/",
                                                  transcript_id),
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

