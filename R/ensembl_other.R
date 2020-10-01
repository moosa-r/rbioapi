#### Archive Endpoints ####

#' Retrieve the latest version for a set of identifiers
#'
#' @param ids
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_archive = function(ids,
                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 1000)))

  v_msg("POST archive/id")

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("id" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "archive/id",
                           body = call_body,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Comparative Genomics Endpoints ####

#' Retrieves a cafe tree of the gene tree using the gene tree stable identifier
#'
#' @param genetree_id
#' @param ...
#' @param compara
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_cafe_genetree  = function(genetree_id,
                                      compara = "vertebrates",
                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "genetree_id",
                               class = "character"),
                          list(arg = "compara",
                               class = "character")))

  v_msg("GET cafe/genetree/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("compara",
                                 compara != "vertebrates",
                                 compara))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("cafe/genetree/id/",
                                         genetree_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Retrieves a cafe tree of the gene tree using the gene tree stable identifier
#'
#' @param genetree_id
#' @param compara
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_cafe_genetree_id  = function(genetree_id,
                                         compara = "vertebrates",
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "genetree_id",
                               class = "character"),
                          list(arg = "compara",
                               class = "character")))

  v_msg("GET cafe/genetree/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("compara",
                                 compara != "vertebrates",
                                 compara))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("cafe/genetree/id/",
                                         genetree_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Retrieves the cafe tree of the gene tree that contains the gene /
#' transcript / translation stable identifier
#'
#' @param ensembl_id
#' @param compara
#' @param db_type
#' @param object_type
#' @param ...
#' @param species
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
                                                ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "object_type",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET cafe/genetree/member/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("db_type",
                                 !is.na(db_type),
                                 db_type),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type),
                            list("species",
                                 !is.na(species),
                                 species))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("cafe/genetree/member/id/",
                                         ensembl_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param object_type
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
                                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_symbol",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character"),
                          list(arg = "object_type",
                               class = "character")))

  v_msg("GET cafe/genetree/member/symbol/:species/:symbol")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("db_type",
                                 !is.na(db_type),
                                 db_type),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("cafe/genetree/member/symbol/",
                                         species, "/",
                                         gene_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Retrieves a family information using the family stable identifier
#'
#' @param familiy_id
#' @param aligned
#' @param compara
#' @param member_source
#' @param ...
#' @param sequence
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
                                  ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "familiy_id",
                               class = "character"),
                          list(arg = "aligned",
                               class = "logical"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "member_source",
                               class = "character",
                               val = c("all",
                                       "ensembl",
                                       "uniprot")),
                          list(arg = "sequence",
                               class = "character",
                               val = c("none",
                                       "cdna",
                                       "protein"))))

  v_msg("GET family/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned",
                                 aligned == FALSE,
                                 "0"),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("member_source",
                                 member_source != "all",
                                 member_source),
                            list("sequence",
                                 sequence != "protein",
                                 sequence))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("family/id/",
                                         familiy_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Retrieves the information for all the families that contains the gene /
#' transcript / translation stable identifier
#'
#' @param aligned
#' @param compara
#' @param member_source
#' @param ensembl_id
#' @param ...
#' @param sequence
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
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character"),
                          list(arg = "aligned",
                               class = "logical"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "member_source",
                               class = "character",
                               val = c("all",
                                       "ensembl",
                                       "uniprot")),
                          list(arg = "sequence",
                               class = "character",
                               val = c("none",
                                       "cdna",
                                       "protein"))))

  v_msg("GET family/member/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned",
                                 aligned == FALSE,
                                 "0"),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("member_source",
                                 member_source != "all",
                                 member_source),
                            list("sequence",
                                 sequence != "protein",
                                 sequence))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("family/member/id/",
                                         ensembl_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param sequence
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
                                              ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_symbol",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "aligned",
                               class = "logical"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character"),
                          list(arg = "member_source",
                               class = "character",
                               val = c("all",
                                       "ensembl",
                                       "uniprot")),
                          list(arg = "sequence",
                               class = "character",
                               val = c("none",
                                       "cdna",
                                       "protein"))))

  v_msg("GET family/member/symbol/:species/:symbol")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned",
                                 aligned == FALSE,
                                 "0"),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("db_type",
                                 db_type != "core",
                                 db_type),
                            list("member_source",
                                 member_source != "all",
                                 member_source),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type),
                            list("sequence",
                                 sequence != "protein",
                                 sequence))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("family/member/symbol/",
                                         species, "/",
                                         gene_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param sequence
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
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "genetree_id",
                               class = "character"),
                          list(arg = "aligned",
                               class = "logical"),
                          list(arg = "cigar_line",
                               class = "logical"),
                          list(arg = "cluterset_id",
                               class = "character"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "prune_species",
                               class = "character"),
                          list(arg = "prune_taxon",
                               class = "character"),
                          list(arg = "sequence",
                               class = "character",
                               val = c("none",
                                       "cdna",
                                       "protein"))))

  v_msg("GET genetree/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned",
                                 aligned == FALSE,
                                 "0"),
                            list("cigar_line",
                                 cigar_line == TRUE,
                                 "1"),
                            list("cluterset_id",
                                 !is.na(cluterset_id),
                                 cluterset_id),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("prune_species",
                                 !is.na(prune_species),
                                 prune_species),
                            list("prune_taxon",
                                 !is.na(prune_taxon),
                                 prune_taxon),
                            list("sequence",
                                 sequence != "protein",
                                 sequence))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("genetree/id/",
                                         genetree_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param sequence
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
                                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_symbol",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "aligned",
                               class = "logical"),
                          list(arg = "cigar_line",
                               class = "logical"),
                          list(arg = "cluterset_id",
                               class = "character"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character"),
                          list(arg = "object_type",
                               class = "character"),
                          list(arg = "prune_species",
                               class = "character"),
                          list(arg = "prune_taxon",
                               class = "character"),
                          list(arg = "sequence",
                               class = "character",
                               val = c("none",
                                       "cdna",
                                       "protein"))))

  v_msg("GET genetree/member/symbol/:species/:symbol")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned",
                                 aligned == FALSE,
                                 "0"),
                            list("cigar_line",
                                 cigar_line == TRUE,
                                 "1"),
                            list("cluterset_id",
                                 !is.na(cluterset_id),
                                 cluterset_id),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("db_type",
                                 db_type != "core",
                                 db_type),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type),
                            list("prune_species",
                                 !is.na(prune_species),
                                 prune_species),
                            list("prune_taxon",
                                 !is.na(prune_taxon),
                                 prune_taxon),
                            list("sequence",
                                 sequence != "protein",
                                 sequence))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("genetree/member/symbol/",
                                         species, "/",
                                         gene_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param species_set_group
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
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "region",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "aligned",
                               class = "logical"),
                          list(arg = "compact",
                               class = "logical"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "display_species_set",
                               class = "character"),
                          list(arg = "mask",
                               class = "character",
                               val = c("hard",
                                       "soft")),
                          list(arg = "method",
                               class = "character",
                               val = c("EPO",
                                       "EPO_LOW_COVERAGE",
                                       "PECAN",
                                       "LASTZ_NET",
                                       "BLASTZ_NET",
                                       "TRANSLATED_BLAT_NET",
                                       "CACTUS_HAL",
                                       "CACTUS_HAL_PW")),
                          list(arg = "species_set",
                               class = "character"),
                          list(arg = "species_set_group",
                               class = "character")))

  v_msg("GET alignment/region/:species/:region")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned",
                                 aligned == FALSE,
                                 "0"),
                            list("compact",
                                 compact == FALSE,
                                 "0"),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("display_species_set",
                                 !is.na(display_species_set),
                                 display_species_set),
                            list("mask",
                                 !is.na(mask),
                                 mask),
                            list("method",
                                 method != "EPO",
                                 method),
                            list("species_set",
                                 !is.na(species_set),
                                 species_set),
                            list("species_set_group",
                                 !is.na(species_set_group),
                                 species_set_group))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("alignment/region/",
                                         species, "/",
                                         region),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param type
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
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensemble_id",
                               class = "character"),
                          list(arg = "aligned",
                               class = "logical"),
                          list(arg = "cigar_line",
                               class = "logical"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "format",
                               class = "character",
                               val = c("full",
                                       "condensed")),
                          list(arg = "sequence",
                               class = "character",
                               val = c("none",
                                       "cdna",
                                       "protein")),
                          list(arg = "target_species",
                               class = "character"),
                          list(arg = "target_taxon",
                               class = "numeric"),
                          list(arg = "type",
                               class = "character",
                               val = c("orthologues",
                                       "paralogues",
                                       "projections",
                                       "all"))))

  v_msg("GET homology/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned",
                                 aligned == FALSE,
                                 "0"),
                            list("cigar_line",
                                 cigar_line == FALSE,
                                 "0"),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("format",
                                 format != "full",
                                 format),
                            list("sequence",
                                 sequence != "protein",
                                 sequence),
                            list("target_species",
                                 !is.na(target_species),
                                 target_species),
                            list("type",
                                 type != "all",
                                 type))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("homology/id/",
                                         ensemble_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param type
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
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_symbol",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "aligned",
                               class = "logical"),
                          list(arg = "cigar_line",
                               class = "logical"),
                          list(arg = "compara",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character"),
                          list(arg = "format",
                               class = "character",
                               val = c("full",
                                       "condensed")),
                          list(arg = "sequence",
                               class = "character",
                               val = c("none",
                                       "cdna",
                                       "protein")),
                          list(arg = "target_species",
                               class = "character"),
                          list(arg = "target_taxon",
                               class = "numeric"),
                          list(arg = "type",
                               class = "character",
                               val = c("orthologues",
                                       "paralogues",
                                       "projections",
                                       "all"))))

  v_msg("GET homology/symbol/:species/:symbol")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned",
                                 aligned == FALSE,
                                 "0"),
                            list("cigar_line",
                                 cigar_line == FALSE,
                                 "0"),
                            list("compara",
                                 compara != "vertebrates",
                                 compara),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db),
                            list("format",
                                 format != "full",
                                 format),
                            list("sequence",
                                 sequence != "protein",
                                 sequence),
                            list("target_species",
                                 !is.na(target_species),
                                 target_species),
                            list("target_taxon",
                                 !is.na(target_taxon),
                                 target_taxon),
                            list("type",
                                 type != "all",
                                 type))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("homology/symbol/",
                                         species, "/",
                                         gene_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param external_symbol
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_symbol = function(external_symbol,
                                    species,
                                    db_type = "core",
                                    external_db = NA,
                                    object_type = NA,
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "external_symbol",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character"),
                          list(arg = "object_type",
                               class = "character")))

  v_msg("GET xrefs/symbol/:species/:symbol")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("db_type",
                                 db_type != "core",
                                 db_type),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("xrefs/symbol/",
                                         species, "/",
                                         external_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param object_type
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_id = function(ensembl_id,
                                species = NA,
                                all_levels = FALSE,
                                db_type = "core",
                                external_db = NA,
                                object_type = NA,
                                ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character",
                               len = 1),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "all_levels",
                               class = "logical"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character"),
                          list(arg = "object_type",
                               class = "character")))

  v_msg("GET xrefs/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("species",
                                 !is.na(species),
                                 species),
                            list("all_levels",
                                 all_levels == TRUE,
                                 "1"),
                            list("db_type",
                                 db_type != "core",
                                 db_type),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("xrefs/id/",
                                         ensembl_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Performs a lookup based upon the primary accession or display label of an
#' external reference and returning the information we hold about the entry
#'
#' @param name
#' @param species
#' @param db_type
#' @param ...
#' @param external_db
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_xrefs_name = function(name,
                                  species,
                                  db_type = "core",
                                  external_db = NA,
                                  ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "name",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "external_db",
                               class = "character")))

  v_msg("GET xrefs/name/:species/:name")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("db_type",
                                 db_type != "core",
                                 db_type),
                            list("external_db",
                                 !is.na(external_db),
                                 external_db))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("xrefs/name/",
                                         species, "/",
                                         name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### EQTL Endpoints ####

#' Returns all tissues currently available in the DB
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_eqtl_tissue = function(species,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"))))

  v_msg("GET eqtl/tissue/:species/")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("eqtl/tissue/",
                                         species),
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Returns the p-value for each SNP in a given gene
#'
#' @param species
#' @param statistic
#' @param tissue
#' @param gene_id
#' @param ...
#' @param variant_name
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_eqtl_gene = function(gene_id,
                                 species,
                                 statistic = NA,
                                 tissue = NA,
                                 variant_name = NA,
                                 ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_id",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "statistic",
                               class = "character"),
                          list(arg = "tissue",
                               class = "character"),
                          list(arg = "variant_name",
                               class = "character")))

  v_msg("GET eqtl/stable_id/:species/:stable_id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("statistic",
                                 !is.na(statistic),
                                 statistic),
                            list("tissue",
                                 !is.na(tissue),
                                 tissue),
                            list("variant_name",
                                 !is.na(variant_name),
                                 variant_name))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("eqtl/id/",
                                         species, "/", gene_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Returns the p-values for a SNP (e.g. rs123)
#'
#' @param variant_name
#' @param species
#' @param stable_id
#' @param statistic
#' @param ...
#' @param tissue
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_eqtl_variant = function(variant_name,
                                    species,
                                    stable_id = NA,
                                    statistic = NA,
                                    tissue = NA,
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "variant_name",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "stable_id",
                               class = "character"),
                          list(arg = "statistic",
                               class = "character"),
                          list(arg = "tissue",
                               class = "character")))

  v_msg("GET eqtl/variant_name/:species/:variant_name")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("stable_id",
                                 !is.na(stable_id),
                                 stable_id),
                            list("statistic",
                                 !is.na(statistic),
                                 statistic),
                            list("tissue",
                                 !is.na(tissue),
                                 tissue))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("eqtl/variant_name/",
                                         species, "/", variant_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param window_size
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
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "variant_id",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "population_name",
                               class = "character"),
                          list(arg = "attribs",
                               class = "logical"),
                          list(arg = "d_prime",
                               class = "numeric"),
                          list(arg = "r2",
                               class = "numeric"),
                          list(arg = "window_size",
                               class = "numeric")))

  v_msg("GET ld/:species/:id/:population_name")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("attribs",
                                 attribs == TRUE,
                                 "1"),
                            list("d_prime",
                                 !is.na(d_prime),
                                 d_prime),
                            list("r2",
                                 !is.na(r2),
                                 r2),
                            list("window_size",
                                 window_size != 500,
                                 as.integer(window_size)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ld/",
                                         species, "/",
                                         variant_id, "/",
                                         population_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Computes and returns LD values between the given variants
#'
#' @param variant_id_1
#' @param variant_id_2
#' @param species
#' @param population_name
#' @param d_prime
#' @param ...
#' @param r2
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
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "variant_id_1",
                               class = "character"),
                          list(arg = "variant_id_2",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "population_name",
                               class = "character"),
                          list(arg = "d_prime",
                               class = "numeric"),
                          list(arg = "r2",
                               class = "numeric")))

  v_msg("GET ld/:species/pairwise/:id1/:id2")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("population_name",
                                 !is.na(population_name),
                                 population_name),
                            list("d_prime",
                                 !is.na(d_prime),
                                 d_prime),
                            list("r2",
                                 !is.na(r2),
                                 r2))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ld/",
                                         species, "/pairwise/",
                                         variant_id_1, "/",
                                         variant_id_2),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Computes and returns LD values between all pairs of variants in the defined
#' region
#'
#' @param region
#' @param population_name
#' @param species
#' @param d_prime
#' @param ...
#' @param r2
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
                                 ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "region",
                               class = "character"),
                          list(arg = "population_name",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "d_prime",
                               class = "numeric"),
                          list(arg = "r2",
                               class = "numeric")))

  v_msg("GET ld/:species/region/:region/:population_name")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("d_prime",
                                 !is.na(d_prime),
                                 d_prime),
                            list("r2",
                                 !is.na(r2),
                                 r2))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ld/",
                                         species, "/region/",
                                         region, "/",
                                         population_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_lookup_id = function(ids,
                                 db_type = NA,
                                 expand = FALSE,
                                 format = "full",
                                 object_type = NA,
                                 species = NA,
                                 ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 1000),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "expand",
                               class = "logical"),
                          list(arg = "format",
                               class = "character",
                               val = c("full",
                                       "condensed")),
                          list(arg = "object_type",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"))))

  v_msg("POST lookup/id")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("db_type",
                                 !is.na(db_type),
                                 db_type),
                            list("expand",
                                 expand == TRUE,
                                 "1"),
                            list("format",
                                 format != "full",
                                 format),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type),
                            list("species",
                                 !is.na(species),
                                 species))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "lookup/id",
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Find the species and database for a set of symbols in a linked external
#' database. Unknown symbols are omitted from the response
#'
#' @param symbols
#' @param species
#' @param expand
#' @param ...
#' @param format
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_lookup_symbol = function(symbols,
                                     species,
                                     expand = FALSE,
                                     format = "full",
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "symbols",
                               class = "character",
                               max_len = 1000),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "expand",
                               class = "logical"),
                          list(arg = "format",
                               class = "character",
                               val = c("full",
                                       "condensed"))))

  v_msg("POST lookup/symbol/:species/:symbol")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"),
                            list("format",
                                 format != "full",
                                 format))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("symbols" = as.array(symbols)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("/lookup/symbol/",
                                         species),
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Mapping Endpoints ####

#' Convert from cDNA coordinates to genomic coordinates. Output reflects
#' forward orientation coordinates as returned from the Ensembl API
#'
#' @param ensembl_id
#' @param region
#' @param species
#' @param ...
#' @param include_original_region
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_map_cdna = function(ensembl_id,
                                region,
                                species = NA,
                                include_original_region = FALSE,
                                ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character"),
                          list(arg = "region",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "include_original_region",
                               class = "logical")))

  v_msg("GET map/cdna/:id/:region")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("species",
                                 !is.na(species),
                                 species),
                            list("include_original_region",
                                 include_original_region == TRUE,
                                 include_original_region))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("map/cdna/",
                                         ensembl_id, "/",
                                         region),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Convert from CDS coordinates to genomic coordinates. Output reflects
#' forward orientation coordinates as returned from the Ensembl API.
#'
#' @param ensembl_id
#' @param region
#' @param species
#' @param ...
#' @param include_original_region
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_map_cds = function(ensembl_id,
                               region,
                               species = NA,
                               include_original_region = FALSE,
                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character"),
                          list(arg = "region",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "include_original_region",
                               class = "logical")))

  v_msg("GET map/cds/:id/:region")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("species",
                                 !is.na(species),
                                 species),
                            list("include_original_region",
                                 include_original_region == TRUE,
                                 include_original_region))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("map/cds/",
                                         ensembl_id, "/",
                                         region),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Convert the co-ordinates of one assembly to another
#'
#' @param asm_one
#' @param asm_two
#' @param region
#' @param species
#' @param coord_system
#' @param ...
#' @param target_coord_system
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_map_assembly = function(asm_one,
                                    asm_two,
                                    region,
                                    species,
                                    coord_system = "chromosome",
                                    target_coord_system = "chromosome",
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "asm_one",
                               class = "character"),
                          list(arg = "asm_two",
                               class = "character"),
                          list(arg = "region",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "coord_system",
                               class = "character"),
                          list(arg = "target_coord_system",
                               class = "character")))

  v_msg("GET map/:species/:asm_one/:region/:asm_two")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("coord_system",
                                 coord_system != "chromosome",
                                 coord_system),
                            list("include_original_region",
                                 target_coord_system = TRUE,
                                 target_coord_system))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("map/",
                                         species, "/",
                                         asm_one, "/",
                                         region, "/",
                                         asm_two),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Convert from protein (translation) coordinates to genomic coordinates.
#' Output reflects forward orientation coordinates as returned from the Ensembl API
#'
#' @param ensembl_id
#' @param region
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_map_translation = function(ensembl_id,
                                       region,
                                       species = NA,
                                       ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character"),
                          list(arg = "region",
                               class = "character"),
                          list(arg = "species",
                               class = c("numeric",
                                         "character"))))

  v_msg("GET map/translation/:id/:region")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("species",
                                 !is.na(species),
                                 species))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("map/translation/",
                                         ensembl_id, "/",
                                         region),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Ontologies Endpoints ####

#' Reconstruct the entire ancestry of a term from is_a and part_of relationships
#'
#' @param term_id
#' @param chart
#' @param ...
#' @param ontology
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_ancestors = function(term_id,
                                          chart = FALSE,
                                          ontology = NA,
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "term_id",
                               class = "character"),
                          list(arg = "chart",
                               class = "logical"),
                          list(arg = "ontology",
                               class = "character")))

  v_msg("GET ontology/ancestors/chart/:id \r\n",
        "GET ontology/ancestors/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("ontology",
                                 !is.na(ontology),
                                 ontology))

  ## Build Function-Specific Call
  if (chart == TRUE) {
    path_input = paste0("ontology/ancestors/chart/",
                        term_id)
    parser_input = "json->list"
  } else {
    path_input = paste0("ontology/ancestors/",
                        term_id)
    parser_input = "json->df"
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = parser_input)

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Find all the terms descended from a given term. By default searches are
#' conducted within the namespace of the given identifier
#'
#' @param term_id
#' @param closest_term
#' @param ontology
#' @param subset
#' @param ...
#' @param zero_distance
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
                                            ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "term_id",
                               class = "character"),
                          list(arg = "closest_term",
                               class = "logical"),
                          list(arg = "ontology",
                               class = "character"),
                          list(arg = "subset",
                               class = "character"),
                          list(arg = "zero_distance",
                               class = "logical")))

  v_msg("GET ontology/descendants/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("closest_term",
                                 closest_term = TRUE,
                                 "1"),
                            list("ontology",
                                 !is.na(ontology),
                                 ontology),
                            list("subset",
                                 !is.na(subset),
                                 subset),
                            list("zero_distance",
                                 zero_distance = TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ontology/descendants/",
                                         term_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Search for an ontological term by its namespaced identifier
#'
#' @param term_id
#' @param relation
#' @param ...
#' @param simple
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_id = function(term_id,
                                   relation = NA,
                                   simple = FALSE,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "term_id",
                               class = "character"),
                          list(arg = "relation",
                               class = "character"),
                          list(arg = "simple",
                               class = "logical")))

  v_msg("GET ontology/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("relation",
                                 !is.na(relation),
                                 relation),
                            list("simple",
                                 simple = TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ontology/id/",
                                         term_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Search for a list of ontological terms by their name
#'
#' @param name
#' @param ontology
#' @param relation
#' @param ...
#' @param simple
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_ontology_name = function(name,
                                     ontology = NA,
                                     relation = NA,
                                     simple = FALSE,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "name",
                               class = "character"),
                          list(arg = "ontology",
                               class = "character"),
                          list(arg = "relation",
                               class = "character"),
                          list(arg = "simple",
                               class = "logical")))

  v_msg("GET ontology/name/:name")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("ontology",
                                 !is.na(ontology),
                                 ontology),
                            list("relation",
                                 !is.na(relation),
                                 relation),
                            list("simple",
                                 simple = TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("ontology/name/",
                                         name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Taxonomy Endpoints ####

#' Return the taxonomic classification of a taxon node
#'
#' @param ...
#' @param taxon_id
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_classification = function(taxon_id,
                                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "taxon_id",
                               class = c("numeric",
                                         "character"))))

  v_msg("GET taxonomy/classification/:id")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("taxonomy/classification/",
                                         taxon_id),
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Search for a taxonomic term by its identifier or name
#'
#' @param taxon_id
#' @param ...
#' @param simple
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_id = function(taxon_id,
                                   simple = FALSE,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "taxon_id",
                               class = c("numeric",
                                         "character")),
                          list(arg = "simple",
                               class = "logical")))

  v_msg("GET taxonomy/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("simple",
                                 simple = TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("taxonomy/id/",
                                         taxon_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Search for a taxonomic id by a non-scientific name
#'
#' @param ...
#' @param name
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_taxonomy_name = function(name,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "name",
                               class = "character")))

  v_msg("GET taxonomy/name/:name")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("taxonomy/name/",
                                         name),
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param variant_set
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
                                  ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character"),
                          list(arg = "feature",
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
                          list(arg = "biotype",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "logic_name",
                               class = "character"),
                          list(arg = "misc_set",
                               class = "character"),
                          list(arg = "object_type",
                               class = "character"),
                          list(arg = "so_term",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "species_set",
                               class = "character"),
                          list(arg = "variant_set",
                               class = "character")))

  v_msg("GET overlap/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("feature" = feature),
                            list("biotype",
                                 !is.na(biotype),
                                 biotype),
                            list("db_type",
                                 !is.na(db_type),
                                 db_type),
                            list("logic_name",
                                 !is.na(logic_name),
                                 logic_name),
                            list("misc_set",
                                 !is.na(misc_set),
                                 misc_set),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type),
                            list("so_term",
                                 !is.na(so_term),
                                 so_term),
                            list("species",
                                 !is.na(species),
                                 species),
                            list("species_set",
                                 species_set != "mammals",
                                 species_set),
                            list("variant_set",
                                 !is.na(variant_set),
                                 variant_set))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("overlap/id/",
                                         ensembl_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param trim_upstream
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
                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "region",
                               class = "character"),
                          list(arg = "feature",
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
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "biotype",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "logic_name",
                               class = "character"),
                          list(arg = "misc_set",
                               class = "character"),
                          list(arg = "so_term",
                               class = "character"),
                          list(arg = "species_set",
                               class = "character"),
                          list(arg = "variant_set",
                               class = "character"),
                          list(arg = "trim_downstream",
                               class = "logical"),
                          list(arg = "trim_upstream",
                               class = "logical")))

  v_msg("GET overlap/region/:species/:region")

  ## Build GET API Request's query
  ## feature can accept more than 1 argument:
  call_query = as.list(feature)
  names(call_query) = rep("feature", length(call_query))

  call_query = rba_ba_query(init = call_query,
                            list("biotype",
                                 !is.na(biotype),
                                 biotype),
                            list("db_type",
                                 !is.na(db_type),
                                 db_type),
                            list("logic_name",
                                 !is.na(logic_name),
                                 logic_name),
                            list("misc_set",
                                 !is.na(misc_set),
                                 misc_set),
                            list("so_term",
                                 !is.na(so_term),
                                 so_term),
                            list("species_set",
                                 species_set != "mammals",
                                 species_set),
                            list("variant_set",
                                 !is.na(variant_set),
                                 variant_set),
                            list("trim_downstream",
                                 trim_downstream == TRUE,
                                 "1"),
                            list("trim_upstream",
                                 trim_upstream = TRUE,
                                 trim_upstream))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("overlap/region/",
                                         species, "/",
                                         region),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param type
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
                                           ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ensembl_id",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "feature",
                               class = "character",
                               val = c("transcript_variation",
                                       "protein_feature",
                                       "residue_overlap",
                                       "translation_exon",
                                       "somatic_transcript_variation"
                               )),
                          list(arg = "so_term",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "type",
                               class = "character")))

  v_msg("GET overlap/translation/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("db_type",
                                 !is.na(db_type),
                                 db_type),
                            list("feature",
                                 feature != "protein_feature",
                                 feature),
                            list("so_term",
                                 !is.na(so_term),
                                 so_term),
                            list("species",
                                 !is.na(species),
                                 species),
                            list("type",
                                 !is.na(type),
                                 type))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("overlap/translation/",
                                         ensembl_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param source
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
                                           ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "include_children",
                               class = "logical"),
                          list(arg = "include_pubmed_id",
                               class = "logical"),
                          list(arg = "include_review_status",
                               class = "logical"),
                          list(arg = "source",
                               class = "character")))

  v_msg("GET /phenotype/accession/:species/:accession")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("include_children",
                                 include_children == TRUE,
                                 "1"),
                            list("include_pubmed_id",
                                 include_pubmed_id == TRUE,
                                 "1"),
                            list("include_review_status",
                                 include_review_status == TRUE,
                                 "1"),
                            list("source",
                                 source != "undef",
                                 source))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("/phenotype/accession/",
                                         species, "/",
                                         accession),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param include_submitter
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
                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "include_associated",
                               class = "logical"),
                          list(arg = "include_overlap",
                               class = "logical"),
                          list(arg = "include_pubmed_id",
                               class = "logical"),
                          list(arg = "include_review_status",
                               class = "logical"),
                          list(arg = "include_submitter",
                               class = "logical")))

  v_msg("GET /phenotype/gene/:species/:gene")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("include_associated",
                                 include_associated == TRUE,
                                 "1"),
                            list("include_overlap",
                                 include_overlap == TRUE,
                                 "1"),
                            list("include_pubmed_id",
                                 include_pubmed_id == TRUE,
                                 "1"),
                            list("include_review_status",
                                 include_review_status == TRUE,
                                 "1"),
                            list("include_submitter",
                                 include_submitter == TRUE,
                                 "1"))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("/phenotype/gene/",
                                         species, "/",
                                         gene),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param only_phenotypes
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
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "region",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "feature_type",
                               class = "character"),
                          list(arg = "include_pubmed_id",
                               class = "logical"),
                          list(arg = "include_review_status",
                               class = "logical"),
                          list(arg = "include_submitter",
                               class = "logical"),
                          list(arg = "only_phenotypes",
                               class = "logical")))

  v_msg("GET /phenotype/region/:species/:region")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("feature_type",
                                 !is.na(feature_type),
                                 feature_type),
                            list("include_pubmed_id",
                                 include_pubmed_id == TRUE,
                                 "1"),
                            list("include_review_status",
                                 include_review_status == TRUE,
                                 "1"),
                            list("include_submitter",
                                 include_submitter == TRUE,
                                 "1"),
                            list("only_phenotypes",
                                 only_phenotypes == TRUE,
                                 "1"))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("/phenotype/region/",
                                         species, "/",
                                         region),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param source
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
                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "term",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "include_children",
                               class = "logical"),
                          list(arg = "include_pubmed_id",
                               class = "logical"),
                          list(arg = "include_review_status",
                               class = "logical"),
                          list(arg = "source",
                               class = "character")))

  v_msg("GET /phenotype/term/:species/:term")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("include_children",
                                 include_children == TRUE,
                                 "1"),
                            list("include_pubmed_id",
                                 include_pubmed_id == TRUE,
                                 "1"),
                            list("include_review_status",
                                 include_review_status == TRUE,
                                 "1"),
                            list("source",
                                 source != "undef",
                                 source))

  ## Build Function-Specific Call
  term = gsub(" ", "%20", term) #replace 'space' in term to %20
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("/phenotype/term/",
                                         species, "/",
                                         term),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Regulation Endpoints ####

#' Returns information about a specific microarray
#'
#' @param microarray
#' @param vendor
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_vendor = function(microarray,
                                         vendor,
                                         species,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "microarray",
                               class = "character"),
                          list(arg = "vendor",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET regulatory/species/:species/microarray/:microarray/vendor/:vendor")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("regulatory/species/",
                                         species,
                                         "/microarray/",
                                         microarray,
                                         "/vendor/",
                                         vendor),
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Returns information about all microarrays available for the given species
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_species = function(species,
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET regulatory/species/:species/microarray")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("regulatory/species/",
                                         species,
                                         "/microarray"),
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Returns information about a specific probe from a microarray
#'
#' @param microarray
#' @param probe
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_probe = function(microarray,
                                        probe,
                                        species,
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "microarray",
                               class = "character"),
                          list(arg = "probe",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET regulatory/species/:species/microarray/:microarray/probe/:probe")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("regulatory/species/",
                                         species,
                                         "/microarray/",
                                         microarray,
                                         "/probe/",
                                         probe),
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Returns information about a specific probe_set from a microarray
#'
#' @param microarray
#' @param probe_set
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_probe_set = function(microarray,
                                            probe_set,
                                            species,
                                            ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "microarray",
                               class = "character"),
                          list(arg = "probe_set",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET regulatory/species/:species/microarray/:microarray/probe_set/:probe_set")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("regulatory/species/",
                                         species,
                                         "/microarray/",
                                         microarray,
                                         "/probe_set/",
                                         probe_set),
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Returns information about all epigenomes available for the given species
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_epigenome = function(species,
                                            ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("GET regulatory/species/:species/epigenome")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("regulatory/species/",
                                         species,
                                         "/epigenome"),
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Return the specified binding matrix
#'
#' @param binding_matrix_id
#' @param species
#' @param ...
#' @param unit
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_binding_matrix = function(binding_matrix_id,
                                                 species,
                                                 unit = "frequencies",
                                                 ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "binding_matrix_id",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "unit",
                               class = "character")))

  v_msg("GET species/:species/binding_matrix/:binding_matrix_stable_id/")

  ## Build GET API Request's query
  additional_pars = list(list(!is.na(unit),
                              list("unit" = unit)))

  call_query = rba_ba_query(init = list(),
                            additional_pars = additional_pars)
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("species/",
                                         species,
                                         "/binding_matrix/",
                                         binding_matrix_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Returns a RegulatoryFeature given its stable ID
#'
#' @param regulatory_feature_id
#' @param species
#' @param ...
#' @param activity
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_regulatory_feature  = function(regulatory_feature_id,
                                                      species,
                                                      activity = FALSE,
                                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "regulatory_feature_id",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "activity",
                               class = "logical")))

  v_msg("GET regulatory/species/:species/id/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("activity",
                                 activity == TRUE,
                                 "1"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("regulatory/species/",
                                         species,
                                         "/id/",
                                         regulatory_feature_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param species
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
                                   type = "genomic",
                                   species = NA,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)

  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 50),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "start",
                               class = "numeric"),
                          list(arg = "end",
                               class = "numeric"),
                          list(arg = "expand_3prime",
                               class = "numeric"),
                          list(arg = "expand_5prime",
                               class = "numeric"),
                          list(arg = "format",
                               class = "character"),
                          list(arg = "mask",
                               class = "character",
                               val = c("hard",
                                       "soft")),
                          list(arg = "mask_feature",
                               class = "logical"),
                          list(arg = "object_type",
                               class = "character"),
                          list(arg = "type",
                               class = "character",
                               val = c("genomic",
                                       "cds",
                                       "cdna",
                                       "protein")),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))))

  v_msg("POST sequence/id")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("db_type",
                                 !is.na(db_type),
                                 db_type),
                            list("start",
                                 !is.na(start),
                                 as.integer(start)),
                            list("end",
                                 !is.na(end),
                                 as.integer(end)),
                            list("expand_3prime",
                                 !is.na(expand_3prime),
                                 as.integer(expand_3prime)),
                            list("expand_5prime",
                                 !is.na(expand_5prime),
                                 as.integer(expand_5prime)),
                            list("format",
                                 !is.na(format),
                                 format),
                            list("mask",
                                 !is.na(mask),
                                 mask),
                            list("mask_feature",
                                 mask_feature == TRUE,
                                 "1"),
                            list("object_type",
                                 !is.na(object_type),
                                 object_type),
                            list("type",
                                 type != "genomic",
                                 type),
                            list("species",
                                 !is.na(species),
                                 species))
  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "sequence/id",
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list_no_simp")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

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
#' @param ...
#' @param mask_feature
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
                                       ...) {
  ## Load Global Options
  rba_ba_ext_args(...)

  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "region",
                               class = "character",
                               max_len = 50),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "coord_system",
                               class = "character"),
                          list(arg = "coord_system_version",
                               class = "character"),
                          list(arg = "expand_3prime",
                               class = "numeric"),
                          list(arg = "expand_5prime",
                               class = "numeric"),
                          list(arg = "format",
                               class = "character"),
                          list(arg = "mask",
                               class = "character",
                               val = c("hard",
                                       "soft")),
                          list(arg = "mask_feature",
                               class = "logical")))

  v_msg("POST sequence/region/:species")

  ## Build POST API Request's query
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

  call_query = rba_ba_query(init = list(),
                            list("coord_system",
                                 !is.na(coord_system),
                                 coord_system),
                            list("coord_system_version",
                                 !is.na(coord_system_version),
                                 coord_system_version),
                            list("expand_3prime",
                                 !is.na(expand_3prime),
                                 as.integer(expand_3prime)),
                            list("expand_5prime",
                                 !is.na(expand_5prime),
                                 as.integer(expand_5prime)),
                            list("format",
                                 !is.na(format),
                                 format),
                            list("mask",
                                 !is.na(mask),
                                 mask),
                            list("mask_feature",
                                 mask_feature == TRUE,
                                 "1"))
  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("regions" = as.array(regions)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("sequence/region/",
                                         species),
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list_no_simp")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Transcript Haplotypes Endpoints ####

#' Computes observed transcript haplotype sequences based on phased genotype data
#'
#' @param transcript_id
#' @param species
#' @param aligned_sequences
#' @param samples
#' @param ...
#' @param sequence
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
                                              ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "aligned_sequences",
                               class = "logical"),
                          list(arg = "samples",
                               class = "logical"),
                          list(arg = "sequence",
                               class = "logical")))

  v_msg("GET transcript_haplotypes/:species/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned_sequences",
                                 aligned_sequences == TRUE,
                                 "1"),
                            list("samples",
                                 samples == TRUE,
                                 "1"),
                            list("sequence",
                                 sequence == TRUE,
                                 "1"))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("transcript_haplotypes/",
                                         species, "/",
                                         transcript_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### VEP Endpoints ####

#' Fetch variant consequences for multiple HGVS notations
#'
#' @param hgvs_notations
#' @param species
#' @param Blosum62
#' @param CADD
#' @param GeneSplicer
#' @param LoF
#' @param MaxEntScan
#' @param Phenotypes
#' @param SpliceAI
#' @param SpliceRegion
#' @param appris
#' @param canonical
#' @param ccds
#' @param dbNSFP
#' @param dbscSNV
#' @param distance
#' @param domains
#' @param failed
#' @param hgvs
#' @param mane
#' @param merged
#' @param miRNA
#' @param minimal
#' @param numbers
#' @param protein
#' @param refseq
#' @param shift_3prime
#' @param shift_genomic
#' @param transcript_id
#' @param transcript_version
#' @param tsl
#' @param uniprot
#' @param variant_class
#' @param vcf_string
#' @param ...
#' @param xref_refseq
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_vep_hgvs = function(hgvs_notations,
                                species,
                                Blosum62 = FALSE,
                                CADD = FALSE,
                                GeneSplicer = FALSE,
                                LoF = FALSE,
                                MaxEntScan = FALSE,
                                Phenotypes = FALSE,
                                SpliceAI = FALSE,
                                SpliceRegion = FALSE,
                                appris = FALSE,
                                canonical = FALSE,
                                ccds = FALSE,
                                dbNSFP = NA,
                                dbscSNV = FALSE,
                                distance = 5000,
                                domains = FALSE,
                                failed = FALSE,
                                hgvs = FALSE,
                                mane = FALSE,
                                merged = FALSE,
                                miRNA = FALSE,
                                minimal = FALSE,
                                numbers = FALSE,
                                protein = FALSE,
                                refseq = FALSE,
                                shift_3prime = FALSE,
                                shift_genomic = FALSE,
                                transcript_id = NA,
                                transcript_version = FALSE,
                                tsl = FALSE,
                                uniprot = FALSE,
                                variant_class = FALSE,
                                vcf_string = FALSE,
                                xref_refseq = FALSE,
                                ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "hgvs_notations",
                               class = "character",
                               max_len = 200),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "Blosum62",
                               class = "logical"),
                          list(arg = "CADD",
                               class = "logical"),
                          list(arg = "GeneSplicer",
                               class = "logical"),
                          list(arg = "LoF",
                               class = "logical"),
                          list(arg = "MaxEntScan",
                               class = "logical"),
                          list(arg = "Phenotypes",
                               class = "logical"),
                          list(arg = "SpliceAI",
                               class = "logical"),
                          list(arg = "SpliceRegion",
                               class = "logical"),
                          list(arg = "appris",
                               class = "logical"),
                          list(arg = "canonical",
                               class = "logical"),
                          list(arg = "ccds",
                               class = "logical"),
                          list(arg = "dbNSFP",
                               class = "character"),
                          list(arg = "dbscSNV",
                               class = "logical"),
                          list(arg = "distance",
                               class = "numeric"),
                          list(arg = "domains",
                               class = "logical"),
                          list(arg = "failed",
                               class = "logical"),
                          list(arg = "hgvs",
                               class = "logical"),
                          list(arg = "mane",
                               class = "logical"),
                          list(arg = "merged",
                               class = "logical"),
                          list(arg = "miRNA",
                               class = "logical"),
                          list(arg = "minimal",
                               class = "logical"),
                          list(arg = "numbers",
                               class = "logical"),
                          list(arg = "protein",
                               class = "logical"),
                          list(arg = "refseq",
                               class = "logical"),
                          list(arg = "shift_3prime",
                               class = "logical"),
                          list(arg = "shift_genomic",
                               class = "logical"),
                          list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "transcript_version",
                               class = "logical"),
                          list(arg = "tsl",
                               class = "logical"),
                          list(arg = "uniprot",
                               class = "logical"),
                          list(arg = "variant_class",
                               class = "logical"),
                          list(arg = "vcf_string",
                               class = "logical"),
                          list(arg = "xref_refseq",
                               class = "logical")))

  v_msg("POST vep/:species/hgvs")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("Blosum62",
                                 Blosum62 == TRUE,
                                 "1"),
                            list("CADD",
                                 CADD == TRUE,
                                 "1"),
                            list("GeneSplicer",
                                 GeneSplicer == TRUE,
                                 "1"),
                            list("LoF",
                                 LoF == TRUE,
                                 "1"),
                            list("MaxEntScan",
                                 MaxEntScan == TRUE,
                                 "1"),
                            list("Phenotypes",
                                 Phenotypes == TRUE,
                                 "1"),
                            list("SpliceAI",
                                 SpliceAI == TRUE,
                                 "1"),
                            list("SpliceRegion",
                                 SpliceRegion == TRUE,
                                 "1"),
                            list("appris",
                                 appris == TRUE,
                                 "1"),
                            list("canonical",
                                 canonical == TRUE,
                                 "1"),
                            list("ccds",
                                 ccds == TRUE,
                                 "1"),
                            list("dbNSFP",
                                 !is.na(dbNSFP),
                                 dbNSFP),
                            list("dbscSNV",
                                 dbscSNV == TRUE,
                                 "1"),
                            list("distance",
                                 distance != 5000,
                                 as.integer(distance)),
                            list("domains",
                                 domains == TRUE,
                                 "1"),
                            list("failed",
                                 failed == TRUE,
                                 "1"),
                            list("hgvs",
                                 hgvs == TRUE,
                                 "1"),
                            list("mane",
                                 mane == TRUE,
                                 "1"),
                            list("merged",
                                 merged == TRUE,
                                 "1"),
                            list("miRNA",
                                 miRNA == TRUE,
                                 "1"),
                            list("minimal",
                                 minimal == TRUE,
                                 "1"),
                            list("numbers",
                                 numbers == TRUE,
                                 "1"),
                            list("protein",
                                 protein == TRUE,
                                 "1"),
                            list("refseq",
                                 refseq == TRUE,
                                 "1"),
                            list("shift_3prime",
                                 shift_3prime == TRUE,
                                 "1"),
                            list("shift_genomic",
                                 shift_genomic == TRUE,
                                 "1"),
                            list("transcript_id",
                                 !is.na(transcript_id),
                                 transcript_id),
                            list("transcript_version",
                                 transcript_version == TRUE,
                                 "1"),
                            list("tsl",
                                 tsl == TRUE,
                                 "1"),
                            list("uniprot",
                                 uniprot == TRUE,
                                 "1"),
                            list("variant_class",
                                 variant_class == TRUE,
                                 "1"),
                            list("vcf_string",
                                 vcf_string == TRUE,
                                 "1"),
                            list("xref_refseq",
                                 xref_refseq == TRUE,
                                 "1"))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("hgvs_notations" = as.array(hgvs_notations)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("vep/",
                                         species,
                                         "/hgvs"),
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list_no_simp")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Fetch variant consequences for multiple ids
#'
#' @param ids
#' @param species
#' @param Blosum62
#' @param CADD
#' @param GeneSplicer
#' @param LoF
#' @param MaxEntScan
#' @param Phenotypes
#' @param SpliceAI
#' @param SpliceRegion
#' @param appris
#' @param canonical
#' @param ccds
#' @param dbNSFP
#' @param dbscSNV
#' @param distance
#' @param domains
#' @param failed
#' @param hgvs
#' @param mane
#' @param merged
#' @param miRNA
#' @param minimal
#' @param numbers
#' @param protein
#' @param refseq
#' @param shift_3prime
#' @param shift_genomic
#' @param transcript_id
#' @param transcript_version
#' @param tsl
#' @param uniprot
#' @param variant_class
#' @param vcf_string
#' @param ...
#' @param xref_refseq
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_vep_ids = function(ids,
                               species,
                               Blosum62 = FALSE,
                               CADD = FALSE,
                               GeneSplicer = FALSE,
                               LoF = FALSE,
                               MaxEntScan = FALSE,
                               Phenotypes = FALSE,
                               SpliceAI = FALSE,
                               SpliceRegion = FALSE,
                               appris = FALSE,
                               canonical = FALSE,
                               ccds = FALSE,
                               dbNSFP = NA,
                               dbscSNV = FALSE,
                               distance = 5000,
                               domains = FALSE,
                               failed = FALSE,
                               hgvs = FALSE,
                               mane = FALSE,
                               merged = FALSE,
                               miRNA = FALSE,
                               minimal = FALSE,
                               numbers = FALSE,
                               protein = FALSE,
                               refseq = FALSE,
                               shift_3prime = FALSE,
                               shift_genomic = FALSE,
                               transcript_id = NA,
                               transcript_version = FALSE,
                               tsl = FALSE,
                               uniprot = FALSE,
                               variant_class = FALSE,
                               vcf_string = FALSE,
                               xref_refseq = FALSE,
                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 200),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "Blosum62",
                               class = "logical"),
                          list(arg = "CADD",
                               class = "logical"),
                          list(arg = "GeneSplicer",
                               class = "logical"),
                          list(arg = "LoF",
                               class = "logical"),
                          list(arg = "MaxEntScan",
                               class = "logical"),
                          list(arg = "Phenotypes",
                               class = "logical"),
                          list(arg = "SpliceAI",
                               class = "logical"),
                          list(arg = "SpliceRegion",
                               class = "logical"),
                          list(arg = "appris",
                               class = "logical"),
                          list(arg = "canonical",
                               class = "logical"),
                          list(arg = "ccds",
                               class = "logical"),
                          list(arg = "dbNSFP",
                               class = "character"),
                          list(arg = "dbscSNV",
                               class = "logical"),
                          list(arg = "distance",
                               class = "numeric"),
                          list(arg = "domains",
                               class = "logical"),
                          list(arg = "failed",
                               class = "logical"),
                          list(arg = "hgvs",
                               class = "logical"),
                          list(arg = "mane",
                               class = "logical"),
                          list(arg = "merged",
                               class = "logical"),
                          list(arg = "miRNA",
                               class = "logical"),
                          list(arg = "minimal",
                               class = "logical"),
                          list(arg = "numbers",
                               class = "logical"),
                          list(arg = "protein",
                               class = "logical"),
                          list(arg = "refseq",
                               class = "logical"),
                          list(arg = "shift_3prime",
                               class = "logical"),
                          list(arg = "shift_genomic",
                               class = "logical"),
                          list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "transcript_version",
                               class = "logical"),
                          list(arg = "tsl",
                               class = "logical"),
                          list(arg = "uniprot",
                               class = "logical"),
                          list(arg = "variant_class",
                               class = "logical"),
                          list(arg = "vcf_string",
                               class = "logical"),
                          list(arg = "xref_refseq",
                               class = "logical")))

  v_msg("POST vep/:species/id")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("Blosum62",
                                 Blosum62 == TRUE,
                                 "1"),
                            list("CADD",
                                 CADD == TRUE,
                                 "1"),
                            list("GeneSplicer",
                                 GeneSplicer == TRUE,
                                 "1"),
                            list("LoF",
                                 LoF == TRUE,
                                 "1"),
                            list("MaxEntScan",
                                 MaxEntScan == TRUE,
                                 "1"),
                            list("Phenotypes",
                                 Phenotypes == TRUE,
                                 "1"),
                            list("SpliceAI",
                                 SpliceAI == TRUE,
                                 "1"),
                            list("SpliceRegion",
                                 SpliceRegion == TRUE,
                                 "1"),
                            list("appris",
                                 appris == TRUE,
                                 "1"),
                            list("canonical",
                                 canonical == TRUE,
                                 "1"),
                            list("ccds",
                                 ccds == TRUE,
                                 "1"),
                            list("dbNSFP",
                                 !is.na(dbNSFP),
                                 dbNSFP),
                            list("dbscSNV",
                                 dbscSNV == TRUE,
                                 "1"),
                            list("distance",
                                 distance != 5000,
                                 as.integer(distance)),
                            list("domains",
                                 domains == TRUE,
                                 "1"),
                            list("failed",
                                 failed == TRUE,
                                 "1"),
                            list("hgvs",
                                 hgvs == TRUE,
                                 "1"),
                            list("mane",
                                 mane == TRUE,
                                 "1"),
                            list("merged",
                                 merged == TRUE,
                                 "1"),
                            list("miRNA",
                                 miRNA == TRUE,
                                 "1"),
                            list("minimal",
                                 minimal == TRUE,
                                 "1"),
                            list("numbers",
                                 numbers == TRUE,
                                 "1"),
                            list("protein",
                                 protein == TRUE,
                                 "1"),
                            list("refseq",
                                 refseq == TRUE,
                                 "1"),
                            list("shift_3prime",
                                 shift_3prime == TRUE,
                                 "1"),
                            list("shift_genomic",
                                 shift_genomic == TRUE,
                                 "1"),
                            list("transcript_id",
                                 !is.na(transcript_id),
                                 transcript_id),
                            list("transcript_version",
                                 transcript_version == TRUE,
                                 "1"),
                            list("tsl",
                                 tsl == TRUE,
                                 "1"),
                            list("uniprot",
                                 uniprot == TRUE,
                                 "1"),
                            list("variant_class",
                                 variant_class == TRUE,
                                 "1"),
                            list("vcf_string",
                                 vcf_string == TRUE,
                                 "1"),
                            list("xref_refseq",
                                 xref_refseq == TRUE,
                                 "1"))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("vep/",
                                         species,
                                         "/id"),
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list_no_simp")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Fetch variant consequences for multiple regions
#'
#' @param species
#' @param Blosum62
#' @param CADD
#' @param GeneSplicer
#' @param LoF
#' @param MaxEntScan
#' @param Phenotypes
#' @param SpliceAI
#' @param SpliceRegion
#' @param appris
#' @param canonical
#' @param ccds
#' @param dbNSFP
#' @param dbscSNV
#' @param distance
#' @param domains
#' @param failed
#' @param hgvs
#' @param mane
#' @param merged
#' @param miRNA
#' @param minimal
#' @param numbers
#' @param protein
#' @param refseq
#' @param shift_3prime
#' @param shift_genomic
#' @param transcript_id
#' @param transcript_version
#' @param tsl
#' @param uniprot
#' @param variant_class
#' @param vcf_string
#' @param variants
#' @param ...
#' @param xref_refseq
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_vep_variant = function(variants,
                                   species,
                                   Blosum62 = FALSE,
                                   CADD = FALSE,
                                   GeneSplicer = FALSE,
                                   LoF = FALSE,
                                   MaxEntScan = FALSE,
                                   Phenotypes = FALSE,
                                   SpliceAI = FALSE,
                                   SpliceRegion = FALSE,
                                   appris = FALSE,
                                   canonical = FALSE,
                                   ccds = FALSE,
                                   dbNSFP = NA,
                                   dbscSNV = FALSE,
                                   distance = 5000,
                                   domains = FALSE,
                                   failed = FALSE,
                                   hgvs = FALSE,
                                   mane = FALSE,
                                   merged = FALSE,
                                   miRNA = FALSE,
                                   minimal = FALSE,
                                   numbers = FALSE,
                                   protein = FALSE,
                                   refseq = FALSE,
                                   shift_3prime = FALSE,
                                   shift_genomic = FALSE,
                                   transcript_id = NA,
                                   transcript_version = FALSE,
                                   tsl = FALSE,
                                   uniprot = FALSE,
                                   variant_class = FALSE,
                                   vcf_string = FALSE,
                                   xref_refseq = FALSE,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "variants",
                               class = "character",
                               max_len = 200),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "Blosum62",
                               class = "logical"),
                          list(arg = "CADD",
                               class = "logical"),
                          list(arg = "GeneSplicer",
                               class = "logical"),
                          list(arg = "LoF",
                               class = "logical"),
                          list(arg = "MaxEntScan",
                               class = "logical"),
                          list(arg = "Phenotypes",
                               class = "logical"),
                          list(arg = "SpliceAI",
                               class = "logical"),
                          list(arg = "SpliceRegion",
                               class = "logical"),
                          list(arg = "appris",
                               class = "logical"),
                          list(arg = "canonical",
                               class = "logical"),
                          list(arg = "ccds",
                               class = "logical"),
                          list(arg = "dbNSFP",
                               class = "character"),
                          list(arg = "dbscSNV",
                               class = "logical"),
                          list(arg = "distance",
                               class = "numeric"),
                          list(arg = "domains",
                               class = "logical"),
                          list(arg = "failed",
                               class = "logical"),
                          list(arg = "hgvs",
                               class = "logical"),
                          list(arg = "mane",
                               class = "logical"),
                          list(arg = "merged",
                               class = "logical"),
                          list(arg = "miRNA",
                               class = "logical"),
                          list(arg = "minimal",
                               class = "logical"),
                          list(arg = "numbers",
                               class = "logical"),
                          list(arg = "protein",
                               class = "logical"),
                          list(arg = "refseq",
                               class = "logical"),
                          list(arg = "shift_3prime",
                               class = "logical"),
                          list(arg = "shift_genomic",
                               class = "logical"),
                          list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "transcript_version",
                               class = "logical"),
                          list(arg = "tsl",
                               class = "logical"),
                          list(arg = "uniprot",
                               class = "logical"),
                          list(arg = "variant_class",
                               class = "logical"),
                          list(arg = "vcf_string",
                               class = "logical"),
                          list(arg = "xref_refseq",
                               class = "logical")))

  v_msg("POST vep/:species/region")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("Blosum62",
                                 Blosum62 == TRUE,
                                 "1"),
                            list("CADD",
                                 CADD == TRUE,
                                 "1"),
                            list("GeneSplicer",
                                 GeneSplicer == TRUE,
                                 "1"),
                            list("LoF",
                                 LoF == TRUE,
                                 "1"),
                            list("MaxEntScan",
                                 MaxEntScan == TRUE,
                                 "1"),
                            list("Phenotypes",
                                 Phenotypes == TRUE,
                                 "1"),
                            list("SpliceAI",
                                 SpliceAI == TRUE,
                                 "1"),
                            list("SpliceRegion",
                                 SpliceRegion == TRUE,
                                 "1"),
                            list("appris",
                                 appris == TRUE,
                                 "1"),
                            list("canonical",
                                 canonical == TRUE,
                                 "1"),
                            list("ccds",
                                 ccds == TRUE,
                                 "1"),
                            list("dbNSFP",
                                 !is.na(dbNSFP),
                                 dbNSFP),
                            list("dbscSNV",
                                 dbscSNV == TRUE,
                                 "1"),
                            list("distance",
                                 distance != 5000,
                                 as.integer(distance)),
                            list("domains",
                                 domains == TRUE,
                                 "1"),
                            list("failed",
                                 failed == TRUE,
                                 "1"),
                            list("hgvs",
                                 hgvs == TRUE,
                                 "1"),
                            list("mane",
                                 mane == TRUE,
                                 "1"),
                            list("merged",
                                 merged == TRUE,
                                 "1"),
                            list("miRNA",
                                 miRNA == TRUE,
                                 "1"),
                            list("minimal",
                                 minimal == TRUE,
                                 "1"),
                            list("numbers",
                                 numbers == TRUE,
                                 "1"),
                            list("protein",
                                 protein == TRUE,
                                 "1"),
                            list("refseq",
                                 refseq == TRUE,
                                 "1"),
                            list("shift_3prime",
                                 shift_3prime == TRUE,
                                 "1"),
                            list("shift_genomic",
                                 shift_genomic == TRUE,
                                 "1"),
                            list("transcript_id",
                                 !is.na(transcript_id),
                                 transcript_id),
                            list("transcript_version",
                                 transcript_version == TRUE,
                                 "1"),
                            list("tsl",
                                 tsl == TRUE,
                                 "1"),
                            list("uniprot",
                                 uniprot == TRUE,
                                 "1"),
                            list("variant_class",
                                 variant_class == TRUE,
                                 "1"),
                            list("vcf_string",
                                 vcf_string == TRUE,
                                 "1"),
                            list("xref_refseq",
                                 xref_refseq == TRUE,
                                 "1"))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("variants" = as.array(variants)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("vep/",
                                         species,
                                         "/region"),
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list_no_simp")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Fetch variant consequences
#'
#' @param allele
#' @param region
#' @param species
#' @param Blosum62
#' @param CADD
#' @param GeneSplicer
#' @param LoF
#' @param MaxEntScan
#' @param Phenotypes
#' @param SpliceAI
#' @param SpliceRegion
#' @param appris
#' @param canonical
#' @param ccds
#' @param dbNSFP
#' @param dbscSNV
#' @param distance
#' @param domains
#' @param failed
#' @param hgvs
#' @param mane
#' @param merged
#' @param miRNA
#' @param minimal
#' @param numbers
#' @param protein
#' @param refseq
#' @param shift_3prime
#' @param shift_genomic
#' @param transcript_id
#' @param transcript_version
#' @param tsl
#' @param uniprot
#' @param variant_class
#' @param vcf_string
#' @param ...
#' @param xref_refseq
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_vep_allele = function(allele,
                                  region,
                                  species,
                                  Blosum62 = FALSE,
                                  CADD = FALSE,
                                  GeneSplicer = FALSE,
                                  LoF = FALSE,
                                  MaxEntScan = FALSE,
                                  Phenotypes = FALSE,
                                  SpliceAI = FALSE,
                                  SpliceRegion = FALSE,
                                  appris = FALSE,
                                  canonical = FALSE,
                                  ccds = FALSE,
                                  dbNSFP = NA,
                                  dbscSNV = FALSE,
                                  distance = 5000,
                                  domains = FALSE,
                                  failed = FALSE,
                                  hgvs = FALSE,
                                  mane = FALSE,
                                  merged = FALSE,
                                  miRNA = FALSE,
                                  minimal = FALSE,
                                  numbers = FALSE,
                                  protein = FALSE,
                                  refseq = FALSE,
                                  shift_3prime = FALSE,
                                  shift_genomic = FALSE,
                                  transcript_id = NA,
                                  transcript_version = FALSE,
                                  tsl = FALSE,
                                  uniprot = FALSE,
                                  variant_class = FALSE,
                                  vcf_string = FALSE,
                                  xref_refseq = FALSE,
                                  ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "variants",
                               class = "character"),
                          list(arg = "variants",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "Blosum62",
                               class = "logical"),
                          list(arg = "CADD",
                               class = "logical"),
                          list(arg = "GeneSplicer",
                               class = "logical"),
                          list(arg = "LoF",
                               class = "logical"),
                          list(arg = "MaxEntScan",
                               class = "logical"),
                          list(arg = "Phenotypes",
                               class = "logical"),
                          list(arg = "SpliceAI",
                               class = "logical"),
                          list(arg = "SpliceRegion",
                               class = "logical"),
                          list(arg = "appris",
                               class = "logical"),
                          list(arg = "canonical",
                               class = "logical"),
                          list(arg = "ccds",
                               class = "logical"),
                          list(arg = "dbNSFP",
                               class = "character"),
                          list(arg = "dbscSNV",
                               class = "logical"),
                          list(arg = "distance",
                               class = "numeric"),
                          list(arg = "domains",
                               class = "logical"),
                          list(arg = "failed",
                               class = "logical"),
                          list(arg = "hgvs",
                               class = "logical"),
                          list(arg = "mane",
                               class = "logical"),
                          list(arg = "merged",
                               class = "logical"),
                          list(arg = "miRNA",
                               class = "logical"),
                          list(arg = "minimal",
                               class = "logical"),
                          list(arg = "numbers",
                               class = "logical"),
                          list(arg = "protein",
                               class = "logical"),
                          list(arg = "refseq",
                               class = "logical"),
                          list(arg = "shift_3prime",
                               class = "logical"),
                          list(arg = "shift_genomic",
                               class = "logical"),
                          list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "transcript_version",
                               class = "logical"),
                          list(arg = "tsl",
                               class = "logical"),
                          list(arg = "uniprot",
                               class = "logical"),
                          list(arg = "variant_class",
                               class = "logical"),
                          list(arg = "vcf_string",
                               class = "logical"),
                          list(arg = "xref_refseq",
                               class = "logical")))

  v_msg("GET vep/:species/region/:region/:allele/")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("Blosum62",
                                 Blosum62 == TRUE,
                                 "1"),
                            list("CADD",
                                 CADD == TRUE,
                                 "1"),
                            list("GeneSplicer",
                                 GeneSplicer == TRUE,
                                 "1"),
                            list("LoF",
                                 LoF == TRUE,
                                 "1"),
                            list("MaxEntScan",
                                 MaxEntScan == TRUE,
                                 "1"),
                            list("Phenotypes",
                                 Phenotypes == TRUE,
                                 "1"),
                            list("SpliceAI",
                                 SpliceAI == TRUE,
                                 "1"),
                            list("SpliceRegion",
                                 SpliceRegion == TRUE,
                                 "1"),
                            list("appris",
                                 appris == TRUE,
                                 "1"),
                            list("canonical",
                                 canonical == TRUE,
                                 "1"),
                            list("ccds",
                                 ccds == TRUE,
                                 "1"),
                            list("dbNSFP",
                                 !is.na(dbNSFP),
                                 dbNSFP),
                            list("dbscSNV",
                                 dbscSNV == TRUE,
                                 "1"),
                            list("distance",
                                 distance != 5000,
                                 as.integer(distance)),
                            list("domains",
                                 domains == TRUE,
                                 "1"),
                            list("failed",
                                 failed == TRUE,
                                 "1"),
                            list("hgvs",
                                 hgvs == TRUE,
                                 "1"),
                            list("mane",
                                 mane == TRUE,
                                 "1"),
                            list("merged",
                                 merged == TRUE,
                                 "1"),
                            list("miRNA",
                                 miRNA == TRUE,
                                 "1"),
                            list("minimal",
                                 minimal == TRUE,
                                 "1"),
                            list("numbers",
                                 numbers == TRUE,
                                 "1"),
                            list("protein",
                                 protein == TRUE,
                                 "1"),
                            list("refseq",
                                 refseq == TRUE,
                                 "1"),
                            list("shift_3prime",
                                 shift_3prime == TRUE,
                                 "1"),
                            list("shift_genomic",
                                 shift_genomic == TRUE,
                                 "1"),
                            list("transcript_id",
                                 !is.na(transcript_id),
                                 transcript_id),
                            list("transcript_version",
                                 transcript_version == TRUE,
                                 "1"),
                            list("tsl",
                                 tsl == TRUE,
                                 "1"),
                            list("uniprot",
                                 uniprot == TRUE,
                                 "1"),
                            list("variant_class",
                                 variant_class == TRUE,
                                 "1"),
                            list("vcf_string",
                                 vcf_string == TRUE,
                                 "1"),
                            list("xref_refseq",
                                 xref_refseq == TRUE,
                                 "1"))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("vep/",
                                         species,
                                         "/region/",
                                         region, "/",
                                         allele),
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#### Variation Endpoints ####

#' Translate a list of variant identifiers, HGVS notations or genomic SPDI
#' notations to all possible variant IDs, HGVS and genomic SPDI
#'
#' @param ids
#' @param species
#' @param ...
#' @param fields
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_variant_recoder = function(ids,
                                       species,
                                       fields = NA,
                                       ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 200),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "fields",
                               class = "character",
                               val = c("id",
                                       "hgvsg",
                                       "hgvsc",
                                       "hgvsp",
                                       "spdi",
                                       "vcf_string"))))

  v_msg("POST variant_recoder/:species")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("fields",
                                 any(is.na(fields)),
                                 paste(fields, collapse = ","))
  )

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("variant_recoder/",
                                         species),
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list_no_simp")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Uses a list of variant identifiers (e.g. rsID) to return the variation
#' features including optional genotype, phenotype and population data
#'
#' @param ids
#' @param species
#' @param genotypes
#' @param phenotypes
#' @param pops
#' @param ...
#' @param population_genotypes
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_variation_id  = function(ids,
                                     species,
                                     genotypes = FALSE,
                                     phenotypes = FALSE,
                                     pops = FALSE,
                                     population_genotypes = FALSE,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 200),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "genotypes",
                               class = "logical"),
                          list(arg = "phenotypes",
                               class = "logical"),
                          list(arg = "pops",
                               class = "logical"),
                          list(arg = "population_genotypes",
                               class = "logical")))

  v_msg("POST variation/:species/")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("genotypes",
                                 genotypes == TRUE,
                                 "1"),
                            list("phenotypes",
                                 phenotypes == TRUE,
                                 "1"),
                            list("pops",
                                 pops == TRUE,
                                 "1"),
                            list("population_genotypes",
                                 population_genotypes == TRUE,
                                 "1"))
  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("variation/",
                                         species),
                           body = call_body,
                           query = call_query,
                           httr::accept_json(),
                           httr::content_type("application/json"),
                           parser = "json->list_no_simp")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Fetch variants by publication using PubMed Central reference number (PMCID)
#' or PubMed reference number (PMID)
#'
#' @param pmid
#' @param pmcid
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_variation_pubmed  = function(pmid = NA,
                                         pmcid = NA,
                                         species,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "pmid",
                               class = "character"),
                          list(arg = "pmcid",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric"))),
              cond = list(list(sum(!is.na(pmid), !is.na(pmcid)) == 2,
                               "You can only provide either PMID or PMCID in one function call."),
                          list(sum(!is.na(pmid), !is.na(pmcid)) == 0,
                               "Provide either PMID or PMCID.")))

  v_msg("GET variation/:species/pmcid/:pmcid",
        "GET variation/:species/pmid/:pmid")

  if (!is.na(pmid)) {
    path_input = paste0("variation/", species, "/pmid/", pmid)
  } else if (!is.na(pmcid)) {
    path_input = paste0("variation/", species, "/pmcid/", pmcid)
  }

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->list")

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}
