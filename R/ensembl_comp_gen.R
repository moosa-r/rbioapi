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
rba_ensembl_cafe_genetree = function(genetree_id,
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
                           path = paste0("cafe/genetree/id/", genetree_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_cafe_genetree.json"))

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
rba_ensembl_cafe_genetree_member_id = function(ensembl_id,
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
                           path = paste0("cafe/genetree/member/id/", ensembl_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_cafe_genetree_member_id.json"))

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
rba_ensembl_cafe_genetree_member_symbol = function(gene_symbol,
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
                           path = sprintf("cafe/genetree/member/symbol/%s/%s",
                                          species, gene_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_cafe_genetree_member_symbol.json"))

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
rba_ensembl_family = function(familiy_id,
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
                           path = paste0("family/id/", familiy_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_family_id.json"))

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
rba_ensembl_familiy_member_id = function(ensembl_id,
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
                           path = paste0("family/member/id/", ensembl_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_familiy_member_id.json"))

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
rba_ensembl_familiy_member_symbol = function(gene_symbol,
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
                           path = sprintf("family/member/symbol/%s/%s",
                                          species, gene_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_familiy_member_symbol.json"))

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
rba_ensembl_genetree = function(genetree_id,
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
                           path = paste0("genetree/id/", genetree_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_genetree_id.json"))

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
rba_ensembl_genetree_member_symbol = function(gene_symbol,
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
                           path = sprintf("genetree/member/symbol/%s/%s",
                                          species, gene_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_genetree_member_symbol.json"))

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
rba_ensembl_alignment_region = function(region,
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
                           path = sprintf("alignment/region/%s/%s",
                                          species, region),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_alignment_region.json"))

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
rba_ensembl_homology_id = function(ensemble_id,
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
                           path = paste0("homology/id/", ensemble_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_homology_id.json"))

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
rba_ensembl_homology_symbol = function(gene_symbol,
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
                           path = sprintf("homology/symbol/%s/%s",
                                          species, gene_symbol),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_homology_symbol.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}
