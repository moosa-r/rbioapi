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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_overlap_id.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                                 trim_downstream,
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_overlap_region.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_overlap_translation.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
