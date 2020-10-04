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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_map_cdna.json"))

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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_map_cds.json"))

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
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_map_assembly.json"))

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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_map_translation.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
