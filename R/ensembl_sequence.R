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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                                 mask_feature,
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
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_sequence_id.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                                 mask_feature,
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
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_sequence_region.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
