#### Regulation Endpoints ####

#' Returns information about a specific microarray
#'
#' @param microarray
#' @param vendor
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_microarray_vendor.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns information about all microarrays available for the given species
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_microarray_species.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns information about a specific probe from a microarray
#'
#' @param microarray
#' @param probe
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_microarray_probe.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns information about a specific probe_set from a microarray
#'
#' @param microarray
#' @param probe_set
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_microarray_probe_set.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns information about all epigenomes available for the given species
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_microarray_epigenome.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Return the specified binding matrix
#'
#' @param binding_matrix_id
#' @param species
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_microarray_binding_matrix.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns a RegulatoryFeature given its stable ID
#'
#' @param regulatory_feature_id
#' @param species
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param activity
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_microarray_regulatory_feature = function(regulatory_feature_id,
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_microarray_regulatory_feature.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
