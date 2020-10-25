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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                                 attribs,
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_ld_variants.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_ld_pairwise.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_ld_region.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
