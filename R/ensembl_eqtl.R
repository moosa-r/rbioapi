#### EQTL Endpoints ####

#' Returns all tissues currently available in the DB
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_eqtl_tissue.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_eqtl_gene.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_eqtl_variant.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
