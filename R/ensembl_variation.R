#### Variation Endpoints ####

#' Translate a list of variant identifiers, HGVS notations or genomic SPDI
#' notations to all possible variant IDs, HGVS and genomic SPDI
#'
#' @param ids
#' @param species
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                                 paste(fields, collapse = ",")))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("variant_recoder/",
                                         species),
                           body = call_body,
                           query = call_query,
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_variant_recoder.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param population_genotypes
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_variation_id = function(ids,
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
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_variation_id.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Fetch variants by publication using PubMed Central reference number (PMCID)
#' or PubMed reference number (PMID)
#'
#' @param pmid
#' @param pmcid
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_variation_pubmed = function(pmid = NA,
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
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_variation_pubmed.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
