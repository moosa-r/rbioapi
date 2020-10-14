#### Phenotype annotations Endpoints ####

#' Return phenotype annotations for genomic features given a phenotype
#' ontology accession
#'
#' @param accession
#' @param species
#' @param include_children
#' @param include_pubmed_id
#' @param include_review_status
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_phenotype_accession.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_phenotype_gene.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_phenotype_region.json"))

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
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_phenotype_term.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
