#### Proteomes Endpoints ####
#' Search proteomes in UniProt
#'
#' @param accession
#' @param reviewed
#' @param isoform
#' @param go_term
#' @param keyword
#' @param ec
#' @param gene
#' @param exact_gene
#' @param protein
#' @param organism
#' @param taxid
#' @param pubmed
#' @param seq_length
#' @param md5
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteomes_search = function(upid = NA,
                                        name = NA,
                                        taxid = NA,
                                        keyword = NA,
                                        xref = NA,
                                        genome_acc = NA,
                                        is_ref_proteome = NA,
                                        is_redundant = NA,
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "upid",
                               class = "character",
                               max_len = 100),
                          list(arg = "name",
                               class = "character"),
                          list(arg = "taxid",
                               class = "numeric",
                               max_len = 20),
                          list(arg = "keyword",
                               class = "character"),
                          list(arg = "xref",
                               class = "character",
                               max_len = 20),
                          list(arg = "genome_acc",
                               class = "character",
                               max_len = 20),
                          list(arg = "is_ref_proteome",
                               class = "logical"),
                          list(arg = "is_redundant",
                               class = "logical"))
  )

  v_msg(paste("get /proteomes Search proteomes in UniProt"))
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("upid",
                                 any(!is.na(upid)),
                                 paste0(upid,
                                        collapse = ",")),
                            list("name",
                                 !is.na(name),
                                 name),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")),
                            list("keyword",
                                 !is.na(keyword),
                                 keyword),
                            list("xref",
                                 any(!is.na(xref)),
                                 paste0(xref,
                                        collapse = ",")),
                            list("genome_acc",
                                 any(!is.na(genome_acc)),
                                 paste0(genome_acc,
                                        collapse = ",")),
                            list("is_ref_proteome",
                                 !is.na(is_ref_proteome),
                                 ifelse(is_ref_proteome,
                                        "true",
                                        "false")))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "proteomes"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get proteome by proteome/proteins UPID
#'
#' @param upid
#' @param get_proteins
#' @param reviewed
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteomes = function(upid,
                                 get_proteins = FALSE,
                                 reviewed = NA,
                                 verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE) {
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "upid",
                               class = "character"),
                          list(arg = "get_proteins",
                               class = "logical"),
                          list(arg = "reviewed",
                               class = "logical")),
              cond = list(list(quote(get_proteins == FALSE && !is.na(reviewed)),
                               "'reviewed' argument is ignored because you provided 'get_proteins' as FALSE")),
              cond_warning = TRUE
  )

  v_msg(paste("get /proteomes/proteins/{upid} Get proteins by proteome UPID",
              "get /proteomes/{upid} Get proteome by proteome UPID"))
  ## make function-specific calls
  if (get_proteins == TRUE) {
    ## build GET API request's query
    call_query = rba_ba_query(init = list(),
                              list("reviewed",
                                   !is.na(reviewed),
                                   ifelse(reviewed,
                                          "true",
                                          "false")))
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "proteomes/proteins/",
                        upid)
  } else {
    call_query = NULL
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "proteomes/",
                        upid)
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Genecentric Endpoints ####

#' Search gene centric proteins
#'
#' @param upid
#' @param accession
#' @param gene
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_genecentric_search = function(upid = NA,
                                          accession = NA,
                                          gene = NA,
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE) {
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "upid",
                               class = "character",
                               max_len = 100),
                          list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "gene",
                               class = "character",
                               max_len = 20))
  )

  v_msg(paste("get /genecentric Search gene centric proteins"))
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("upid",
                                 any(!is.na(upid)),
                                 paste0(upid,
                                        collapse = ",")),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("gene",
                                 any(!is.na(gene)),
                                 paste0(gene,
                                        collapse = ",")))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "genecentric"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get gene centric proteins by Uniprot accession
#'
#' @param accession
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_genecentric = function(accession,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"))
  )

  v_msg(paste("get /genecentric/{accession} Get gene centric proteins by Uniprot accession"))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "genecentric/",
                                         accession),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
