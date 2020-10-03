#### Proteomes Endpoints ####
#' Search proteomes in UniProt
#'
#' @param keyword
#' @param upid
#' @param name
#' @param xref
#' @param genome_acc
#' @param is_ref_proteome
#' @param is_redundant
#' @param ...
#' @param taxid
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
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
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

  v_msg("get /proteomes Search proteomes in UniProt")
  ## Build GET API Request's query
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
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "proteomes"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("uniprot_proteomes_search.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get proteome by proteome/proteins UPID
#'
#' @param upid
#' @param get_proteins
#' @param ...
#' @param reviewed
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteomes = function(upid,
                                 get_proteins = FALSE,
                                 reviewed = NA,
                                 ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
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

  v_msg("get /proteomes/proteins/{upid} Get proteins by proteome UPID",
        "get /proteomes/{upid} Get proteome by proteome UPID")
  ## Build Function-Specific Call
  if (get_proteins == TRUE) {
    ## Build GET API Request's query
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
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_proteomes.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Genecentric Endpoints ####

#' Search gene centric proteins
#'
#' @param upid
#' @param accession
#' @param ...
#' @param gene
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_genecentric_search = function(upid = NA,
                                          accession = NA,
                                          gene = NA,
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
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

  v_msg("get /genecentric Search gene centric proteins")
  ## Build GET API Request's query
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
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "genecentric"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp",
                           save_to = rba_ba_file("uniprot_genecentric_search.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get gene centric proteins by Uniprot accession
#'
#' @param ...
#' @param accession
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_genecentric = function(accession,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"))
  )

  v_msg("get /genecentric/{accession} Get gene centric proteins by Uniprot accession")
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "genecentric/",
                                         accession),
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_genecentric.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
