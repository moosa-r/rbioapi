#' Search UniParc entries
#'
#' @param upi
#' @param accession
#' @param db_type
#' @param db_id
#' @param gene
#' @param protein
#' @param taxid
#' @param organism
#' @param sequence_checksum
#' @param ipr
#' @param signature_type
#' @param signature_id
#' @param upid
#' @param seq_length
#' @param rf_dd_type
#' @param rf_db_id
#' @param rf_active
#' @param rf_tax_id
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param location
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_uniparc_search = function(upi = NA,
                                      accession = NA,
                                      db_type = NA,
                                      db_id = NA,
                                      gene = NA,
                                      protein = NA,
                                      taxid = NA,
                                      organism = NA,
                                      sequence_checksum= NA,
                                      ipr = NA,
                                      signature_type = NA,
                                      signature_id = NA,
                                      upid = NA,
                                      seq_length = NA,
                                      rf_dd_type = NA,
                                      rf_db_id = NA,
                                      rf_active = NA,
                                      rf_tax_id = NA,
                                      location = NA,
                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "upi",
                               class = "character",
                               max_len = 100),
                          list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "db_id",
                               class = "character"),
                          list(arg = "gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "protein",
                               class = "character"),
                          list(arg = "taxid",
                               class = "character",
                               max_len = 20),
                          list(arg = "organism",
                               class = "character"),
                          list(arg = "sequence_checksum",
                               class = "character"),
                          list(arg = "ipr",
                               class = "character",
                               max_len = 100),
                          list(arg = "signature_type",
                               class = "character",
                               max_len = 20),
                          list(arg = "upid",
                               class = "character",
                               max_len = 100),
                          list(arg = "seq_length",
                               class = "character"),
                          list(arg = "rf_dd_type",
                               class = "character"),
                          list(arg = "rf_db_id",
                               class = "character"),
                          list(arg = "rf_active",
                               class = "logical"),
                          list(arg = "rf_tax_id",
                               class = "character"))
  )

  v_msg("get /uniparc Search UniParc entries")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("upi",
                                 any(!is.na(upi)),
                                 paste0(upi,
                                        collapse = ",")),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("dbtype",
                                 !is.na(db_type),
                                 db_type),
                            list("dbid",
                                 any(!is.na(db_id)),
                                 paste0(db_id,
                                        collapse = ",")),
                            list("gene",
                                 any(!is.na(gene)),
                                 paste0(gene,
                                        collapse = ",")),
                            list("protein",
                                 !is.na(protein),
                                 protein),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")),
                            list("organism",
                                 !is.na(organism),
                                 organism),
                            list("sequencechecksum",
                                 !is.na(sequence_checksum),
                                 sequence_checksum),
                            list("ipr",
                                 any(!is.na(ipr)),
                                 paste0(ipr,
                                        collapse = ",")),
                            list("signaturetype",
                                 any(!is.na(signature_type)),
                                 paste0(signature_type,
                                        collapse = ",")),
                            list("signatureid",
                                 any(!is.na(signature_id)),
                                 paste0(signature_id,
                                        collapse = ",")),
                            list("upid",
                                 any(!is.na(upid)),
                                 paste0(upid,
                                        collapse = ",")),
                            list("seqLength",
                                 !is.na(seq_length),
                                 seq_length),
                            list("rfDdtype",
                                 any(!is.na(rf_dd_type)),
                                 paste0(rf_dd_type,
                                        collapse = ",")),
                            list("rfDbid",
                                 any(!is.na(rf_db_id)),
                                 paste0(rf_db_id,
                                        collapse = ",")),
                            list("rfActive",
                                 !is.na(rf_active),
                                 ifelse(rf_active,
                                        "true",
                                        "false")),
                            list("rfTaxId",
                                 any(!is.na(rf_tax_id)),
                                 paste0(rf_tax_id,
                                        collapse = ",")))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "uniparc"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_uniparc_search.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniParc entry only by UniProt accession
#'
#' @param accession
#' @param rf_dd_type
#' @param rf_db_id
#' @param rf_active
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param rf_tax_id
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_uniparc = function(accession = NA,
                               rf_dd_type = NA,
                               rf_db_id = NA,
                               rf_active = NA,
                               rf_tax_id = NA,
                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "rf_dd_type",
                               class = "character"),
                          list(arg = "rf_db_id",
                               class = "character"),
                          list(arg = "rf_active",
                               class = "logical"),
                          list(arg = "rf_tax_id",
                               class = "character"))
  )

  v_msg("get /uniparc/accession/{accession} Get UniParc entry only by UniProt accession")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("rfDdtype",
                                 any(!is.na(rf_dd_type)),
                                 paste0(rf_dd_type,
                                        collapse = ",")),
                            list("rfDbid",
                                 any(!is.na(rf_db_id)),
                                 paste0(rf_db_id,
                                        collapse = ",")),
                            list("rfActive",
                                 !is.na(rf_active),
                                 ifelse(rf_active,
                                        "true",
                                        "false")),
                            list("rfTaxId",
                                 any(!is.na(rf_tax_id)),
                                 paste0(rf_tax_id,
                                        collapse = ",")))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "uniparc/accession/",
                                         accession),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("uniprot_uniparc.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' For a given user input (request parameters), Best Guess returns the
#' UniParcEntry with a cross-reference to the longest active UniProtKB
#' sequence (preferably from Swiss-Prot and if not then TrEMBL). It also
#' returns the sequence and related information. If it finds more than one
#' longest active UniProtKB sequence it returns 400 (Bad Request) error
#' response with the list of cross references found.
#'
#' @param upi
#' @param accession
#' @param db_id
#' @param gene
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param taxid
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_uniparc_bestguess = function(upi = NA,
                                         accession = NA,
                                         db_id = NA,
                                         gene = NA,
                                         taxid = NA,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "upi",
                               class = "character",
                               max_len = 100),
                          list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "db_id",
                               class = "character"),
                          list(arg = "gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "taxid",
                               class = "character",
                               max_len = 20))
  )

  v_msg("get /uniparc/bestguess Get UniParc longest sequence for entries.")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("upi",
                                 any(!is.na(upi)),
                                 paste0(upi,
                                        collapse = ",")),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("dbid",
                                 any(!is.na(db_id)),
                                 paste0(db_id,
                                        collapse = ",")),
                            list("gene",
                                 any(!is.na(gene)),
                                 paste0(gene,
                                        collapse = ",")),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "uniparc/bestguess"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_uniparc_bestguess.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniParc entries by all UniParc cross reference accessions
#'
#' @param db_id
#' @param rf_dd_type
#' @param rf_db_id
#' @param rf_active
#' @param upid
#' @param upi
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param rf_tax_id
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_uniparc_get = function(db_id = NA,
                                   upid = NA,
                                   upi = NA,
                                   rf_dd_type = NA,
                                   rf_db_id = NA,
                                   rf_active = NA,
                                   rf_tax_id = NA,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "db_id",
                               class = "character"),
                          list(arg = "upid",
                               class = "character"),
                          list(arg = "upi",
                               class = "character"),
                          list(arg = "rf_dd_type",
                               class = "character"),
                          list(arg = "rf_db_id",
                               class = "character"),
                          list(arg = "rf_active",
                               class = "logical"),
                          list(arg = "rf_tax_id",
                               class = "character")),
              cond = list(list(quote(sum(!is.na(db_id), !is.na(upid), !is.na(upi)) != 1),
                               "Please provide one of the arguments 'db_id', 'upid' or 'upi'."))
  )

  v_msg("get /uniparc/dbreference/{dbid} Get UniParc entries by all UniParc cross reference accessions",
        "get /uniparc/proteome/{upid} Get UniParc entries by Proteome UPID",
        "get /uniparc/upi/{upi} Get UniParc entry by UniParc UPI")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("rfDdtype",
                                 any(!is.na(rf_dd_type)),
                                 paste0(rf_dd_type,
                                        collapse = ",")),
                            list("rfDbid",
                                 any(!is.na(rf_db_id)),
                                 paste0(rf_db_id,
                                        collapse = ",")),
                            list("rfActive",
                                 !is.na(rf_active),
                                 ifelse(rf_active,
                                        "true",
                                        "false")),
                            list("rfTaxId",
                                 any(!is.na(rf_tax_id)),
                                 paste0(rf_tax_id,
                                        collapse = ",")))
  ## Build Function-Specific Call
  if (!is.na(db_id)) {
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "uniparc/dbreference/",
                        db_id)
  } else if (!is.na(upid)) {
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "uniparc/proteome/",
                        upid)
  } else if (!is.na(upi)) {
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "uniparc/upi/",
                        upi)
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_uniparc_get.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniParc entries by sequence
#'
#' @param sequence
#' @param rf_dd_type
#' @param rf_db_id
#' @param rf_active
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param rf_tax_id
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_uniparc_sequence = function(sequence,
                                        rf_dd_type = NA,
                                        rf_db_id = NA,
                                        rf_active = NA,
                                        rf_tax_id = NA,
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "rf_dd_type",
                               class = "character"),
                          list(arg = "rf_db_id",
                               class = "character"),
                          list(arg = "rf_active",
                               class = "logical"),
                          list(arg = "rf_tax_id",
                               class = "character")))

  v_msg("post /uniparc/sequence Get UniParc entries by sequence")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("rfDdtype",
                                 any(!is.na(rf_dd_type)),
                                 paste0(rf_dd_type,
                                        collapse = ",")),
                            list("rfDbid",
                                 any(!is.na(rf_db_id)),
                                 paste0(rf_db_id,
                                        collapse = ",")),
                            list("rfActive",
                                 !is.na(rf_active),
                                 ifelse(rf_active,
                                        "true",
                                        "false")),
                            list("rfTaxId",
                                 any(!is.na(rf_tax_id)),
                                 paste0(rf_tax_id,
                                        collapse = ",")))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "uniparc/sequence"),
                           query = call_query,
                           body = sequence,
                           accept = "application/json",
                           httr::content_type("text/plain"),
                           parser = "json->list_simp",
                           save_to = rba_ba_file("uniprot_uniparc_sequence.json"))
  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
