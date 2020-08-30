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
#' @param location
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = upi,
                                         name = "upi",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = db_id,
                                         name = "db_id",
                                         class = "character"),
                                    list(arg = gene,
                                         name = "gene",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = protein,
                                         name = "protein",
                                         class = "character"),
                                    list(arg = taxid,
                                         name = "taxid",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = organism,
                                         name = "organism",
                                         class = "character"),
                                    list(arg = sequence_checksum,
                                         name = "sequence_checksum",
                                         class = "character"),
                                    list(arg = ipr,
                                         name = "ipr",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = signature_type,
                                         name = "signature_type",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = upid,
                                         name = "upid",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = seq_length,
                                         name = "seq_length",
                                         class = "character"),
                                    list(arg = rf_dd_type,
                                         name = "rf_dd_type",
                                         class = "character"),
                                    list(arg = rf_db_id,
                                         name = "rf_db_id",
                                         class = "character"),
                                    list(arg = rf_active,
                                         name = "rf_active",
                                         class = "logical"),
                                    list(arg = rf_tax_id,
                                         name = "rf_tax_id",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /uniparc Search UniParc entries")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(upi)),
                              list("upi" = paste0(upi,
                                                  collapse = ","))),
                         list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(!is.na(db_type),
                              list("dbtype" = db_type)),
                         list(any(!is.na(db_id)),
                              list("dbid" = paste0(db_id,
                                                   collapse = ","))),
                         list(any(!is.na(gene)),
                              list("gene" = paste0(gene,
                                                   collapse = ","))),
                         list(!is.na(protein),
                              list("protein" = protein)),
                         list(any(!is.na(taxid)),
                              list("taxid" = paste0(taxid,
                                                    collapse = ","))),
                         list(!is.na(organism),
                              list("organism" = organism)),
                         list(!is.na(sequence_checksum),
                              list("sequencechecksum" = sequence_checksum)),
                         list(any(!is.na(ipr)),
                              list("ipr" = paste0(ipr,
                                                  collapse = ","))),
                         list(any(!is.na(signature_type)),
                              list("signaturetype" = paste0(signature_type,
                                                            collapse = ","))),
                         list(any(!is.na(signature_id)),
                              list("signatureid" = paste0(signature_id,
                                                          collapse = ","))),
                         list(any(!is.na(upid)),
                              list("upid" = paste0(upid,
                                                   collapse = ","))),
                         list(!is.na(seq_length),
                              list("seqLength" = seq_length)),
                         list(any(!is.na(rf_dd_type)),
                              list("rfDdtype" = paste0(rf_dd_type,
                                                       collapse = ","))),
                         list(any(!is.na(rf_db_id)),
                              list("rfDbid" = paste0(rf_db_id,
                                                     collapse = ","))),
                         list(!is.na(rf_active),
                              list("rfActive" = ifelse(rf_active,
                                                       "true",
                                                       "false"))),
                         list(any(!is.na(rf_tax_id)),
                              list("rfTaxId" = paste0(rf_tax_id,
                                                      collapse = ",")))

  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "uniparc"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get UniParc entry only by UniProt accession
#'
#' @param accession
#' @param rf_dd_type
#' @param rf_db_id
#' @param rf_active
#' @param rf_tax_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                               verbose = TRUE,
                               progress_bar = FALSE,
                               diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = rf_dd_type,
                                         name = "rf_dd_type",
                                         class = "character"),
                                    list(arg = rf_db_id,
                                         name = "rf_db_id",
                                         class = "character"),
                                    list(arg = rf_active,
                                         name = "rf_active",
                                         class = "logical"),
                                    list(arg = rf_tax_id,
                                         name = "rf_tax_id",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /uniparc/accession/{accession} Get UniParc entry only by UniProt accession")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(rf_dd_type)),
                              list("rfDdtype" = paste0(rf_dd_type,
                                                       collapse = ","))),
                         list(any(!is.na(rf_db_id)),
                              list("rfDbid" = paste0(rf_db_id,
                                                     collapse = ","))),
                         list(!is.na(rf_active),
                              list("rfActive" = ifelse(rf_active,
                                                       "true",
                                                       "false"))),
                         list(any(!is.na(rf_tax_id)),
                              list("rfTaxId" = paste0(rf_tax_id,
                                                      collapse = ",")))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "uniparc/accession/",
                                                  accession),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

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
#' @param taxid
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = upi,
                                         name = "upi",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = db_id,
                                         name = "db_id",
                                         class = "character"),
                                    list(arg = gene,
                                         name = "gene",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = taxid,
                                         name = "taxid",
                                         class = "character",
                                         max_len = 20)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /uniparc/bestguess Get UniParc longest sequence for entries.")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(upi)),
                              list("upi" = paste0(upi,
                                                  collapse = ","))),
                         list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(any(!is.na(db_id)),
                              list("dbid" = paste0(db_id,
                                                   collapse = ","))),
                         list(any(!is.na(gene)),
                              list("gene" = paste0(gene,
                                                   collapse = ","))),
                         list(any(!is.na(taxid)),
                              list("taxid" = paste0(taxid,
                                                    collapse = ",")))

  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "uniparc/bestguess"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get UniParc entries by all UniParc cross reference accessions
#'
#' @param db_id
#' @param rf_dd_type
#' @param rf_db_id
#' @param rf_active
#' @param rf_tax_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                           verbose = TRUE,
                                           progress_bar = FALSE,
                                           diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = db_id,
                                         name = "db_id",
                                         class = "character"),
                                    list(arg = upid,
                                         name = "upid",
                                         class = "character"),
                                    list(arg = upi,
                                         name = "upi",
                                         class = "character"),
                                    list(arg = rf_dd_type,
                                         name = "rf_dd_type",
                                         class = "character"),
                                    list(arg = rf_db_id,
                                         name = "rf_db_id",
                                         class = "character"),
                                    list(arg = rf_active,
                                         name = "rf_active",
                                         class = "logical"),
                                    list(arg = rf_tax_id,
                                         name = "rf_tax_id",
                                         class = "character")),
                        cond = list(list(sum(!is.na(db_id), !is.na(upid), !is.na(upi)) != 1,
                                         "Please provide one of the arguments 'db_id', 'upid' or 'upi'.")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /uniparc/dbreference/{dbid} Get UniParc entries by all UniParc cross reference accessions",
            "get /uniparc/proteome/{upid} Get UniParc entries by Proteome UPID",
            "get /uniparc/upi/{upi} Get UniParc entry by UniParc UPI")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(rf_dd_type)),
                              list("rfDdtype" = paste0(rf_dd_type,
                                                       collapse = ","))),
                         list(any(!is.na(rf_db_id)),
                              list("rfDbid" = paste0(rf_db_id,
                                                     collapse = ","))),
                         list(!is.na(rf_active),
                              list("rfActive" = ifelse(rf_active,
                                                       "true",
                                                       "false"))),
                         list(any(!is.na(rf_tax_id)),
                              list("rfTaxId" = paste0(rf_tax_id,
                                                      collapse = ",")))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  if (!is.na(db_id)) {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "uniparc/dbreference/",
                        db_id)
  } else if (!is.na(upid)) {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "uniparc/proteome/",
                        upid)
  } else if (!is.na(upi)) {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "uniparc/upi/",
                        upi)
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = path_input,
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get UniParc entries by sequence
#'
#' @param sequence
#' @param rf_dd_type
#' @param rf_db_id
#' @param rf_active
#' @param rf_tax_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = rf_dd_type,
                                         name = "rf_dd_type",
                                         class = "character"),
                                    list(arg = rf_db_id,
                                         name = "rf_db_id",
                                         class = "character"),
                                    list(arg = rf_active,
                                         name = "rf_active",
                                         class = "logical"),
                                    list(arg = rf_tax_id,
                                         name = "rf_tax_id",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("post /uniparc/sequence Get UniParc entries by sequence")
  }
  ## build GET API request's query
  additional_pars = list(list(any(!is.na(rf_dd_type)),
                              list("rfDdtype" = paste0(rf_dd_type,
                                                       collapse = ","))),
                         list(any(!is.na(rf_db_id)),
                              list("rfDbid" = paste0(rf_db_id,
                                                     collapse = ","))),
                         list(!is.na(rf_active),
                              list("rfActive" = ifelse(rf_active,
                                                       "true",
                                                       "false"))),
                         list(any(!is.na(rf_tax_id)),
                              list("rfTaxId" = paste0(rf_tax_id,
                                                      collapse = ",")))
  )

  call_query = rba_ba_body_add_pars(call_body = list(),
                                    additional_pars = additional_pars)


  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_uniprot"),
                                     path = paste0(getOption("rba_pth_uniprot"),
                                                   "uniparc/sequence"),
                                     query = call_query,
                                     body = sequence,
                                     httr::accept_json(),
                                     httr::content_type("text/plain")

  ))
  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}
